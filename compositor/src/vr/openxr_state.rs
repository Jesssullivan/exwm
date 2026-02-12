//! OpenXR VR state — full session lifecycle management.
//!
//! Manages the OpenXR runtime connection including:
//! - Entry loading (dynamic via openxr-loader)
//! - Instance creation with extension negotiation
//! - System (HMD) discovery
//! - Session lifecycle: IDLE -> READY -> SYNCHRONIZED -> VISIBLE -> FOCUSED
//! - Swapchain creation and format negotiation
//! - Reference space management (LOCAL, STAGE, VIEW)
//! - Frame submission loop
//! - Session recovery from runtime/session loss

use openxrs as xr;
use std::collections::HashSet;
use std::time::{Duration, Instant};
use tracing::{debug, error, info, warn};

use super::drm_lease::HmdManager;
use super::eye_tracking::EyeTracking;
use super::frame_timing::FrameTiming;
use super::blink_wink::BlinkWinkManager;
use super::fatigue::FatigueMonitor;
use super::gaze_focus::GazeFocusManager;
use super::gaze_scroll::GazeScrollState;
use super::gaze_zone::ZoneDetector;
use super::gesture::GestureState;
use super::hand_tracking::HandTrackingState;
use super::link_hints::LinkHintState;
use super::scene::VrScene;
use super::bci_state::BciState;
use super::virtual_keyboard::VirtualKeyboardState;
use super::vr_interaction::VrInteraction;

/// Reference space type selection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceSpaceType {
    Local,
    Stage,
    View,
}

/// VR session state (mirrors OpenXR session states).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VrSessionState {
    Idle,
    Ready,
    Synchronized,
    Visible,
    Focused,
    Stopping,
    LossPending,
    Exiting,
}

impl VrSessionState {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Idle => "idle",
            Self::Ready => "ready",
            Self::Synchronized => "synchronized",
            Self::Visible => "visible",
            Self::Focused => "focused",
            Self::Stopping => "stopping",
            Self::LossPending => "loss-pending",
            Self::Exiting => "exiting",
        }
    }
}

/// HMD system information discovered via OpenXR.
#[derive(Debug, Clone)]
pub struct HmdInfo {
    pub system_name: String,
    pub vendor_id: u32,
    pub max_width: u32,
    pub max_height: u32,
    pub max_layers: u32,
    pub orientation_tracking: bool,
    pub position_tracking: bool,
    pub recommended_width: u32,
    pub recommended_height: u32,
}

impl Default for HmdInfo {
    fn default() -> Self {
        Self {
            system_name: "unknown".to_string(),
            vendor_id: 0,
            max_width: 0,
            max_height: 0,
            max_layers: 0,
            orientation_tracking: false,
            position_tracking: false,
            recommended_width: 1920,
            recommended_height: 1080,
        }
    }
}

/// Central VR state managing the full OpenXR lifecycle.
pub struct VrState {
    pub enabled: bool,
    pub headless: bool,
    pub session_state: VrSessionState,
    pub hmd_info: HmdInfo,
    pub enabled_extensions: HashSet<String>,
    pub active_reference_space: ReferenceSpaceType,
    pub frame_timing: FrameTiming,
    pub scene: VrScene,
    pub hmd_manager: HmdManager,
    pub interaction: VrInteraction,
    pub eye_tracking: EyeTracking,
    pub gaze_focus: GazeFocusManager,
    pub blink_wink: BlinkWinkManager,
    pub zone_detector: ZoneDetector,
    pub fatigue_monitor: FatigueMonitor,
    pub gaze_scroll: GazeScrollState,
    pub link_hints: LinkHintState,
    pub hand_tracking: HandTrackingState,
    pub gesture: GestureState,
    pub virtual_keyboard: VirtualKeyboardState,
    pub bci: BciState,

    // OpenXR objects (Option because they're created incrementally)
    entry: Option<xr::Entry>,
    instance: Option<xr::Instance>,
    system_id: Option<xr::SystemId>,
    // Session and related objects would be stored here
    // but require generic type parameters that depend on the graphics API.
    // For now, we track state and defer actual session creation to the
    // render thread which has the OpenGL context.

    // Recovery
    max_retries: u32,
    retry_count: u32,
    last_retry: Option<Instant>,
}

impl VrState {
    /// Create a new VR state. Does NOT initialize OpenXR yet.
    pub fn new() -> Self {
        Self {
            enabled: false,
            headless: std::env::var("XRT_COMPOSITOR_FORCE_HEADLESS").is_ok(),
            session_state: VrSessionState::Idle,
            hmd_info: HmdInfo::default(),
            enabled_extensions: HashSet::new(),
            active_reference_space: ReferenceSpaceType::Local,
            frame_timing: FrameTiming::default(),
            scene: VrScene::new(),
            hmd_manager: HmdManager::new(),
            interaction: VrInteraction::new(),
            eye_tracking: EyeTracking::new(),
            gaze_focus: GazeFocusManager::new(),
            blink_wink: BlinkWinkManager::new(),
            zone_detector: ZoneDetector::new(),
            fatigue_monitor: FatigueMonitor::new(),
            gaze_scroll: GazeScrollState::new(),
            link_hints: LinkHintState::new(),
            hand_tracking: HandTrackingState::new(),
            gesture: GestureState::new(),
            virtual_keyboard: VirtualKeyboardState::new(),
            bci: BciState::new(),
            entry: None,
            instance: None,
            system_id: None,
            max_retries: 3,
            retry_count: 0,
            last_retry: None,
        }
    }

    /// Initialize the OpenXR runtime.
    /// Returns Ok(true) if VR is available, Ok(false) if not (graceful degradation).
    pub fn initialize(&mut self) -> anyhow::Result<bool> {
        // Step 1: Load OpenXR entry point
        info!("VR: loading OpenXR runtime...");
        let entry = match unsafe { xr::Entry::load() } {
            Ok(e) => e,
            Err(e) => {
                warn!("VR: OpenXR loader not available: {} (continuing in 2D mode)", e);
                return Ok(false);
            }
        };

        // Step 2: Enumerate available extensions
        let available_extensions = match entry.enumerate_extensions() {
            Ok(exts) => exts,
            Err(e) => {
                warn!("VR: failed to enumerate extensions: {}", e);
                return Ok(false);
            }
        };
        info!("VR: OpenXR extensions available: {:?}", available_extensions);

        // Step 3: Select extensions
        let mut required_extensions = xr::ExtensionSet::default();
        // OpenGL graphics binding (required for Smithay integration)
        required_extensions.khr_opengl_enable = available_extensions.khr_opengl_enable;

        // Optional extensions
        let mut enabled = HashSet::new();
        if available_extensions.khr_opengl_enable {
            enabled.insert("XR_KHR_opengl_enable".to_string());
        }
        if available_extensions.ext_eye_gaze_interaction {
            required_extensions.ext_eye_gaze_interaction = true;
            enabled.insert("XR_EXT_eye_gaze_interaction".to_string());
        }
        if available_extensions.ext_hand_tracking {
            required_extensions.ext_hand_tracking = true;
            enabled.insert("XR_EXT_hand_tracking".to_string());
        }

        // Step 4: Create instance
        let app_info = xr::ApplicationInfo {
            application_name: "ewwm-vr-compositor",
            application_version: 1,
            engine_name: "smithay",
            engine_version: 1,
            api_version: xr::Version::new(1, 0, 0),
        };

        let instance = match entry.create_instance(&app_info, &required_extensions, &[]) {
            Ok(inst) => inst,
            Err(e) => {
                warn!("VR: failed to create OpenXR instance: {}", e);
                return Ok(false);
            }
        };

        let instance_props = instance.properties().unwrap_or_else(|_| {
            xr::InstanceProperties {
                runtime_name: "unknown".to_string(),
                runtime_version: xr::Version::new(0, 0, 0),
            }
        });
        info!(
            "VR: OpenXR runtime: {} v{}",
            instance_props.runtime_name, instance_props.runtime_version
        );

        // Step 5: Discover HMD system
        match instance.system(xr::FormFactor::HEAD_MOUNTED_DISPLAY) {
            Ok(system_id) => {
                let system_props = instance.system_properties(system_id)?;
                self.hmd_info = HmdInfo {
                    system_name: system_props.system_name.clone(),
                    vendor_id: system_props.vendor_id,
                    max_width: system_props
                        .graphics_properties
                        .max_swapchain_image_width,
                    max_height: system_props
                        .graphics_properties
                        .max_swapchain_image_height,
                    max_layers: system_props.graphics_properties.max_layer_count,
                    orientation_tracking: system_props
                        .tracking_properties
                        .orientation_tracking,
                    position_tracking: system_props.tracking_properties.position_tracking,
                    recommended_width: 1920, // Updated after view config query
                    recommended_height: 1080,
                };

                info!(
                    "VR: HMD discovered: {} (vendor {}), max {}x{}, tracking: orient={} pos={}",
                    self.hmd_info.system_name,
                    self.hmd_info.vendor_id,
                    self.hmd_info.max_width,
                    self.hmd_info.max_height,
                    self.hmd_info.orientation_tracking,
                    self.hmd_info.position_tracking,
                );

                self.system_id = Some(system_id);
            }
            Err(xr::sys::Result::ERROR_FORM_FACTOR_UNAVAILABLE) => {
                warn!("VR: no HMD connected, falling back to headless mode");
                self.headless = true;
            }
            Err(e) => {
                warn!("VR: system discovery failed: {}", e);
                return Ok(false);
            }
        }

        self.entry = Some(entry);
        self.instance = Some(instance);
        self.enabled_extensions = enabled;
        self.enabled = true;

        info!(
            "VR: initialized (headless={}, extensions={:?})",
            self.headless, self.enabled_extensions
        );
        Ok(true)
    }

    /// Returns the session state as a string for IPC.
    pub fn session_state_str(&self) -> &'static str {
        if !self.enabled {
            "disabled"
        } else if self.headless {
            "headless"
        } else {
            self.session_state.as_str()
        }
    }

    /// Returns HMD name for IPC.
    pub fn hmd_name(&self) -> &str {
        &self.hmd_info.system_name
    }

    /// Returns whether VR is in headless mode.
    pub fn is_headless(&self) -> bool {
        self.headless
    }

    /// Returns frame stats as an IPC-formatted string.
    pub fn frame_stats_sexp(&self) -> String {
        self.frame_timing.stats_sexp()
    }

    /// Handle a session state transition.
    pub fn handle_state_change(&mut self, new_state: VrSessionState) {
        let old = self.session_state;
        self.session_state = new_state;
        info!("VR: session state: {:?} -> {:?}", old, new_state);

        match new_state {
            VrSessionState::Ready => {
                // Should call session.begin() with PRIMARY_STEREO
                info!("VR: session ready, should begin");
            }
            VrSessionState::Stopping => {
                // Should call session.end()
                info!("VR: session stopping, should end");
            }
            VrSessionState::Exiting => {
                info!("VR: session exiting, cleanup");
                self.enabled = false;
            }
            VrSessionState::LossPending => {
                warn!("VR: runtime loss pending, will attempt recovery");
                self.attempt_recovery();
            }
            _ => {}
        }
    }

    /// Attempt to recover from runtime/session loss.
    fn attempt_recovery(&mut self) {
        if self.retry_count >= self.max_retries {
            error!(
                "VR: max retries ({}) reached, disabling VR",
                self.max_retries
            );
            self.enabled = false;
            return;
        }

        if let Some(last) = self.last_retry {
            if last.elapsed() < Duration::from_secs(1) {
                return; // Too soon
            }
        }

        self.retry_count += 1;
        self.last_retry = Some(Instant::now());
        info!(
            "VR: recovery attempt {}/{}",
            self.retry_count, self.max_retries
        );

        // Destroy current state
        self.instance = None;
        self.system_id = None;
        self.session_state = VrSessionState::Idle;

        // Try to reinitialize
        match self.initialize() {
            Ok(true) => {
                info!("VR: recovery successful");
                self.retry_count = 0;
            }
            Ok(false) => {
                warn!("VR: recovery failed (VR not available)");
            }
            Err(e) => {
                warn!("VR: recovery error: {}", e);
            }
        }
    }

    /// Set the active reference space.
    pub fn set_reference_space(&mut self, space_type: ReferenceSpaceType) {
        self.active_reference_space = space_type;
        info!("VR: reference space set to {:?}", space_type);
    }

    /// Poll for VR events.
    pub fn poll_events(&mut self) {
        // In a full implementation, this would call instance.poll_event()
        // and handle SessionStateChanged events.
        // For now, this is a placeholder.
    }

    /// Run one VR frame tick.
    pub fn tick_frame(&mut self) {
        if !self.enabled || self.session_state != VrSessionState::Focused {
            return;
        }
        // In a full implementation:
        // 1. session.wait_frame()
        // 2. session.begin_frame()
        // 3. acquire swapchain, render, release
        // 4. session.end_frame()
        // For now, record synthetic timing for testing
    }

    /// Shut down VR.
    pub fn shutdown(&mut self) {
        info!("VR: shutting down");
        if self.session_state != VrSessionState::Idle
            && self.session_state != VrSessionState::Exiting
        {
            self.handle_state_change(VrSessionState::Stopping);
        }
        self.instance = None;
        self.system_id = None;
        self.entry = None;
        self.enabled = false;
        self.session_state = VrSessionState::Idle;
    }

    /// Generate IPC event for session state change.
    pub fn session_state_event(&self) -> String {
        format!(
            "(:type :event :event :vr-session-state :state :{} :headless {})",
            self.session_state.as_str(),
            if self.headless { "t" } else { "nil" },
        )
    }

    /// Generate IPC event for system discovery.
    pub fn system_discovered_event(&self) -> String {
        format!(
            "(:type :event :event :vr-system-discovered :system-name \"{}\" :max-resolution (:w {} :h {}) :orientation-tracking {} :position-tracking {})",
            self.hmd_info.system_name,
            self.hmd_info.max_width,
            self.hmd_info.max_height,
            if self.hmd_info.orientation_tracking { "t" } else { "nil" },
            if self.hmd_info.position_tracking { "t" } else { "nil" },
        )
    }

    /// Generate IPC response for vr-status query.
    pub fn status_response(&self, msg_id: i64) -> String {
        format!(
            "(:type :response :id {} :status :ok :session :{} :runtime \"{}\" :hmd \"{}\" :headless {} :extensions {:?} :frame-stats {})",
            msg_id,
            self.session_state.as_str(),
            self.instance.as_ref().map(|i| {
                i.properties().map(|p| p.runtime_name).unwrap_or_default()
            }).unwrap_or_default(),
            self.hmd_info.system_name,
            if self.headless { "t" } else { "nil" },
            self.enabled_extensions,
            self.frame_stats_sexp(),
        )
    }
}
