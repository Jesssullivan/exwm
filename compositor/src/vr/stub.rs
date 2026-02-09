//! VR stub — used when the `vr` feature is not enabled.
//!
//! Provides a no-op VrState so the rest of the compositor compiles
//! without OpenXR dependencies (e.g., s390x headless builds).

use tracing::info;

use super::blink_wink::BlinkWinkManager;
use super::drm_lease::HmdManager;
use super::eye_tracking::EyeTracking;
use super::fatigue::FatigueMonitor;
use super::gaze_focus::GazeFocusManager;
use super::gaze_zone::ZoneDetector;
use super::scene::VrScene;
use super::vr_interaction::VrInteraction;

/// Reference space type selection (stub).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReferenceSpaceType {
    Local,
    Stage,
    View,
}

/// Stub VR state when OpenXR is not compiled in.
pub struct VrState {
    pub enabled: bool,
    pub scene: VrScene,
    pub hmd_manager: HmdManager,
    pub interaction: VrInteraction,
    pub eye_tracking: EyeTracking,
    pub gaze_focus: GazeFocusManager,
    pub blink_wink: BlinkWinkManager,
    pub zone_detector: ZoneDetector,
    pub fatigue_monitor: FatigueMonitor,
}

impl Default for VrState {
    fn default() -> Self {
        Self {
            enabled: false,
            scene: VrScene::new(),
            hmd_manager: HmdManager::new(),
            interaction: VrInteraction::new(),
            eye_tracking: EyeTracking::new(),
            gaze_focus: GazeFocusManager::new(),
            blink_wink: BlinkWinkManager::new(),
            zone_detector: ZoneDetector::new(),
            fatigue_monitor: FatigueMonitor::new(),
        }
    }
}

impl VrState {
    pub fn new() -> Self {
        info!("VR subsystem disabled (compiled without 'vr' feature)");
        Self::default()
    }

    /// Returns the session state as a string for IPC.
    pub fn session_state_str(&self) -> &'static str {
        "disabled"
    }

    /// Returns HMD name for IPC.
    pub fn hmd_name(&self) -> &'static str {
        "none"
    }

    /// Returns whether VR is in headless mode.
    pub fn is_headless(&self) -> bool {
        false
    }

    /// Returns frame stats as an IPC-formatted string.
    pub fn frame_stats_sexp(&self) -> String {
        "(:fps 0 :missed 0 :frame-time-ms 0.0)".to_string()
    }

    /// Poll for VR events. No-op when VR is disabled.
    pub fn poll_events(&mut self) {}

    /// Run one VR frame. No-op when VR is disabled.
    pub fn tick_frame(&mut self) {}

    /// Set the active reference space. No-op when VR is disabled.
    pub fn set_reference_space(&mut self, _space_type: ReferenceSpaceType) {}

    /// Shut down VR. No-op when VR is disabled.
    pub fn shutdown(&mut self) {}
}
