//! VR stub — used when the `vr` feature is not enabled.
//!
//! Provides a no-op VrState so the rest of the compositor compiles
//! without OpenXR dependencies (e.g., s390x headless builds).

use tracing::info;

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
}

impl Default for VrState {
    fn default() -> Self {
        Self { enabled: false }
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
