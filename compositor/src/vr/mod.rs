//! VR subsystem — OpenXR runtime integration and 3D scene management.
//!
//! Provides:
//! - `VrState`: OpenXR lifecycle (gated behind `vr` feature)
//! - `scene`: 3D scene graph for Wayland surfaces in VR
//! - `texture`: DMA-BUF texture import pipeline (gated behind `vr` feature)
//! - `vr_renderer`: Stereo rendering to OpenXR swapchains (gated behind `vr` feature)

#[cfg(feature = "vr")]
pub mod openxr_state;

#[cfg(feature = "vr")]
pub mod frame_timing;

#[cfg(feature = "vr")]
pub mod texture;

#[cfg(feature = "vr")]
pub mod vr_renderer;

#[cfg(feature = "vr")]
pub use openxr_state::{ReferenceSpaceType, VrState};

#[cfg(not(feature = "vr"))]
pub mod stub;

#[cfg(not(feature = "vr"))]
pub use stub::{ReferenceSpaceType, VrState};

// Scene graph, DRM lease, interaction, and eye tracking are always available (no openxrs dependency).
pub mod scene;
pub mod drm_lease;
pub mod vr_interaction;
pub mod eye_tracking;
pub mod gaze_focus;
