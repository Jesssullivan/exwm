//! VR subsystem — OpenXR runtime integration.
//!
//! Provides the `VrState` struct managing the full OpenXR lifecycle:
//! - Entry loading (linked or dynamic)
//! - Instance creation with extension negotiation
//! - System (HMD) discovery
//! - Session creation with OpenGL graphics binding
//! - Swapchain management and format negotiation
//! - Reference space creation (LOCAL, STAGE, VIEW)
//! - Frame submission loop (WaitFrame -> BeginFrame -> EndFrame)
//! - Session state machine with recovery
//! - Frame timing instrumentation
//!
//! Gated behind the `vr` feature flag.

#[cfg(feature = "vr")]
pub mod openxr_state;

#[cfg(feature = "vr")]
pub mod frame_timing;

#[cfg(feature = "vr")]
pub use openxr_state::{ReferenceSpaceType, VrState};

#[cfg(not(feature = "vr"))]
pub mod stub;

#[cfg(not(feature = "vr"))]
pub use stub::{ReferenceSpaceType, VrState};
