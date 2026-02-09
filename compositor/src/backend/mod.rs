//! Backend abstraction — DRM, Winit, and headless backends.

pub mod drm;
pub mod headless;
pub mod winit;

/// Backend type selector.
#[derive(Debug, Clone, Copy)]
pub enum BackendType {
    Winit,
    Drm,
    Headless,
}

/// Run the compositor with the selected backend.
pub fn run(
    backend: BackendType,
    socket_name: Option<String>,
    headless_exit_after: Option<u64>,
) -> anyhow::Result<()> {
    match backend {
        BackendType::Winit => winit::run(socket_name),
        BackendType::Drm => drm::run(socket_name),
        BackendType::Headless => headless::run(socket_name, headless_exit_after),
    }
}
