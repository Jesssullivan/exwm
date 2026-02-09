//! Rendering pipeline — surface compositing and frame submission.

use smithay::{
    backend::{
        renderer::{
            element::surface::WaylandSurfaceRenderElement,
            gles::GlesRenderer,
            Color32F, Frame, Renderer,
        },
        winit::WinitGraphicsBackend,
    },
    desktop::Space,
    output::Output,
    utils::{Physical, Rectangle, Transform},
};
use tracing::{trace, warn};

use crate::state::EwwmState;

/// Background color (Catppuccin Mocha base).
const BG_COLOR: Color32F = Color32F::new(0.118, 0.118, 0.180, 1.0);

/// Render a frame using the Winit backend.
pub fn render_winit(
    backend: &mut WinitGraphicsBackend<GlesRenderer>,
    state: &mut EwwmState,
    output: &Output,
) {
    let size = backend.window_size();
    let damage = Rectangle::from_loc_and_size((0, 0), size);

    // Bind render target
    match backend.bind() {
        Ok(()) => {}
        Err(e) => {
            warn!("Failed to bind winit backend: {}", e);
            return;
        }
    }

    let renderer = backend.renderer();

    // Begin render
    match renderer.render(size, Transform::Normal) {
        Ok(mut frame) => {
            // Clear background
            if let Err(e) = frame.clear(BG_COLOR, &[damage]) {
                warn!("Failed to clear frame: {}", e);
            }

            // Render surfaces from space
            // Space render elements would go here in full implementation
            // For now, just clear to background

            if let Err(e) = frame.finish() {
                warn!("Failed to finish frame: {}", e);
            }
        }
        Err(e) => {
            warn!("Failed to begin render: {}", e);
        }
    }

    // Submit frame
    if let Err(e) = backend.submit(Some(&[damage])) {
        warn!("Failed to submit frame: {}", e);
    }

    // Tell space we rendered
    state.space.elements().for_each(|window| {
        window.send_frame(
            output,
            state.space.output_geometry(output).unwrap().loc,
            std::time::Duration::from_millis(16),
            None,
        );
    });
}
