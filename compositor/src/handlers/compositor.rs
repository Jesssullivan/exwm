//! wl_compositor and wl_buffer handler.

use crate::state::{ClientState, EwwmState};
use smithay::{
    backend::renderer::utils::on_commit_buffer_handler,
    delegate_compositor,
    reexports::wayland_server::{protocol::wl_surface::WlSurface, Client},
    wayland::compositor::{
        get_parent, is_sync_subsurface, CompositorClientState, CompositorHandler, CompositorState,
    },
};
use tracing::trace;

impl CompositorHandler for EwwmState {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.compositor_state
    }
}

impl smithay::wayland::buffer::BufferHandler for EwwmState {
    fn buffer_destroyed(
        &mut self,
        _buffer: &smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
    ) {
    }
}

delegate_compositor!(EwwmState);
smithay::delegate_buffer!(EwwmState);
