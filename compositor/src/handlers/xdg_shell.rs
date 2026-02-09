//! xdg_shell handler — application window lifecycle.

use crate::state::{next_surface_id, EwwmState, SurfaceData};
use smithay::{
    delegate_xdg_shell,
    desktop::{Space, Window},
    reexports::wayland_server::protocol::wl_seat::WlSeat,
    utils::Serial,
    wayland::shell::xdg::{
        PopupSurface, PositionerState, ToplevelSurface, XdgShellHandler, XdgShellState,
    },
};
use tracing::info;

impl XdgShellHandler for EwwmState {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.xdg_shell_state
    }

    fn new_toplevel(&mut self, surface: ToplevelSurface) {
        let surface_id = next_surface_id();
        info!(surface_id, "new toplevel surface");

        let window = Window::new_wayland_window(surface.clone());
        self.space.map_element(window, (0, 0), false);

        self.surfaces.insert(
            surface_id,
            SurfaceData {
                surface_id,
                app_id: None,
                title: None,
            },
        );

        // Send initial configure
        surface.send_configure();
    }

    fn new_popup(&mut self, _surface: PopupSurface, _positioner: PositionerState) {
        // Popup support — stub for now
    }

    fn grab(&mut self, _surface: PopupSurface, _seat: WlSeat, _serial: Serial) {
        // Interactive grab — stub for now
    }

    fn reposition(&mut self, _surface: PopupSurface, _positioner: PositionerState, _token: u32) {
        // Popup reposition — stub
    }
}

delegate_xdg_shell!(EwwmState);
