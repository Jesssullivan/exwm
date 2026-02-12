//! xdg_shell handler — application window lifecycle and popup management.

use crate::ipc::{dispatch::format_event, server::IpcServer};
use crate::state::{next_surface_id, EwwmState, SurfaceData};
use smithay::{
    delegate_xdg_shell,
    desktop::{
        find_popup_root_surface, layer_map_for_output, PopupKind, PopupManager, Space, Window,
        WindowSurfaceType,
    },
    input::{
        pointer::{Focus, GrabStartData as PointerGrabStartData},
        Seat,
    },
    reexports::wayland_server::protocol::wl_seat::WlSeat,
    utils::Serial,
    wayland::shell::xdg::{
        PopupSurface, PositionerState, ToplevelSurface, XdgShellHandler, XdgShellState,
        XdgToplevelSurfaceData,
    },
};
use tracing::{debug, info, warn};

impl XdgShellHandler for EwwmState {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.xdg_shell_state
    }

    fn new_toplevel(&mut self, surface: ToplevelSurface) {
        let surface_id = next_surface_id();
        info!(surface_id, "new toplevel surface");

        let window = Window::new_wayland_window(surface.clone());
        self.space.map_element(window.clone(), (0, 0), false);
        self.surface_to_window.insert(surface_id, window);

        let mut data = SurfaceData::new(surface_id);
        data.workspace = self.active_workspace;
        self.surfaces.insert(surface_id, data);

        // Emit IPC event
        let event = format_event(
            "surface-created",
            &[
                ("id", &surface_id.to_string()),
                ("app-id", "\"\""),
                ("title", "\"\""),
                ("x11", "nil"),
            ],
        );
        IpcServer::broadcast_event(self, &event);

        // Send initial configure
        surface.send_configure();
    }

    fn new_popup(&mut self, surface: PopupSurface, _positioner: PositionerState) {
        // Track the popup (NOT as a managed surface — no ewwm buffer)
        if let Err(e) = self.popups.track_popup(PopupKind::from(surface)) {
            warn!("Failed to track popup: {}", e);
        }
    }

    fn grab(&mut self, surface: PopupSurface, seat: WlSeat, serial: Serial) {
        let seat = match Seat::from_resource(&seat) {
            Some(s) => s,
            None => return,
        };

        let kind = PopupKind::Xdg(surface);

        // Find the root surface that this popup belongs to
        let root = match find_popup_root_surface(&kind) {
            Ok(root) => root,
            Err(_) => return,
        };

        // Try to grab
        if let Ok(grab) = self.popups.grab_popup(root, kind, &seat, serial) {
            if let Some(keyboard) = seat.get_keyboard() {
                if let Some(focus) = grab.current_grab() {
                    keyboard.set_focus(self, Some(focus), serial);
                }
                keyboard.set_grab(
                    self,
                    smithay::desktop::PopupKeyboardGrab::new(&grab),
                    serial,
                );
            }
            if let Some(pointer) = seat.get_pointer() {
                pointer.set_grab(
                    self,
                    smithay::desktop::PopupPointerGrab::new(&grab),
                    serial,
                    Focus::Keep,
                );
            }
        }
    }

    fn reposition_request(&mut self, surface: PopupSurface, positioner: PositionerState, token: u32) {
        surface.with_pending_state(|state| {
            state.geometry = positioner.get_geometry();
            state.positioner = positioner;
        });
        surface.send_repositioned(token);
    }

    fn toplevel_destroyed(&mut self, surface: ToplevelSurface) {
        // Find surface_id by matching the WlSurface
        let wl_surface = surface.wl_surface().clone();
        let surface_id = self.surfaces.iter().find_map(|(id, _data)| {
            // Match by checking space elements
            self.space.elements().any(|w| {
                w.toplevel()
                    .map(|t| *t.wl_surface() == wl_surface)
                    .unwrap_or(false)
            }).then_some(*id)
        });

        if let Some(sid) = surface_id {
            self.surfaces.remove(&sid);
            self.surface_to_window.remove(&sid);

            let event = format_event("surface-destroyed", &[("id", &sid.to_string())]);
            IpcServer::broadcast_event(self, &event);
        }

        // Remove from space
        let window = self.space.elements().find(|w| {
            w.toplevel()
                .map(|t| *t.wl_surface() == wl_surface)
                .unwrap_or(false)
        }).cloned();

        if let Some(w) = window {
            self.space.unmap_elem(&w);
        }
    }
}

delegate_xdg_shell!(EwwmState);
