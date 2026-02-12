//! XWayland handler — X11 legacy application support.
//!
//! Implements XwmHandler to manage X11 windows via XWayland,
//! bridging them into the Wayland compositor space.

use crate::ipc::{dispatch::format_event, server::IpcServer};
use crate::state::{next_surface_id, EwwmState, SurfaceData};
use smithay::{
    delegate_xwayland_shell,
    desktop::Window,
    utils::Rectangle,
    wayland::xwayland_shell::{XWaylandShellHandler, XWaylandShellState},
    xwayland::{
        xwm::{Reorder, ResizeEdge, XwmHandler, XwmId},
        X11Surface, X11Wm,
    },
};
use tracing::{debug, info, warn};

impl XwmHandler for EwwmState {
    fn xwm_state(&mut self, _xwm: XwmId) -> &mut X11Wm {
        self.xwm.as_mut().expect("xwm not initialized")
    }

    fn new_window(&mut self, _xwm: XwmId, _window: X11Surface) {
        // Window created but not yet mapped — nothing to do until map_window_request
        debug!("XWayland: new window notification");
    }

    fn new_override_redirect_window(&mut self, _xwm: XwmId, _window: X11Surface) {
        // Override-redirect windows (menus, tooltips) are unmanaged
        debug!("XWayland: new override-redirect window");
    }

    fn map_window_request(&mut self, _xwm: XwmId, window: X11Surface) {
        // X11 client wants to map (display) a window
        let surface_id = next_surface_id();
        info!(surface_id, "XWayland: mapping X11 window");

        // Set the window as mapped
        if let Err(e) = window.set_mapped(true) {
            warn!(surface_id, "XWayland: failed to set mapped: {}", e);
            return;
        }

        // Create a unified Window element
        let win = Window::new_x11_window(window.clone());
        self.space.map_element(win.clone(), (0, 0), false);

        // Extract X11 properties
        // Smithay 0.7: class(), instance(), title() return String, not Option<String>
        let wm_class = window.class();
        let wm_instance = window.instance();
        let title = window.title();
        let is_transient = window.is_transient_for().is_some();
        let _is_override_redirect = window.is_override_redirect();

        // Helper: convert empty strings to None for Option<String> fields
        let non_empty = |s: &str| -> Option<String> {
            if s.is_empty() { None } else { Some(s.to_string()) }
        };

        // Create surface data
        let mut data = SurfaceData::new_x11(surface_id);
        data.app_id = non_empty(&wm_class).or_else(|| non_empty(&wm_instance));
        data.title = non_empty(&title);
        data.x11_class = non_empty(&wm_class);
        data.x11_instance = non_empty(&wm_instance);
        data.workspace = self.active_workspace;

        // Auto-float transient windows (dialogs)
        if is_transient {
            data.floating = true;
        }

        self.surfaces.insert(surface_id, data);
        self.surface_to_window.insert(surface_id, win);

        // Configure the window
        if let Some(geo) = self.space.elements().last().and_then(|w| {
            self.space.element_geometry(w)
        }) {
            if let Err(e) = window.configure(Some(geo)) {
                warn!(surface_id, "XWayland: configure failed: {}", e);
            }
        }

        // Emit IPC event with :x11 flag
        // Smithay 0.7: these are String, use as_str() directly
        let app_id = if wm_class.is_empty() { &wm_instance } else { &wm_class };
        let title_str = title.as_str();
        let x11_class_str = wm_class.as_str();
        let x11_instance_str = wm_instance.as_str();

        let event = format_event(
            "surface-created",
            &[
                ("id", &surface_id.to_string()),
                ("app-id", &format!("\"{}\"", escape_str(app_id))),
                ("title", &format!("\"{}\"", escape_str(title_str))),
                ("x11", "t"),
                ("x11-class", &format!("\"{}\"", escape_str(x11_class_str))),
                ("x11-instance", &format!("\"{}\"", escape_str(x11_instance_str))),
                ("transient", if is_transient { "t" } else { "nil" }),
            ],
        );
        IpcServer::broadcast_event(self, &event);
    }

    fn mapped_override_redirect_window(&mut self, _xwm: XwmId, _window: X11Surface) {
        // Override-redirect mapped — render but don't manage
        debug!("XWayland: override-redirect window mapped");
    }

    fn unmapped_window(&mut self, _xwm: XwmId, window: X11Surface) {
        // X11 window unmapped — find and remove from space
        debug!("XWayland: window unmapped");

        // Find the surface_id for this specific X11 window
        let window_id = window.window_id();
        let surface_id = self
            .surface_to_window
            .iter()
            .find(|(_, w)| {
                w.x11_surface()
                    .map(|xs| xs.window_id() == window_id)
                    .unwrap_or(false)
            })
            .map(|(id, _)| *id);

        if let Some(sid) = surface_id {
            self.surfaces.remove(&sid);
            self.surface_to_window.remove(&sid);

            // Emit IPC event
            let event = format_event("surface-destroyed", &[("id", &sid.to_string())]);
            IpcServer::broadcast_event(self, &event);
        }

        // Remove from space — collect first to avoid borrowing self.space twice
        let to_unmap = self.space.elements().find(|w| {
            w.x11_surface()
                .map(|xs| xs.window_id() == window.window_id())
                .unwrap_or(false)
        }).cloned();
        if let Some(w) = to_unmap {
            self.space.unmap_elem(&w);
        }
    }

    fn destroyed_window(&mut self, _xwm: XwmId, _window: X11Surface) {
        debug!("XWayland: window destroyed");
    }

    fn configure_request(
        &mut self,
        _xwm: XwmId,
        window: X11Surface,
        x: Option<i32>,
        y: Option<i32>,
        w: Option<u32>,
        h: Option<u32>,
        _reorder: Option<Reorder>,
    ) {
        // X11 client requests geometry change
        let geo = Rectangle::new(
            (x.unwrap_or(0), y.unwrap_or(0)).into(),
            (w.unwrap_or(800) as i32, h.unwrap_or(600) as i32).into(),
        );
        if let Err(e) = window.configure(Some(geo)) {
            warn!("XWayland: configure request failed: {}", e);
        }
    }

    fn configure_notify(
        &mut self,
        _xwm: XwmId,
        _window: X11Surface,
        _geometry: Rectangle<i32, smithay::utils::Logical>,
        _above: Option<u32>,
    ) {
        // X11 server acknowledges configuration
    }

    fn resize_request(
        &mut self,
        _xwm: XwmId,
        _window: X11Surface,
        _button: u32,
        _resize_edge: ResizeEdge,
    ) {
        // Interactive resize — stub
    }

    fn move_request(&mut self, _xwm: XwmId, _window: X11Surface, _button: u32) {
        // Interactive move — stub
    }

    fn maximize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        if let Err(e) = window.set_maximized(true) {
            warn!("XWayland: maximize failed: {}", e);
        }
    }

    fn unmaximize_request(&mut self, _xwm: XwmId, window: X11Surface) {
        if let Err(e) = window.set_maximized(false) {
            warn!("XWayland: unmaximize failed: {}", e);
        }
    }

    fn fullscreen_request(&mut self, _xwm: XwmId, window: X11Surface) {
        if let Err(e) = window.set_fullscreen(true) {
            warn!("XWayland: fullscreen failed: {}", e);
        }
    }

    fn unfullscreen_request(&mut self, _xwm: XwmId, window: X11Surface) {
        if let Err(e) = window.set_fullscreen(false) {
            warn!("XWayland: unfullscreen failed: {}", e);
        }
    }

    fn minimize_request(&mut self, _xwm: XwmId, _window: X11Surface) {
        // Minimize — stub
    }
}

impl XWaylandShellHandler for EwwmState {
    fn xwayland_shell_state(&mut self) -> &mut XWaylandShellState {
        &mut self.xwayland_shell_state
    }
}

delegate_xwayland_shell!(EwwmState);

/// Escape a string for s-expression embedding.
fn escape_str(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}
