//! wlr-layer-shell handler — panels, overlays, and screen-edge surfaces.
//!
//! Implements WlrLayerShellHandler for waybar, mako, rofi, and other
//! layer-shell clients. Manages exclusive zones that reduce the
//! usable area for tiling layout.

use crate::ipc::{dispatch::format_event, server::IpcServer};
use crate::state::EwwmState;
use smithay::{
    delegate_layer_shell,
    desktop::{
        layer_map_for_output,
        LayerSurface,
    },
    output::Output,
    wayland::shell::wlr_layer::{
        ExclusiveZone, WlrLayerShellHandler, WlrLayerShellState,
    },
};
use tracing::{debug, info, warn};

impl WlrLayerShellHandler for EwwmState {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.layer_shell_state
    }

    fn new_layer_surface(&mut self, surface: LayerSurface) {
        info!("layer-shell: new surface");

        // Default to first output
        let output = self.space.outputs().next().cloned();

        let Some(output) = output else {
            warn!("layer-shell: no output available for layer surface");
            return;
        };

        let mut map = layer_map_for_output(&output);
        if let Err(e) = map.map_layer(&surface) {
            warn!("layer-shell: failed to map layer surface: {}", e);
            return;
        }

        // Recalculate usable area for tiling layout
        drop(map);
        self.recalculate_usable_area(&output);

        debug!("layer-shell: surface mapped");
    }

    fn layer_destroyed(&mut self, surface: LayerSurface) {
        debug!("layer-shell: surface destroyed");

        let outputs: Vec<Output> = self.space.outputs().cloned().collect();

        let target_output = outputs.into_iter().find(|o| {
            let map = layer_map_for_output(o);
            map.layers().any(|l| l == &surface)
        });

        if let Some(output) = target_output {
            let mut map = layer_map_for_output(&output);
            map.unmap_layer(&surface);
            drop(map);
            self.recalculate_usable_area(&output);
        }
    }
}

impl EwwmState {
    /// Recalculate the usable output area after layer-shell changes.
    /// Sends IPC event to Emacs so layout engine can adjust.
    pub fn recalculate_usable_area(&mut self, output: &Output) {
        let output_geo = match self.space.output_geometry(output) {
            Some(g) => g,
            None => return,
        };

        // Start with full output area
        let mut x = output_geo.loc.x;
        let mut y = output_geo.loc.y;
        let mut w = output_geo.size.w;
        let mut h = output_geo.size.h;

        // Subtract exclusive zones from layer surfaces
        let map = layer_map_for_output(output);
        for layer in map.layers() {
            let layer_data = layer.cached_state();

            let exclusive: i32 = match layer_data.exclusive_zone {
                ExclusiveZone::Exclusive(val) => val as i32,
                _ => continue,
            };

            let anchor = layer_data.anchor;

            // Top-anchored (like waybar at top)
            if anchor.contains(smithay::wayland::shell::wlr_layer::Anchor::TOP)
                && !anchor.contains(smithay::wayland::shell::wlr_layer::Anchor::BOTTOM)
            {
                y += exclusive;
                h -= exclusive;
            }
            // Bottom-anchored
            else if anchor.contains(smithay::wayland::shell::wlr_layer::Anchor::BOTTOM)
                && !anchor.contains(smithay::wayland::shell::wlr_layer::Anchor::TOP)
            {
                h -= exclusive;
            }
            // Left-anchored
            else if anchor.contains(smithay::wayland::shell::wlr_layer::Anchor::LEFT)
                && !anchor.contains(smithay::wayland::shell::wlr_layer::Anchor::RIGHT)
            {
                x += exclusive;
                w -= exclusive;
            }
            // Right-anchored
            else if anchor.contains(smithay::wayland::shell::wlr_layer::Anchor::RIGHT)
                && !anchor.contains(smithay::wayland::shell::wlr_layer::Anchor::LEFT)
            {
                w -= exclusive;
            }
        }
        drop(map);

        self.usable_area.x = x;
        self.usable_area.y = y;
        self.usable_area.w = w;
        self.usable_area.h = h;

        // Emit IPC event so Emacs layout engine can adjust
        let event = format_event(
            "output-usable-area-changed",
            &[
                ("x", &x.to_string()),
                ("y", &y.to_string()),
                ("w", &w.to_string()),
                ("h", &h.to_string()),
            ],
        );
        IpcServer::broadcast_event(self, &event);

        info!(x, y, w, h, "layer-shell: usable area updated");
    }
}

delegate_layer_shell!(EwwmState);
