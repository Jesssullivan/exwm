//! Compositor state — the central struct holding all Smithay state.
//!
//! Follows niri pattern: single `EwwmState` struct owns everything,
//! passed as `&mut self` to all handler trait implementations.

use smithay::{
    delegate_compositor, delegate_data_device, delegate_output, delegate_seat, delegate_shm,
    delegate_xdg_shell,
    desktop::{Space, Window},
    input::{Seat, SeatState},
    reexports::{
        calloop::{generic::Generic, Interest, LoopHandle, Mode, PostAction},
        wayland_server::{
            backend::{ClientData, ClientId, DisconnectReason},
            protocol::wl_surface::WlSurface,
            Display, DisplayHandle,
        },
    },
    wayland::{
        compositor::{CompositorClientState, CompositorState},
        output::OutputManagerState,
        selection::data_device::DataDeviceState,
        shell::xdg::XdgShellState,
        shm::ShmState,
    },
};
use std::{
    collections::HashMap,
    sync::atomic::{AtomicU64, Ordering},
};
use tracing::info;

/// Monotonically increasing surface ID generator.
static NEXT_SURFACE_ID: AtomicU64 = AtomicU64::new(1);

/// Generate a unique surface ID.
pub fn next_surface_id() -> u64 {
    NEXT_SURFACE_ID.fetch_add(1, Ordering::Relaxed)
}

/// Tracked surface data for Emacs IPC reporting.
#[derive(Debug)]
pub struct SurfaceData {
    pub surface_id: u64,
    pub app_id: Option<String>,
    pub title: Option<String>,
}

/// Central compositor state.
pub struct EwwmState {
    // Wayland core
    pub display_handle: DisplayHandle,
    pub loop_handle: LoopHandle<'static, Self>,

    // Protocol states
    pub compositor_state: CompositorState,
    pub xdg_shell_state: XdgShellState,
    pub shm_state: ShmState,
    pub output_state: OutputManagerState,
    pub seat_state: SeatState<Self>,
    pub data_device_state: DataDeviceState,

    // Input
    pub seat: Seat<Self>,

    // Window management
    pub space: Space<Window>,
    pub surfaces: HashMap<u64, SurfaceData>,

    // Shutdown flag
    pub running: bool,
}

impl EwwmState {
    pub fn new(
        display: &mut Display<Self>,
        loop_handle: LoopHandle<'static, Self>,
    ) -> Self {
        let display_handle = display.handle();

        let compositor_state = CompositorState::new::<Self>(&display_handle);
        let xdg_shell_state = XdgShellState::new::<Self>(&display_handle);
        let shm_state = ShmState::new::<Self>(&display_handle, vec![]);
        let output_state = OutputManagerState::new_with_xdg_output::<Self>(&display_handle);
        let mut seat_state = SeatState::new();
        let data_device_state = DataDeviceState::new::<Self>(&display_handle);

        let seat = seat_state.new_wl_seat(&display_handle, "ewwm-seat");

        info!("EwwmState initialized");

        Self {
            display_handle,
            loop_handle,
            compositor_state,
            xdg_shell_state,
            shm_state,
            output_state,
            seat_state,
            data_device_state,
            seat,
            space: Space::default(),
            surfaces: HashMap::new(),
            running: true,
        }
    }
}

/// Per-client state required by Smithay's CompositorHandler.
#[derive(Default)]
pub struct ClientState {
    pub compositor_state: CompositorClientState,
}

impl ClientData for ClientState {
    fn initialized(&self, _client_id: ClientId) {}
    fn disconnected(&self, _client_id: ClientId, _reason: DisconnectReason) {}
}
