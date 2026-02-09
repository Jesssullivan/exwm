//! wl_seat handler — input device management.

use crate::state::EwwmState;
use smithay::{
    delegate_data_device, delegate_output, delegate_seat,
    input::{SeatHandler, SeatState},
    reexports::wayland_server::protocol::wl_surface::WlSurface,
    wayland::selection::data_device::{
        ClientDndGrabHandler, DataDeviceHandler, DataDeviceState, ServerDndGrabHandler,
    },
};

impl SeatHandler for EwwmState {
    type KeyboardFocus = WlSurface;
    type PointerFocus = WlSurface;
    type TouchFocus = WlSurface;

    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.seat_state
    }

    fn cursor_image(
        &mut self,
        _seat: &smithay::input::Seat<Self>,
        _image: smithay::input::pointer::CursorImageStatus,
    ) {
        // Cursor image handling — stub
    }

    fn focus_changed(
        &mut self,
        _seat: &smithay::input::Seat<Self>,
        _focused: Option<&WlSurface>,
    ) {
        // Focus change notification — will notify Emacs via IPC in Week 4
    }
}

impl DataDeviceHandler for EwwmState {
    fn data_device_state(&self) -> &DataDeviceState {
        &self.data_device_state
    }
}

impl ClientDndGrabHandler for EwwmState {}
impl ServerDndGrabHandler for EwwmState {}

delegate_seat!(EwwmState);
delegate_data_device!(EwwmState);
delegate_output!(EwwmState);
