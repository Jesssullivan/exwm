//! Input event handling — keyboard, pointer, focus management.

use crate::state::EwwmState;
use smithay::{
    backend::input::{
        AbsolutePositionEvent, Axis, AxisSource, ButtonState, Event, InputBackend, InputEvent,
        KeyState, KeyboardKeyEvent, PointerAxisEvent, PointerButtonEvent, PointerMotionAbsoluteEvent,
    },
    input::{
        keyboard::{FilterResult, KeyboardHandle},
        pointer::{AxisFrame, ButtonEvent, MotionEvent},
    },
    utils::SERIAL_COUNTER,
};
use tracing::trace;

/// Handle an input event from any backend.
pub fn handle_input<B: InputBackend>(state: &mut EwwmState, event: InputEvent<B>) {
    match event {
        InputEvent::Keyboard { event } => handle_keyboard(state, event),
        InputEvent::PointerMotionAbsolute { event } => handle_pointer_motion_absolute(state, event),
        InputEvent::PointerButton { event } => handle_pointer_button(state, event),
        InputEvent::PointerAxis { event } => handle_pointer_axis(state, event),
        _ => {}
    }
}

fn handle_keyboard<B: InputBackend>(state: &mut EwwmState, event: B::KeyboardKeyEvent) {
    let serial = SERIAL_COUNTER.next_serial();
    let time = Event::time_msec(&event);
    let keycode = event.key_code();
    let key_state = event.state();

    let keyboard = state.seat.get_keyboard().unwrap();
    keyboard.input::<(), _>(state, keycode, key_state, serial, time, |_, _, _| {
        FilterResult::Forward
    });
}

fn handle_pointer_motion_absolute<B: InputBackend>(
    state: &mut EwwmState,
    event: B::PointerMotionAbsoluteEvent,
) {
    let output = state.space.outputs().next().cloned();
    if let Some(output) = output {
        let output_geo = state.space.output_geometry(&output).unwrap();
        let pos = event.position_transformed(output_geo.size);

        let serial = SERIAL_COUNTER.next_serial();
        let pointer = state.seat.get_pointer().unwrap();

        // Find surface under pointer for focus
        let surface_under = state
            .space
            .element_under(pos)
            .map(|(w, loc)| {
                let surface = w.toplevel().expect("window has toplevel").wl_surface().clone();
                (surface, loc)
            });

        pointer.motion(
            state,
            surface_under,
            &MotionEvent {
                location: pos,
                serial,
                time: Event::time_msec(&event) as u32,
            },
        );
    }
}

fn handle_pointer_button<B: InputBackend>(state: &mut EwwmState, event: B::PointerButtonEvent) {
    let serial = SERIAL_COUNTER.next_serial();
    let button = event.button_code();
    let button_state = event.state();

    let pointer = state.seat.get_pointer().unwrap();
    pointer.button(
        state,
        &ButtonEvent {
            button,
            state: button_state,
            serial,
            time: Event::time_msec(&event) as u32,
        },
    );

    // Focus follows click: set keyboard focus to surface under pointer
    if button_state == ButtonState::Pressed {
        if let Some((_surface, _loc)) = pointer.current_focus() {
            // Keyboard focus will follow pointer focus
        }
    }
}

fn handle_pointer_axis<B: InputBackend>(state: &mut EwwmState, event: B::PointerAxisEvent) {
    let source = event.source();
    let horizontal = event.amount(Axis::Horizontal).unwrap_or(0.0);
    let vertical = event.amount(Axis::Vertical).unwrap_or(0.0);

    let pointer = state.seat.get_pointer().unwrap();
    let mut frame = AxisFrame::new(Event::time_msec(&event) as u32).source(source);
    if horizontal != 0.0 {
        frame = frame.value(Axis::Horizontal, horizontal);
    }
    if vertical != 0.0 {
        frame = frame.value(Axis::Vertical, vertical);
    }
    pointer.axis(state, frame);
    pointer.frame(state);
}
