//! Input event handling — keyboard, pointer, focus management.

use crate::ipc::{dispatch::format_event, server::IpcServer};
use crate::state::EwwmState;
use smithay::{
    backend::input::{
        AbsolutePositionEvent, Axis, AxisSource, ButtonState, Event, InputBackend, InputEvent,
        KeyState, KeyboardKeyEvent, PointerAxisEvent, PointerButtonEvent,
        PointerMotionAbsoluteEvent,
    },
    input::{
        keyboard::{FilterResult, KeyboardHandle, ModifiersState},
        pointer::{AxisFrame, ButtonEvent, MotionEvent},
    },
    utils::SERIAL_COUNTER,
};
use tracing::{debug, trace};

/// Handle an input event from any backend.
pub fn handle_input<B: InputBackend>(state: &mut EwwmState, event: InputEvent<B>) {
    match event {
        InputEvent::Keyboard { event } => handle_keyboard::<B>(state, event),
        InputEvent::PointerMotionAbsolute { event } => {
            handle_pointer_motion_absolute::<B>(state, event)
        }
        InputEvent::PointerButton { event } => handle_pointer_button::<B>(state, event),
        InputEvent::PointerAxis { event } => handle_pointer_axis::<B>(state, event),
        _ => {}
    }
}

/// Convert xkbcommon modifiers + keysym to an Emacs-style key description.
fn format_key_description(keysym: xkbcommon::xkb::Keysym, mods: &ModifiersState) -> Option<String> {
    let sym_name = xkbcommon::xkb::keysym_get_name(keysym);

    // Map common keysym names to Emacs names
    let key_name = match sym_name.as_str() {
        "Return" => "RET".to_string(),
        "Escape" => "ESC".to_string(),
        "BackSpace" => "DEL".to_string(),
        "Tab" => "TAB".to_string(),
        "space" => "SPC".to_string(),
        "Delete" => "delete".to_string(),
        name if name.len() == 1 => name.to_lowercase(),
        name => name.to_string(),
    };

    let mut desc = String::new();
    if mods.ctrl {
        desc.push_str("C-");
    }
    if mods.alt {
        desc.push_str("M-");
    }
    if mods.logo {
        desc.push_str("s-");
    }
    if mods.shift && key_name.len() > 1 {
        desc.push_str("S-");
    }
    desc.push_str(&key_name);

    Some(desc)
}

fn handle_keyboard<B: InputBackend>(state: &mut EwwmState, event: B::KeyboardKeyEvent) {
    let serial = SERIAL_COUNTER.next_serial();
    let time = Event::time_msec(&event);
    let keycode = event.key_code();
    let key_state = event.state();

    let keyboard = state.seat.get_keyboard().unwrap();

    // Check for grabbed keys
    let _grab_result =
        keyboard.input::<bool, _>(state, keycode, key_state, serial, time, |state, mods, handle| {
            if key_state == KeyState::Pressed {
                let keysym = handle.modified_sym();
                if let Some(key_desc) = format_key_description(keysym, mods) {
                    if state.grabbed_keys.contains(&key_desc) {
                        debug!(key = %key_desc, "grabbed key intercepted");
                        // Emit key-pressed event to IPC clients
                        let event = format_event(
                            "key-pressed",
                            &[
                                ("key", &format!("\"{}\"", key_desc)),
                                (
                                    "modifiers",
                                    &format!(
                                        "(:super {} :ctrl {} :alt {} :shift {})",
                                        if mods.logo { "t" } else { "nil" },
                                        if mods.ctrl { "t" } else { "nil" },
                                        if mods.alt { "t" } else { "nil" },
                                        if mods.shift { "t" } else { "nil" },
                                    ),
                                ),
                                ("timestamp", &time.to_string()),
                            ],
                        );
                        IpcServer::broadcast_event(state, &event);
                        return FilterResult::Intercept(true);
                    }
                }
            }
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
        let surface_under = state.space.element_under(pos).map(|(w, loc)| {
            let surface = w
                .toplevel()
                .expect("window has toplevel")
                .wl_surface()
                .clone();
            (surface, loc.to_f64())
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
    // Smithay 0.7: current_focus() returns Option<WlSurface>, not a tuple
    if button_state == ButtonState::Pressed {
        if let Some(_surface) = pointer.current_focus() {
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
