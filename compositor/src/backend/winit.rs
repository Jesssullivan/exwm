//! Winit backend — development mode, compositor runs inside a window.

use crate::{render, state::EwwmState};
use smithay::{
    backend::winit::{self as winit_backend, WinitEvent},
    output::{Mode as OutputMode, Output, PhysicalProperties, Subpixel},
    reexports::calloop::{
        timer::{TimeoutAction, Timer},
        EventLoop,
    },
    reexports::wayland_server::Display,
    utils::{Rectangle, Size, Transform},
};
use std::time::Duration;
use tracing::{error, info, warn};

pub fn run(socket_name: Option<String>) -> anyhow::Result<()> {
    let mut event_loop = EventLoop::<EwwmState>::try_new()?;
    let mut display = Display::<EwwmState>::new()?;

    let mut state = EwwmState::new(&mut display, event_loop.handle());

    // Initialize Winit backend
    let (mut backend, mut winit_evt) = winit_backend::init::<
        smithay::backend::renderer::gles::GlesRenderer,
    >()?;

    // Create output matching window size
    let mode = OutputMode {
        size: (1920, 1080).into(),
        refresh: 60_000,
    };
    let output = Output::new(
        "winit-0".to_string(),
        PhysicalProperties {
            size: (0, 0).into(),
            subpixel: Subpixel::Unknown,
            make: "EWWM".into(),
            model: "Winit".into(),
        },
    );
    output.change_current_state(Some(mode), Some(Transform::Normal), None, Some((0, 0).into()));
    output.set_preferred(mode);
    state.space.map_output(&output, (0, 0));

    // Set up Wayland socket
    let socket = if let Some(name) = socket_name {
        display.handle().add_socket_name(name)?
    } else {
        display.handle().add_socket_auto()?
    };
    info!("Wayland socket: {}", socket.to_string_lossy());
    std::env::set_var("WAYLAND_DISPLAY", &socket);

    // Insert Wayland display source into event loop
    event_loop.handle().insert_source(
        smithay::reexports::calloop::generic::Generic::new(
            display.backend().poll_fd(),
            smithay::reexports::calloop::Interest::READ,
            smithay::reexports::calloop::Mode::Level,
        ),
        |_, _, state: &mut EwwmState| {
            // This will be connected to display dispatch
            Ok(smithay::reexports::calloop::PostAction::Continue)
        },
    )?;

    // Render timer (60 Hz)
    event_loop.handle().insert_source(
        Timer::from_duration(Duration::from_millis(16)),
        move |_, _, state| {
            // Render frame
            render::render_winit(&mut backend, state, &output);
            TimeoutAction::ToDuration(Duration::from_millis(16))
        },
    )?;

    info!("Winit backend initialized, entering event loop");

    // Main event loop
    while state.running {
        // Dispatch Winit events
        winit_evt.dispatch_new_events(|event| match event {
            WinitEvent::Resized { size, .. } => {
                let mode = OutputMode {
                    size,
                    refresh: 60_000,
                };
                output.change_current_state(Some(mode), None, None, None);
            }
            WinitEvent::CloseRequested => {
                state.running = false;
            }
            WinitEvent::Input(event) => {
                crate::input::handle_input(&mut state, event);
            }
            _ => {}
        })?;

        // Dispatch calloop events
        event_loop.dispatch(Some(Duration::from_millis(1)), &mut state)?;

        // Flush client events
        display.flush_clients()?;
    }

    info!("Winit backend shutting down");
    Ok(())
}
