//! Headless backend — CI testing without GPU or display.

use crate::state::EwwmState;
use smithay::{
    output::{Mode as OutputMode, Output, PhysicalProperties, Subpixel},
    reexports::{
        calloop::{
            timer::{TimeoutAction, Timer},
            EventLoop,
        },
        wayland_server::Display,
    },
    utils::Transform,
};
use std::time::Duration;
use tracing::info;

pub fn run(
    socket_name: Option<String>,
    exit_after: Option<u64>,
) -> anyhow::Result<()> {
    let mut event_loop = EventLoop::<EwwmState>::try_new()?;
    let mut display = Display::<EwwmState>::new()?;

    let mut state = EwwmState::new(&mut display, event_loop.handle());

    // Create virtual output
    let mode = OutputMode {
        size: (1920, 1080).into(),
        refresh: 60_000,
    };
    let output = Output::new(
        "headless-0".to_string(),
        PhysicalProperties {
            size: (0, 0).into(),
            subpixel: Subpixel::Unknown,
            make: "EWWM".into(),
            model: "Headless".into(),
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

    // Exit timer for CI
    if let Some(seconds) = exit_after {
        info!("Will exit after {} seconds", seconds);
        event_loop.handle().insert_source(
            Timer::from_duration(Duration::from_secs(seconds)),
            |_, _, state: &mut EwwmState| {
                info!("Headless exit timer fired");
                state.running = false;
                TimeoutAction::Drop
            },
        )?;
    }

    info!("Headless backend initialized, entering event loop");

    while state.running {
        event_loop.dispatch(Some(Duration::from_millis(100)), &mut state)?;
        display.flush_clients()?;
    }

    info!("Headless backend shutting down");
    Ok(())
}
