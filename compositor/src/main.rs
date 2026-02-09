//! EWWM Compositor - VR-first Wayland compositor built on Smithay
//!
//! Part of the EXWM-VR project: a transhuman Emacs window manager.

mod backend;
mod handlers;
mod input;
pub mod ipc;
mod render;
mod state;
pub mod vr;

use clap::Parser;
use tracing::info;

#[derive(Parser, Debug)]
#[command(name = "ewwm-compositor", about = "EXWM-VR Wayland compositor")]
struct Cli {
    /// Backend to use: winit, drm, or headless
    #[arg(long, default_value = "auto")]
    backend: String,

    /// Wayland socket name (default: auto-assigned)
    #[arg(long)]
    wayland_socket: Option<String>,

    /// Exit after N seconds (headless mode testing)
    #[arg(long)]
    headless_exit_after: Option<u64>,

    /// IPC socket path (default: $XDG_RUNTIME_DIR/ewwm-ipc.sock)
    #[arg(long)]
    ipc_socket: Option<String>,

    /// Log all IPC messages to stderr
    #[arg(long)]
    ipc_trace: bool,

    /// Show version and exit
    #[arg(long)]
    version: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    if cli.version {
        println!("ewwm-compositor {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "ewwm_compositor=info,smithay=warn".into()),
        )
        .init();

    info!("ewwm-compositor v{} starting", env!("CARGO_PKG_VERSION"));
    info!("backend: {}", cli.backend);

    let backend_type = match cli.backend.as_str() {
        "winit" => backend::BackendType::Winit,
        "drm" => backend::BackendType::Drm,
        "headless" => backend::BackendType::Headless,
        "auto" => {
            if std::env::var("DISPLAY").is_ok() || std::env::var("WAYLAND_DISPLAY").is_ok() {
                info!("auto-detected: running under existing display, using winit backend");
                backend::BackendType::Winit
            } else {
                info!("auto-detected: no display found, using drm backend");
                backend::BackendType::Drm
            }
        }
        other => {
            eprintln!("Unknown backend: {other}. Use: winit, drm, headless, or auto");
            std::process::exit(1);
        }
    };

    let ipc_socket = cli.ipc_socket.map(std::path::PathBuf::from);

    backend::run(
        backend_type,
        cli.wayland_socket,
        cli.headless_exit_after,
        ipc_socket,
        cli.ipc_trace,
    )
}
