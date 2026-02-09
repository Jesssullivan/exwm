//! DRM backend — production mode, direct hardware rendering.
//!
//! Requires libseat session, DRM device access, and KMS/GBM.
//! This is the backend used when running as a display server.

use tracing::{error, info};

pub fn run(socket_name: Option<String>) -> anyhow::Result<()> {
    // DRM backend implementation follows the anvil pattern:
    // 1. LibSeatSession::new() for seat access
    // 2. Enumerate DRM devices via udev
    // 3. Open primary DRM device
    // 4. Enumerate connectors/CRTCs
    // 5. GbmAllocator + GlesRenderer
    // 6. DrmCompositor per output
    // 7. Register DRM fd as calloop source for vblank
    //
    // Full implementation in Week 3 stage 3.6
    // Currently a placeholder that errors with a helpful message.

    info!("DRM backend selected");
    error!(
        "DRM backend not yet fully implemented. \
         Use --backend winit for development or --backend headless for CI."
    );

    // TODO: Full DRM implementation
    // For now, set up the basic structure so the code compiles.
    Err(anyhow::anyhow!(
        "DRM backend requires Linux with libseat. Use --backend winit for development."
    ))
}
