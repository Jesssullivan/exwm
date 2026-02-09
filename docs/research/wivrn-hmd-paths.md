# WiVRn Streaming & Alternative HMD Connection Paths

## Overview

This document covers the practical implementation landscape for connecting VR
headsets to a Smithay-based Wayland compositor on NixOS. It evaluates WiVRn
streaming architecture, codec selection for text-heavy workloads, wired
DisplayPort paths, simulated/headless development, Linux HMD driver status,
compositor overlay approaches, and hotplug handling.

---

## 1. WiVRn Architecture

Source: https://github.com/WiVRn/WiVRn

### High-Level Design

WiVRn is a Monado-based OpenXR streaming application that transmits rendered VR
frames from a Linux PC to a standalone Android headset (Quest, Pico, Vive Focus,
etc.) over Wi-Fi or USB tethering. It consists of three components:

```
Linux PC                                    Standalone HMD
+-------------------------------------------+   +-------------------+
| Monado (OpenXR runtime, forked)           |   | WiVRn Client APK  |
|   +-- OpenXR app submits frames           |   |   +-- Decoder     |
|   +-- WiVRn capture layer intercepts      |   |   +-- TimeWarp    |
|   +-- Encoder pipeline (GPU)              |   |   +-- Display     |
|       +-- VA-API / NVENC / Vulkan / x264  |   +-------------------+
|   +-- Network transport (UDP+TCP:9757)    |        ^
|   +-- Audio: virtual PulseAudio sink      |        |
+-------------------------------------------+        |
        |  mDNS discovery (Avahi, port 5353)         |
        +--------------------------------------------+
```

### Frame Capture

WiVRn intercepts frames at the OpenXR compositor level inside its Monado fork.
When an OpenXR application calls `xrEndFrame`, the submitted swapchain images
are captured as GPU textures (VkImage/DMA-BUF). This avoids any GPU-to-CPU
readback -- the textures remain in VRAM for the encoding step.

### Encoder Pipeline

WiVRn supports four encoder backends, selected at configuration time:

| Encoder  | Backend       | Codecs              | 10-bit | Notes |
|----------|---------------|---------------------|--------|-------|
| `nvenc`  | NVIDIA NVENC  | H.264, H.265, AV1  | H.265/AV1 | Default on NVIDIA GPUs. Lowest latency HW encoder. |
| `vaapi`  | FFmpeg VA-API | H.264, H.265, AV1  | H.265/AV1 | Default on AMD/Intel. Can target iGPU via device path. |
| `vulkan` | Vulkan Video  | H.264, H.265       | No     | Experimental. Uses VK_KHR_video_encode. Driver support varies. |
| `x264`   | libx264 (CPU) | H.264              | No     | Software fallback. High CPU load, acceptable quality. |

Default selection order: `nvenc` (if NVIDIA + compiled) > `vaapi` (if FFmpeg) > `x264`.

**Encoder grouping**: WiVRn can run multiple encoders in parallel by assigning
them to different groups. Encoders in the same group run sequentially; different
groups run concurrently. This enables splitting the stereo image into regions
(e.g., left eye on one encoder, right eye on another, or foveated center vs
periphery).

**VA-API device override**: On systems with both discrete and integrated GPUs,
the `device` field can point encoding to the iGPU (`/dev/dri/renderD128`) to
offload from the render GPU. Custom FFmpeg options can be passed directly to
`avcodec_open2`.

### Foveated Encoding

WiVRn applies foveated scaling: the center of the image is encoded at 1:1
pixel ratio while the periphery is downscaled. The default stream resolution
is 50% of native, which activates foveation. Setting resolution to 100%
disables foveation for maximum sharpness (relevant for text-heavy content).

### Network Transport

- **Discovery**: Avahi/mDNS on UDP port 5353
- **Data**: UDP + TCP on port 9757 (configurable to TCP-only, at a latency cost)
- **Requirement**: Wired gigabit Ethernet from PC to router; Wi-Fi 5/6/6E from
  router to headset. 5GHz dedicated band strongly recommended.

### NixOS Configuration

```nix
# /etc/nixos/configuration.nix
services.wivrn = {
  enable = true;
  openFirewall = true;      # Opens UDP/TCP 9757 + Avahi
  defaultRuntime = true;     # Sets as active OpenXR runtime
  autoStart = true;          # Starts wivrn-server on login
};

# For NVIDIA GPU encoding:
services.wivrn.package = pkgs.wivrn.override { cudaSupport = true; };

# Additional tools:
environment.systemPackages = with pkgs; [
  wlx-overlay-s     # In-VR desktop overlay
  android-tools      # ADB for wired WiVRn connection
];
```

As of WiVRn v0.23+, OpenComposite paths are managed automatically.

---

## 2. WiVRn Latency Breakdown

### Pipeline Stages

```
App Render -> Capture -> Encode -> Network TX -> Decode -> TimeWarp -> Display
   |            |          |          |             |          |          |
   v            v          v          v             v          v          v
 ~5-15ms     <1ms       2-5ms      1-8ms         2-4ms      <1ms      ~3ms
 (GPU)     (zero-copy)  (HW enc)  (Wi-Fi 6)    (XR2 Gen2) (reproj)  (panel)
```

**Typical end-to-end**: 20-40ms over Wi-Fi 6 with hardware encoding.
**Achievable best case**: ~15-20ms with NVENC, wired USB, and H.264.
**Comfortable VR threshold**: <80ms total motion-to-photon.

### Stage Details

| Stage | Typical | Notes |
|-------|---------|-------|
| Application render | 5-15ms | Depends on scene complexity. For a 2D Emacs buffer composited onto a quad, this should be <2ms. |
| Frame capture | <1ms | Zero-copy DMA-BUF handoff within GPU. No readback. |
| Encode (NVENC) | 2-4ms | Fixed pipeline delay. RTX 4090 measures 3-5ms for NVENC. |
| Encode (VA-API H.265) | 3-6ms | ~3ms extra vs H.264 due to more complex prediction. |
| Network (Wi-Fi 6) | 2-8ms | Depends on interference, distance. Wired USB adds ~1-2ms. |
| Decode (Quest 3 XR2 Gen 2) | 2-4ms | Hardware decode. H.264 with CAVLC may save ~1ms vs CABAC. |
| TimeWarp/Reprojection | <1ms | Asynchronous timewarp on headset corrects for total latency. |
| Panel response | 2-4ms | LCD panel switching time. |

### Text Readability Assessment

For text-heavy workloads (Emacs, terminal emulators), the critical factor is not
latency but **visual quality**. The compression artifacts from video encoding are
the primary obstacle to sharp text. At 30-40ms motion-to-photon, head movement
feels responsive enough for reading, but blocky artifacts from low bitrate or
aggressive foveation destroy legibility.

**Recommendations for text readability via WiVRn**:
- Set resolution scale to **100%** (disables foveation)
- Use **H.265 or AV1** at high bitrate (50+ Mbps)
- Prefer **wired USB** connection to eliminate Wi-Fi jitter
- Consider the `raw` codec for uncompressed streaming over USB (bandwidth
  permitting -- requires ~6 Gbps for Quest 3 resolution)

---

## 3. Codec Comparison for Text-Heavy Content

### The Problem

Video codecs are optimized for natural imagery with smooth gradients. Text is the
adversarial case: high-frequency edges, single-pixel strokes, and high contrast
ratios. Block-based compression (all three codecs use block transforms) smears
these edges, producing ringing and mosquito noise around glyphs.

### Comparison Matrix

| Property | H.264 (AVC) | H.265 (HEVC) | AV1 |
|----------|-------------|---------------|-----|
| Block size | 16x16 fixed macroblocks | 8x8 to 64x64 CTU (adaptive) | 4x4 to 128x128 superblocks (adaptive) |
| Text sharpness at same bitrate | Worst -- fixed blocks cause visible grid on text | Better -- adaptive blocks conform to glyph edges | Best -- smallest blocks + advanced in-loop filtering |
| Compression ratio (vs H.264) | Baseline | ~2x better | ~2.5-3x better |
| HW encode latency | Lowest (~2ms) | ~3ms extra | ~3-5ms extra |
| HW decode (Quest 3) | Yes | Yes | Yes (XR2 Gen 2 only) |
| HW decode (Quest 2) | Yes | Yes | No |
| HW encode (GPU required) | Any modern GPU | Any modern GPU | RTX 4000+ / RX 7000+ / Intel Arc |
| 10-bit support | Rare in HW enc | Yes (VA-API, NVENC) | Yes (VA-API, NVENC) |

### Analysis for Emacs/Terminal Use

**H.264**: At bitrates below ~30 Mbps, text is noticeably soft with visible
macroblock boundaries. The fixed 16x16 block size means thin font strokes (1-2px)
get averaged with surrounding background pixels. Increasing bitrate to 50+ Mbps
helps but wastes bandwidth that H.265/AV1 would use more efficiently.

**H.265**: The adaptive CTU sizing (down to 8x8) significantly improves text
sharpness. At 30 Mbps H.265 roughly matches 50+ Mbps H.264 for text. The 3ms
encode latency penalty is acceptable for a reading-focused workflow. This is the
**recommended default** for text-heavy streaming on current hardware.

**AV1**: Theoretically best for text due to 4x4 minimum block size and superior
in-loop deblocking filters. However, hardware encode support requires latest-gen
GPUs (RTX 4000+, RX 7000+), and encode latency is highest. The Quest 3's XR2
Gen 2 supports AV1 decode, but Quest 2/Pro do not. AV1 is the **future best
choice** once encoder hardware matures.

**Raw (uncompressed)**: WiVRn supports a `raw` codec for zero-loss transmission.
Requires enormous bandwidth (~6+ Gbps for dual 2064x2208 at 90Hz). Only viable
over USB 3.2 Gen 2 or Thunderbolt, not Wi-Fi. Perfect text quality. Worth
investigating for a wired desktop VR workstation.

### Practical Recommendation

For a Smithay compositor targeting Emacs/terminal use:
1. **Default**: H.265 at 50 Mbps, foveation disabled, 100% resolution scale
2. **High-end**: AV1 at 30 Mbps (if RTX 4000+ / RX 7000+ available)
3. **Wired workstation**: Raw codec over USB 3.2 for perfect text
4. **Always**: Disable foveated encoding for text readability

---

## 4. USB-C DisplayPort Alt Mode (Quest Link, Pimax Crystal)

### How It Works

USB-C DisplayPort Alternate Mode repurposes USB-C pins to carry native
DisplayPort signals. This is fundamentally different from streaming:

```
Streaming (WiVRn/ALVR/Quest Link):
  GPU -> Render -> Encode -> USB/WiFi -> Decode -> Display
  Latency: 20-40ms, lossy compression

DisplayPort Alt Mode (Pimax Crystal, native PCVR headsets):
  GPU -> DP 1.4 -> Headset Display Controller -> Display
  Latency: 4-5ms total pipeline, zero compression
```

### Quest Link vs Native DisplayPort

Quest Link over USB is **not** DisplayPort Alt Mode. It uses USB bulk transfers
to send compressed video (H.264/H.265). The Quest 3 via USB Link accumulates
over 19ms pipeline delay due to encoding, USB transmission, and decoding. The
compression artifacts remain.

Native DisplayPort headsets (Pimax Crystal Light, Valve Index, Bigscreen Beyond)
maintain a consistent ~4.8ms total pipeline delay from GPU render completion to
pixel response, with zero compression artifacts.

### How This Differs from DRM Lease

DRM lease is a **protocol mechanism** for granting exclusive display access;
DisplayPort Alt Mode is a **physical transport**. They are complementary:

- **DRM lease** lets a Wayland compositor hand off a DRM connector to Monado/SteamVR
  so the VR runtime can drive the display directly with its own mode-setting,
  buffer management, and timing.
- **DisplayPort** is the cable carrying the signal.

A DP-connected HMD appears as a DRM connector with the `non-desktop` property.
The compositor detects this, avoids placing desktop windows on it, and makes it
available for DRM lease via the `wp_drm_lease_device_v1` protocol.

### Implications for a Smithay Compositor

For wired PCVR headsets, the compositor must:
1. Detect `non-desktop` connectors via DRM enumeration
2. Exclude them from desktop output layout
3. Expose them through the DRM lease protocol
4. Handle lease creation/revocation lifecycle
5. Handle hotplug (connector appears/disappears) gracefully

Smithay provides `DrmLeaseState` and a delegation macro for `wp-drm-lease-v1`.
The `wayland-protocols` crate includes full server-side bindings for the protocol.

---

## 5. Virtual/Simulated HMDs for Testing

### Monado Simulated Driver

Monado includes a built-in simulated driver that provides a virtual HMD and two
controllers without any physical hardware. This is essential for CI/CD and
development on laptops.

**Activation**: Disconnect all real HMDs. Monado falls back to the simulated
driver automatically. Verify by checking stdout for "Simulated HMD" at startup.

**Limitations**:
- Cannot configure resolution, refresh rate, or FOV at runtime
- Simulates fixed two-controller + HMD setup
- No visual output -- frames render to a dummy target
- Cannot test DRM lease paths (no real connector)

### Monado Remote Driver

For more control, the remote driver allows GUI-based manipulation:

```json
// ~/.config/monado/config_v0.json
{
    "active": "remote",
    "remote": {
        "version": 0,
        "port": 4242
    }
}
```

Or set `P_OVERRIDE_ACTIVE_CONFIG=remote`. Then launch `monado-gui` to
interactively adjust HMD position, controller state, etc.

### Headless OpenXR Sessions

Monado supports the `XR_MND_headless` extension, allowing OpenXR sessions
without any display output. As of Monado 25.1.0, headless sessions properly
transition to FOCUSED on `xrBeginSession`, fixing action bindings.

**Use case**: Run OpenXR integration tests in CI without GPU or display.

### NixOS Development Setup

```nix
# For headless VR development/testing
environment.systemPackages = with pkgs; [
  monado
  openxr-loader
];

# Set Monado as active OpenXR runtime
environment.sessionVariables.XR_RUNTIME_JSON =
  "${pkgs.monado}/share/openxr/1/openxr_monado.json";

# Force simulated driver (optional)
environment.sessionVariables.XRT_COMPOSITOR_FORCE_XCB = "1";
```

### Practical Testing Strategy

For a Smithay compositor:
1. **Unit tests**: Headless OpenXR sessions via `XR_MND_headless`
2. **Integration tests**: Simulated driver for frame submission validation
3. **Visual tests**: Remote driver + `monado-gui` for interactive debugging
4. **DRM lease tests**: Requires a real (or virtual) DRM connector; use a
   second monitor with `non-desktop` override for testing lease paths

---

## 6. HMD Hardware: Linux Driver Status

### Tier 1: Full Monado Support

| Headset | Connection | Tracking | Monado Status | Notes |
|---------|-----------|----------|---------------|-------|
| Valve Index | DP 1.2 + USB 3 | Lighthouse 2.0 | Full support | Reference device for Linux VR. Audio via DP. May need power replug on first connect. |
| HTC Vive/Pro/Pro Eye | HDMI/DP + USB | Lighthouse 1.0/2.0 | Full support | Pro mic needs 44.1kHz. HDMI audio device created. |
| Oculus Rift CV1/S | HDMI/DP + USB | Constellation/Inside-out | Supported | Legacy but functional. |
| WMR headsets | HDMI + USB | Inside-out (6DoF) | Supported | Experimental 6DoF controllers. HP Reverb G1/G2 included. |

### Tier 2: Works with Patches

| Headset | Connection | Monado Status | Blockers |
|---------|-----------|---------------|----------|
| Vive Pro 2 | DP + USB | Supported (AMD; NVIDIA needs driver 580+ kernel patches) | NVIDIA direct mode requires kernel patches |
| Bigscreen Beyond 1/2/e | DP + USB | Supported (kernel patch required) | Needs AMDGPU kernel patch. Beyond 2e works with Monado. Does NOT work on NVIDIA. Use `bsb2e_linux` fork of Baballonia for eye tracking. |
| Pimax 5K/8K/Vision series | DP + USB | WIP | Requires Coreforge's Monado branch. EDID patching may be needed on some systems. Udev rules required for HID access. |

### Tier 3: Limited or No Support

| Headset | Connection | Status | Notes |
|---------|-----------|--------|-------|
| Pimax Crystal/Light/Super | DP + USB | Protocols not reverse-engineered | The Crystal's inside-out tracking protocol is proprietary and uncracked. Display output may work via generic DP, but no tracking. |
| Somnium VR1 | DP + USB | "Rumoured successful internal test" | Uses Lighthouse tracking (good for Linux). Linux support officially "in development". High-res (2880x2880/eye), 125deg FOV. |
| Meta Quest (standalone) | Wi-Fi / USB streaming | Via WiVRn/ALVR only | No native DP output. Streaming only. |

### Monado 25.1.0 New Drivers

The December 2025 release added:
- **Blubur S1**: New driver
- **Rift DK2**: New driver (legacy support)
- **SolarXR IPC**: New driver
- **Improved**: Razer Hydra, Vive Pro 2, Xreal Air 2 Ultra, Fujitsu headset
- **Compositor**: Per-scanline timewarp for rolling-scanout displays

### Monado SLAM/Inside-Out Tracking

Monado integrates open-source SLAM backends (Basalt, ORB-SLAM3, Kimera-VIO) for
inside-out tracking on devices with cameras + IMU. Currently supported:
- WMR headsets (via WMR driver)
- RealSense-based devices
- Devices using Monado's generic camera tracking pipeline

Not yet supported: Pimax Crystal SLAM, Quest inside-out (proprietary).

---

## 7. DRM Lease vs Overlay Approach

### DRM Lease (Direct Mode)

**How it works**: The Wayland compositor (DRM master) leases a DRM connector,
CRTC, and planes to a VR client via the `wp-drm-lease-v1` protocol. The client
gets a DRM fd and drives the display directly -- mode-setting, buffer submission,
and vsync are all handled by the VR runtime (Monado/SteamVR).

Source: https://drewdevault.com/2019/08/09/DRM-leasing-and-VR-for-Wayland.html

```
Compositor                          VR Runtime (Monado)
    |                                    |
    | -- wp_drm_lease_connector_v1 -->   |
    |    (advertise non-desktop HMD)     |
    |                                    |
    | <-- wp_drm_lease_request_v1 --     |
    |    (request lease)                 |
    |                                    |
    | -- lease_fd (DRM fd) ---------->   |
    |    (connector appears "destroyed"  |
    |     to compositor)                 |
    |                                    |
    |    VR runtime drives display       |
    |    directly via DRM ioctls         |
    |                                    |
    | -- finished (on hotunplug) ------>  |
```

**Vulkan integration**: The `VK_EXT_acquire_wl_display` extension bridges Vulkan
WSI with DRM leasing. Monado's "wayland direct" target uses this path.

**Tradeoffs**:
- (+) Lowest latency -- VR runtime has direct hardware access
- (+) Full control over refresh rate, mode-setting, reprojection
- (+) Well-defined Wayland protocol with hotplug support
- (-) Only works with wired DP/HDMI headsets (not streaming)
- (-) Compositor loses all knowledge of what's on the HMD
- (-) Cannot composite 2D Wayland windows into VR scene

### Overlay Approach (XR_EXTX_overlay)

**How it works**: Multiple OpenXR applications share the same VR scene. One app
is the "main" session (e.g., a game); overlay apps composite additional layers
(e.g., desktop windows, notifications) on top.

Source: https://www.collabora.com/news-and-blog/news-and-events/monado-multi-application-support-with-xr-extx-overlay.html

```
Main OpenXR App          Overlay App (desktop windows)
    |                         |
    v                         v
  XrCompositionLayer      XrCompositionLayerProjection
  Projection              (with XrSessionCreateInfoOverlayEXTX)
    |                         |
    +-----------+-------------+
                |
          Monado Compositor
          (out-of-process, multi-layer)
                |
                v
          HMD Display
```

**Tradeoffs**:
- (+) Multiple apps share the VR scene simultaneously
- (+) Desktop windows can appear alongside VR content
- (-) Experimental extension (EXTX prefix)
- (-) Requires Monado's out-of-process compositor (merged in v0.2+)
- (-) Overlay app cannot control the base session's rendering
- (-) Added composition latency from multi-layer merge

### WayVR: Smithay-Based VR Wayland Server

Source: https://github.com/wlx-team/wayvr

WayVR runs a Smithay-powered Wayland compositor that renders client windows into
textures, then presents them as panels in VR via OpenXR or OpenVR overlay APIs.

- Supports Monado, WiVRn, and SteamVR as backends
- Wayland clients render normally; WayVR captures their buffers
- Panels are positioned in 3D space via wlx-overlay-s
- Multi-display support for launching apps directly in VR

**Relevance to our compositor**: WayVR demonstrates the pattern we would follow:
Smithay compositor captures client buffers as textures, submits them to OpenXR
as composition layers. The key difference is WayVR runs as an overlay on top of
another VR session, while our compositor would be the primary XR session.

### Stardust XR: Modular XR Display Server

Source: https://stardustxr.org/docs/dive-deeper/deep-overview

Stardust XR is a spatial display server with a built-in Wayland compositor. It
uses a scenegraph architecture over Unix domain sockets with FlatBuffers IPC.
Currently uses Bevy/Rust for its reference server implementation.

- Can run as overlay (alongside wlx-overlay-s) or standalone
- Wayland compositor integration via `stardust-xr-flatland`
- Spatial Universal Interaction System (SUIS) for 3D input routing

**Relevance**: Stardust XR's modular approach (separate processes for compositor,
launcher, environment) is architecturally interesting but adds IPC overhead. A
monolithic Smithay compositor with integrated XR rendering avoids this.

### WXRD: wlroots-Based VR Compositor

Source: https://www.collabora.com/news-and-blog/news-and-events/wxrd-a-standalone-wayland-compositor-for-xrdesktop.html

WXRD (Collabora) is a standalone wlroots-based Wayland compositor for VR. Windows
render at the HMD's native refresh rate with pixel density tuned for VR rather
than a monitor. Uses gulkan (Vulkan abstraction) + gxr (XR runtime abstraction)
+ xrdesktop (3D window management).

**Key insight**: WXRD renders at HMD-native pixel density. When no desktop
monitor is connected, there is no reason to match a monitor's DPI -- fonts should
be rendered at the PPD (pixels per degree) of the HMD.

### Summary Table

| Approach | Latency | Text Quality | Multi-App | Streaming | Complexity |
|----------|---------|-------------|-----------|-----------|------------|
| DRM Lease | Best | Best (no compression) | No | No | Medium |
| XR_EXTX_overlay | Good | Good (direct render) | Yes | No | High |
| WayVR overlay | Good | Depends on backend | Yes | Yes (via WiVRn) | Medium |
| WiVRn streaming | Moderate | Codec-dependent | N/A | Yes | Low |
| Stardust XR | Good | Good | Yes | Partial | High |
| WXRD standalone | Best | Best | Limited | No | High |

---

## 8. HMD Hotplug Handling in Wayland Compositors

### The DRM Lease Hotplug Protocol

Source: https://wayland.app/protocols/drm-lease-v1

The `wp-drm-lease-v1` protocol handles hotplug through a well-defined event
sequence:

**On HMD connect (hotplug in)**:
1. DRM subsystem detects new connector via udev event
2. Compositor reads connector properties, checks `non-desktop` flag
3. Compositor sends `wp_drm_lease_connector_v1` event to bound clients
4. Compositor sends `wp_drm_lease_device_v1.done` to signal enumeration complete

**On HMD disconnect (hotplug out)**:
1. DRM subsystem detects connector removal via udev event
2. If connector is leased: compositor sends `wp_drm_lease_v1.finished` to
   revoke the lease
3. If connector is not leased: compositor sends
   `wp_drm_lease_connector_v1.withdrawn`
4. Compositor sends `wp_drm_lease_device_v1.done`

**On compositor losing DRM master** (e.g., VT switch):
1. All active leases must be revoked via `finished` events
2. All connectors should be sent `withdrawn` events

### Implementation in Smithay

Smithay's `DrmLeaseState` handles the protocol-level state machine. The
compositor must integrate this with its DRM backend's hotplug detection:

```rust
// Pseudocode for hotplug handling in a Smithay compositor
fn handle_drm_event(&mut self, event: DrmEvent) {
    match event {
        DrmEvent::ConnectorConnected(connector) => {
            let props = self.drm.get_connector_props(connector);
            if props.non_desktop {
                // Don't add to desktop output layout
                // Instead, advertise via DRM lease protocol
                self.drm_lease_state.add_connector(connector);
            } else {
                self.add_output(connector);
            }
        }
        DrmEvent::ConnectorDisconnected(connector) => {
            if self.drm_lease_state.is_leased(connector) {
                // Revoke active lease -- sends finished event
                self.drm_lease_state.revoke_lease(connector);
            }
            self.drm_lease_state.remove_connector(connector);
        }
    }
}
```

### Known Pitfalls

1. **Valve Index first-connect**: The Index may enter a bad state on first
   connection or when plugged in before boot, causing DRM lease failures. A
   power-cycle of the headset resolves this.

2. **AMDVLK incompatibility**: AMDVLK (the proprietary AMD Vulkan driver) cannot
   perform DRM leases and causes video corruption with WiVRn. Use RADV (Mesa's
   AMD Vulkan driver) instead. On NixOS, this is the default.

3. **Race conditions**: udev connector events may arrive before the DRM device is
   fully initialized. Add a small delay or retry loop when reading connector
   properties after hotplug.

4. **Lease persistence across VT switch**: When the compositor loses DRM master
   (VT switch, logind session deactivation), all leases must be revoked. When
   regaining DRM master, connectors should be re-advertised.

5. **Streaming headsets don't hotplug**: WiVRn/ALVR headsets connect over the
   network, not DRM. Hotplug handling for these requires monitoring the WiVRn
   server's connection state separately.

### Recommended Architecture

```
udev DRM events
    |
    v
Smithay DRM Backend
    |
    +-- non_desktop connector? --> DrmLeaseState (wp-drm-lease-v1)
    |                                  |
    |                                  +--> Monado (wayland direct target)
    |                                  |       |
    |                                  |       v
    |                                  |    VR rendering pipeline
    |                                  |
    |                                  +--> Lease revocation on disconnect
    |
    +-- regular connector? --> Output management (desktop displays)

WiVRn/network HMDs (separate path):
    Avahi mDNS discovery
        |
        v
    WiVRn server (Monado fork)
        |
        v
    OpenXR streaming pipeline
```

---

## Summary: Recommended Path for Our Compositor

For a Smithay-based VR Wayland compositor on NixOS targeting text-heavy
workloads (Emacs, terminals):

1. **Primary HMD path**: DRM lease for wired DP headsets (Index, Beyond 2e).
   Implement `wp-drm-lease-v1` using Smithay's `DrmLeaseState`. This gives the
   lowest latency and best text quality (zero compression).

2. **Streaming path**: WiVRn for Quest/Pico standalone headsets. Use H.265 at
   50+ Mbps with foveation disabled. Enable via NixOS `services.wivrn`. Consider
   raw codec over USB for workstation setups.

3. **Development/CI**: Monado simulated driver + headless OpenXR sessions. No
   hardware needed. Remote driver + `monado-gui` for interactive testing.

4. **Desktop overlay**: WayVR pattern -- capture Wayland client buffers as
   textures, submit as OpenXR composition layers. Our compositor can do this
   natively without a separate overlay process.

5. **Hotplug**: Implement full DRM connector hotplug handling with `non-desktop`
   detection. Revoke leases on disconnect. Handle Valve Index first-connect
   quirk. Use RADV, not AMDVLK.

6. **Future headsets**: Watch Somnium VR1 (Lighthouse tracking = good Linux
   story), Pimax Crystal (blocked on protocol RE), Beyond 2e (works with patches
   today).
