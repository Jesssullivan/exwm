# DRM Lease Implementation Survey for VR Headset Display Management

Research for a Smithay-based compositor implementing `wp_drm_lease_v1` to support
VR headsets (SteamVR, Monado, WiVRn).

---

## 1. wlroots/Sway: `wlr_drm_lease_v1_manager`

### Architecture

wlroots implements DRM leasing in two layers:

- **Protocol layer**: `types/wlr_drm_lease_v1.c` implements the `wp_drm_lease_v1`
  Wayland protocol, managing client-facing lease device globals, connector
  advertisements, and lease request lifecycle.
- **Backend layer**: `backend/drm/drm.c` contains `wlr_drm_create_lease()`, which
  enumerates kernel DRM objects and calls `drmModeCreateLease()`.

### Sway's non-desktop detection and lease offering

From `sway/desktop/output.c` and `sway/server.c`:

```c
// server.c -- initialization
server->drm_lease_manager = wlr_drm_lease_manager_v1_create(
    server->wl_display, server->backend);
// Signal handler auto-grants lease requests for non-desktop outputs

// output.c -- connector detection
if (wlr_output->non_desktop) {
    sway_log(SWAY_DEBUG, "Not configuring non-desktop output");
    if (server->drm_lease_manager) {
        wlr_drm_lease_v1_manager_offer_output(
            server->drm_lease_manager, wlr_output);
    }
    // Store in root->non_desktop_outputs, skip desktop rendering
    return;
}
```

Key behavior: non-desktop outputs are stored in `sway_output_non_desktop` structs
rather than `sway_output`. They never enter the rendering pipeline.

### Object enumeration in `wlr_drm_create_lease()`

From `backend/drm/drm.c`:

```c
int wlr_drm_create_lease(struct wlr_output **outputs, size_t n_outputs,
        uint32_t *lessee_id) {
    struct wlr_drm_backend *drm =
        get_drm_backend_from_backend(outputs[0]->backend);

    int n_objects = 0;
    uint32_t objects[4 * n_outputs + 1];

    for (size_t i = 0; i < n_outputs; ++i) {
        struct wlr_drm_connector *conn =
            get_drm_connector_from_output(outputs[i]);
        assert(conn->lessee_id == 0);

        objects[n_objects++] = conn->id;           // connector
        if (!conn->crtc) {
            wlr_log(WLR_ERROR, "Connector has no CRTC");
            return -1;
        }
        objects[n_objects++] = conn->crtc->id;     // CRTC
        objects[n_objects++] = conn->crtc->primary->id;  // primary plane

        if (conn->crtc->cursor) {
            objects[n_objects++] = conn->crtc->cursor->id; // cursor plane
        }
    }

    int lease_fd = drmModeCreateLease(drm->fd, objects, n_objects, 0,
        lessee_id);
    if (lease_fd < 0) return lease_fd;

    // Mark connector and CRTC as leased
    for (size_t i = 0; i < n_outputs; ++i) {
        struct wlr_drm_connector *conn =
            get_drm_connector_from_output(outputs[i]);
        conn->lessee_id = *lessee_id;
        conn->crtc->lessee_id = *lessee_id;
    }
    return lease_fd;
}
```

Objects included per output: **connector + CRTC + primary plane + cursor plane
(if present)**. Note: overlay planes are NOT included in wlroots' implementation.

### Lease lifecycle

- Leased connectors appear "destroyed" to the compositor (mimics hotplug disconnect)
- Revocation via `wlr_drm_lease_manager_v1_revoke_lease()` calls
  `drmModeRevokeLease()` and closes the fd
- On lease termination, outputs are returned to compositor control

---

## 2. KDE KWin DRM Lease

### Implementation location

KWin's DRM lease code spans:
- `src/backends/drm/drm_backend.cpp` -- non-desktop output detection
- `src/backends/drm/drm_output.cpp` -- `addLeaseObjects` function
- `src/backends/drm/drm_pipeline.cpp` -- atomic plane support for leased outputs

### Non-desktop detection

KWin checks the `isNonDesktop()` property on DRM connector objects during output
enumeration. Non-desktop connectors are excluded from desktop layout and made
available for leasing.

### CRTC allocation bug (Bug 487938)

A well-documented pitfall in KWin: the Valve Index VR headset could not be leased
with error:

```
kwin_wayland_drm: Can't lease connector: No suitable crtc available
```

**Root cause**: CRTC resources were already claimed by desktop outputs. When the
compositor assigns CRTCs to desktop outputs first, the non-desktop connector may
find no compatible CRTC remaining. This requires either:
- Reserving a CRTC for the non-desktop connector during initial enumeration
- Re-allocating CRTCs when a lease is requested (expensive, may flicker)
- The user unplugging/replugging the HMD to trigger re-enumeration

### Atomic plane support

KWin's DRM leasing MR (#662) required adding atomic plane support for
`DrmLeaseOutput` objects. Without this, lessees cannot use atomic modesetting
(`drmModeAtomicCommit`) and are limited to legacy mode-setting, which SteamVR
and Monado do not expect.

### Protocol version

KDE Plasma 5.24+ supports `wp_drm_lease_device_v1` on Wayland. Works with AMD
GPUs. NVIDIA support is broken -- the HMD is properly leased but SteamVR errors
out due to Vulkan/driver incompatibilities.

---

## 3. Monado: `comp_window_direct_wayland.c`

### Architecture

Monado's Wayland direct mode is a **DRM lease consumer** (client side). The
implementation lives in `compositor/main/comp_window_direct_wayland.c`.

### Data structures

```c
struct direct_wayland_lease {
    // Lease state and DRM file descriptor
};

struct direct_wayland_lease_connector {
    char *name;
    uint32_t id;
    char *description;
};

struct direct_wayland_lease_device {
    // DRM device abstraction with connector list
};

struct comp_window_direct_wayland {
    // Main compositor window wrapping Wayland integration
};
```

### Protocol flow (client side)

1. **Registry discovery**: Monado binds to `wp_drm_lease_device_v1` globals via
   `wl_registry` listener.

2. **drm_fd event**: Compositor sends a non-master DRM fd for querying device
   properties (modes, EDID).

3. **Connector enumeration**: `drm_lease_device_listener` receives connector
   events. Each connector has name, description, and `connector_id` (the DRM
   object ID). Monado stores these in `direct_wayland_lease_connector`.

4. **Connector selection**: `get_named_connector_or_first()` selects the target
   HMD by name or falls back to the first available.

5. **Lease request**: Creates `wp_drm_lease_request_v1`, adds desired connector(s)
   via `request_connector()`, then calls `submit()`.

6. **Lease fd**: `_lease_fd` callback receives the DRM master fd with leased
   resources. Monado then:
   - Imports the fd for Vulkan display acquisition
   - Uses `VK_EXT_acquire_drm_display` or `VK_KHR_display` + `VK_KHR_display_swapchain`
   - Creates Vulkan swapchain for rendering directly to the HMD

7. **Cleanup**: `_lease_finished` handles lease revocation or rejection.

### Display detection hierarchy

Monado uses multiple strategies depending on the environment:

| Target | Env Variable | Detection Method |
|--------|-------------|-----------------|
| Wayland direct | (default on Wayland) | `wp_drm_lease_v1` protocol |
| X11 direct | (default on X11) | `non-desktop` randr property + `VK_EXT_acquire_xlib_display` |
| NVIDIA direct | `XRT_COMPOSITOR_FORCE_NVIDIA_DISPLAY` | `VK_KHR_display` (hardcoded allowlist) |
| KMS/VT | `XRT_COMPOSITOR_FORCE_VK_DISPLAY=N` | `VK_KHR_display` (requires DRM master, text VT) |
| Wayland window | `XRT_COMPOSITOR_FORCE_WAYLAND=1` | Normal Wayland surface (fallback) |
| X11 window | `XRT_COMPOSITOR_FORCE_XCB=1` | Normal X11 window (fallback) |

### NVIDIA special path

NVIDIA drivers hide non-desktop connectors entirely from X11/Wayland. They appear
only via `VK_KHR_display`. Monado maintains its own NVIDIA HMD allowlist,
extensible via `XRT_COMPOSITOR_FORCE_NVIDIA_DISPLAY="Display Name"`.

---

## 4. Gamescope DRM Lease Handling

### Current state

Gamescope (Valve's SteamOS compositor) is built on wlroots and inherits its DRM
lease infrastructure. However, Gamescope's primary VR integration path is through
the **OpenVR backend**, not DRM leasing:

- **OpenVR Backend**: Renders gamescope's composited output to SteamVR via the
  OpenVR API. Gamescope acts as a VR overlay manager.
- **DRM Backend**: When running as the sole compositor (Steam Deck mode), gamescope
  controls DRM directly but does not implement DRM lease serving for external VR
  runtimes.

### Wayland server (`wlserver.cpp`)

The Wayland server implementation (`src/wlserver.cpp`) creates a basic DRM
interface for XWayland compatibility:

```c
wlr_drm_create(wlserver.display, wlserver.wlr.renderer);
```

But does NOT create a `wlr_drm_lease_v1_manager`. As of the examined codebase,
gamescope does not serve DRM leases to client applications.

### Valve's approach

Valve's strategy for SteamVR on Linux relies on:
1. The **host compositor** (Sway, KWin, GNOME) providing DRM lease via
   `wp_drm_lease_v1`
2. SteamVR/Monado acquiring the lease as a Wayland client
3. Gamescope optionally compositing game windows before they reach SteamVR

This means a Smithay-based compositor serving DRM leases is the correct
architecture -- gamescope is not the lease provider.

---

## 5. Common Pitfalls

### Missing CRTC/plane in lease object set

**The most critical pitfall.** The lease object set passed to
`drmModeCreateLease()` must include ALL DRM objects the lessee needs:

| Object | Required? | Consequence if missing |
|--------|-----------|----------------------|
| Connector | Yes | Lessee cannot find any output |
| CRTC | Yes | Lessee cannot drive the display |
| Primary plane | Yes | Lessee cannot scanout framebuffers |
| Cursor plane | No | Lessee has no hardware cursor (acceptable for VR) |
| Overlay planes | No | Lessee cannot use multi-plane composition |

**wlroots pattern** (correct minimum set):
```
connector + CRTC + primary_plane [+ cursor_plane]
```

**Common failure mode**: Forgetting the primary plane. The connector and CRTC alone
are insufficient -- without a primary plane, the lessee cannot perform page flips
or atomic commits. VLC's DRM lease module encountered this exact bug.

### CRTC availability

CRTCs are a scarce resource. A GPU typically has fewer CRTCs than connectors.
If the compositor claims all CRTCs for desktop outputs, none remain for leasing.

**Mitigation strategies**:
1. **Reserve CRTCs for non-desktop connectors during initial CRTC allocation.**
   When scanning connectors, assign CRTCs to non-desktop connectors first (or
   at least ensure one compatible CRTC remains free).
2. **Re-allocate CRTCs on lease request.** Expensive -- may require disabling
   a desktop output temporarily.
3. **Fail gracefully.** Return `LeaseRejected` with a clear error message.

KWin Bug 487938 (Valve Index) is the canonical example of this failure.

### DRM master vs lease fd permissions

| Capability | DRM Master fd | Lease fd | Render node fd |
|-----------|--------------|---------|---------------|
| Mode setting | All connectors | Leased objects only | No |
| Page flip | All CRTCs | Leased CRTCs only | No |
| Atomic commit | Full device | Leased objects only | No |
| Buffer allocation | Yes | Yes | Yes |
| Create sub-lease | Yes | **No** (depth limited to 1) | No |
| See other masters' connectors | Yes | **No** (appear disconnected) | N/A |
| See other masters' CRTCs | Yes | **No** (masked out) | N/A |

**Key restriction**: Lease tree depth is limited to 1. A lessee cannot create
sub-leases. This is a kernel-level restriction.

**Resource isolation**: Objects not controlled by a lessee appear idle/unusable:
- Connectors report as "disconnected"
- Encoders report no possible CRTCs or clones
- CRTCs and planes are hidden from enumeration

### CRTC index mapping

Lessees see a subset of the device's CRTCs, but kernel APIs sometimes expect
global CRTC indices. From Keith Packard's blog:

> "For leases, figuring out the index into the kernel list of crtcs is pretty
> tricky -- our lease has a subset of those crtcs, so we can't actually compute
> the global crtc index."

This affects `drmCrtcGetSequence()` and `drmCrtcQueueSequence()`. Lessees must
use the CRTC ID directly rather than computing indices.

### Thread safety

Simultaneous page flips to the same DRM device from different masters (lessor +
lessee) can trigger thread-safety issues in some kernel/driver combinations.
Early testing with kmscube revealed this.

---

## 6. Non-Desktop Connector Detection

### Kernel-level: EDID quirk table

The Linux kernel maintains an EDID quirk table in `drivers/gpu/drm/drm_edid.c`.
Devices matching specific vendor/product IDs are tagged with
`EDID_QUIRK_NON_DESKTOP`, which sets the `non-desktop` DRM connector property.

**Quirked devices** (as of Linux 6.x):

| Vendor | Devices | EDID IDs |
|--------|---------|----------|
| HTC | Vive, Vive Pro | HVR-aa01, HVR-aa02 |
| Oculus | Rift DK1/DK2/CV1/Rift S | OVR-0001/0003/0004/0012 |
| Windows MR | Various | ACR-7fce, LEN-0408, FUJ-1970, DEL-7fce, SEC-144a, AUS-c102 |
| Sony | PSVR | SNY-0704 |
| Sensics | Various | SEN-1019 |
| OSVR | HDK, HDK2 | SVR-1019, AUO-1111 |
| Valve | Index | (quirked separately) |
| Bigscreen | Beyond | (recent addition) |
| Pimax | Various | (recent addition) |

### Connector property query

The `non-desktop` property is a standard DRM connector property (boolean).

**In Smithay/drm-rs** (from Anvil):

```rust
let non_desktop = drm_device
    .get_properties(connector.handle())
    .ok()
    .and_then(|props| {
        let (info, value) = props
            .into_iter()
            .filter_map(|(handle, value)| {
                let info = drm_device.get_property(handle).ok()?;
                Some((info, value))
            })
            .find(|(info, _)| info.name().to_str() == Ok("non-desktop"))?;
        info.value_type().convert_value(value).as_boolean()
    })
    .unwrap_or(false);
```

**In wlroots** (C): The `wlr_output.non_desktop` field is populated automatically
by the DRM backend when scanning connectors.

### Fallback: manual detection

For headsets not in the kernel quirk table, compositors can:
1. Match on EDID vendor/product manually
2. Check for unusual resolutions (e.g. 2160x1200 for Vive, 1832x1920 per eye for Quest)
3. Allow user configuration to force a connector as non-desktop

---

## 7. WiVRn Network Streaming Architecture

### How WiVRn avoids DRM lease

WiVRn takes a fundamentally different approach from SteamVR/Monado direct mode.
Instead of leasing a physical display, WiVRn:

1. **Runs as an OpenXR runtime** on the host PC (built on Monado)
2. **Renders to GPU framebuffers** using normal Vulkan rendering (no physical HMD
   connected to the host)
3. **Encodes frames** using hardware video encoders (VAAPI/FFmpeg for AMD/Intel,
   NVENC for NVIDIA, x264 software fallback)
4. **Streams encoded video** over the network to the standalone headset
5. **Receives tracking data** from the headset over the network

### Network protocol

- **Ports**: TCP+UDP 9757 (primary), UDP 5353 (mDNS/Avahi discovery)
- **Codec support**: H.264, H.265, AV1; 8-bit and 10-bit
- **Transport**: Low-latency UDP for video, TCP for control
- **USB tethering**: `adb reverse tcp:9757 tcp:9757` for wired mode

### Why DRM lease is irrelevant

WiVRn's host PC has **no physical HMD connector**. The standalone headset (Quest,
Pico, etc.) has its own display driven by its own SoC. The host PC only needs:
- GPU access for rendering (normal Vulkan)
- Video encoder access (VAAPI/NVENC)
- Network access

This means a compositor serving WiVRn does NOT need DRM lease support. WiVRn
works with any compositor that provides a functioning Vulkan stack. The compositor
does not need to detect non-desktop connectors or manage HMD outputs.

### Encoding pipeline

```
OpenXR app --> Vulkan render --> GPU framebuffer
    --> Hardware encode (VAAPI/NVENC/x264)
    --> Network transport (UDP)
    --> Standalone headset decode + display
```

Multiple encoders can run in parallel (visible in WiVRn's stats page as multiple
encoder timing graphs), allowing split encoding of left/right eye or
tiled encoding for latency reduction.

---

## 8. Desktop Fallback When No HMD Available

### Strategy 1: Monado window mode (recommended for development)

When no non-desktop connector is found, Monado falls back to rendering in a
regular window:

| Fallback | Env Variable | Description |
|----------|-------------|-------------|
| X11 window | `XRT_COMPOSITOR_FORCE_XCB=1` | Renders to an X11 window |
| Wayland window | `XRT_COMPOSITOR_FORCE_WAYLAND=1` | Renders to a Wayland surface |
| X11 fullscreen | `XRT_COMPOSITOR_XCB_FULLSCREEN=1` | Fullscreen X11 window |

This is the standard developer/testing path. The compositor does not need to do
anything special -- just serve normal Wayland/X11 surfaces.

### Strategy 2: Simulated non-desktop output

For testing DRM lease without hardware, a compositor can:
1. Create a virtual output
2. Mark it as non-desktop
3. Offer it for leasing

This allows testing the lease protocol flow without a physical HMD.

### Strategy 3: Graceful protocol presence

The compositor should **always advertise `wp_drm_lease_device_v1`** globals for
each DRM node, even if no non-desktop connectors are detected. The protocol
handles this naturally:

```
Client binds wp_drm_lease_device_v1
  --> Compositor sends drm_fd
  --> Compositor sends done (zero connectors)
  --> Client sees no connectors, falls back to window mode
```

If a headset is later hot-plugged:
```
Compositor detects non-desktop connector
  --> Sends connector event to all bound clients
  --> Sends done
  --> Client can now request a lease
```

### Strategy 4: WiVRn-style streaming (no HMD at all)

If the use case is wireless VR only, no DRM lease infrastructure is needed.
WiVRn handles everything through standard Vulkan rendering + network streaming.

---

## Smithay Implementation Recommendations

### Initialization

```rust
// During device setup, create lease state for each DRM node
let leasing_global = DrmLeaseState::new::<MyState>(
    &display_handle,
    &drm_node,
).ok();
```

### Non-desktop connector detection and offering

```rust
fn connector_connected(&mut self, node: DrmNode, connector: connector::Info,
                        crtc: crtc::Handle) {
    let drm = self.drm_device();

    let non_desktop = drm.get_properties(connector.handle())
        .ok()
        .and_then(|props| {
            let (info, value) = props.into_iter()
                .filter_map(|(h, v)| drm.get_property(h).ok().map(|i| (i, v)))
                .find(|(info, _)| info.name().to_str() == Ok("non-desktop"))?;
            info.value_type().convert_value(value).as_boolean()
        })
        .unwrap_or(false);

    if non_desktop {
        self.non_desktop_connectors.push((connector.handle(), crtc));
        if let Some(lease_state) = self.leasing_global.as_mut() {
            lease_state.add_connector::<MyState>(
                connector.handle(),
                output_name,
                format!("{make} {model}"),
            );
        }
        return; // Do NOT create a desktop output
    }
    // ... normal desktop output setup
}
```

### Lease request handler (critical path)

```rust
impl DrmLeaseHandler for MyState {
    fn drm_lease_state(&mut self, node: DrmNode) -> &mut DrmLeaseState {
        self.backends.get_mut(&node).unwrap()
            .leasing_global.as_mut().unwrap()
    }

    fn lease_request(
        &mut self,
        node: DrmNode,
        request: DrmLeaseRequest,
    ) -> Result<DrmLeaseBuilder, LeaseRejected> {
        let backend = self.backends.get(&node)
            .ok_or(LeaseRejected::default())?;
        let drm = backend.drm_device();
        let mut builder = DrmLeaseBuilder::new(drm);

        for conn in request.connectors {
            // ONLY lease non-desktop connectors
            if let Some((_, crtc)) = backend.non_desktop_connectors
                .iter()
                .find(|(handle, _)| *handle == conn)
            {
                builder.add_connector(conn);
                builder.add_crtc(*crtc);

                // MUST include primary plane -- this is the #1 pitfall
                let planes = drm.planes(crtc)
                    .map_err(LeaseRejected::with_cause)?;

                let (primary, claim) = planes.primary.iter()
                    .find_map(|p| {
                        drm.claim_plane(p.handle, *crtc)
                            .map(|c| (p, c))
                    })
                    .ok_or_else(LeaseRejected::default)?;
                builder.add_plane(primary.handle, claim);

                // Cursor plane is optional but nice to have
                if let Some((cursor, claim)) = planes.cursor.iter()
                    .find_map(|p| {
                        drm.claim_plane(p.handle, *crtc)
                            .map(|c| (p, c))
                    })
                {
                    builder.add_plane(cursor.handle, claim);
                }
            } else {
                warn!("Lease requested for desktop connector, denying");
                return Err(LeaseRejected::default());
            }
        }
        Ok(builder)
    }

    fn new_active_lease(&mut self, node: DrmNode, lease: DrmLease) {
        self.backends.get_mut(&node).unwrap()
            .active_leases.push(lease);
    }

    fn lease_destroyed(&mut self, node: DrmNode, lease: u32) {
        self.backends.get_mut(&node).unwrap()
            .active_leases.retain(|l| l.id() != lease);
    }
}
```

### Session suspend/resume

```rust
// On session suspend (e.g., VT switch)
if let Some(lease_global) = backend.leasing_global.as_mut() {
    lease_global.suspend();
}

// On session resume
if let Some(lease_global) = backend.leasing_global.as_mut() {
    lease_global.resume::<MyState>();
}
```

### Checklist for implementation

- [ ] Create `DrmLeaseState` per DRM node during device init
- [ ] Query `non-desktop` property on every connector during scan
- [ ] Store non-desktop connectors separately (never create desktop outputs)
- [ ] Offer non-desktop connectors via `lease_state.add_connector()`
- [ ] In `lease_request`: validate connector is non-desktop
- [ ] In `lease_request`: add connector + CRTC + primary plane (minimum)
- [ ] In `lease_request`: claim planes via `drm.claim_plane()` before adding
- [ ] Track active leases for cleanup
- [ ] Handle lease destruction (release CRTC/planes back to compositor)
- [ ] Handle session suspend/resume for lease state
- [ ] Handle hot-plug: new non-desktop connector -> advertise to clients
- [ ] Handle hot-unplug: withdraw connector from lease state
- [ ] Reserve CRTC for non-desktop connectors during initial allocation
- [ ] Always advertise `wp_drm_lease_device_v1` even with zero connectors

---

## References

- [DRM Lease Protocol Specification (Wayland Explorer)](https://wayland.app/protocols/drm-lease-v1)
- [Drew DeVault: DRM leasing: VR for Wayland](https://drewdevault.com/2019/08/09/DRM-leasing-and-VR-for-Wayland.html)
- [Keith Packard: DRM-lease kernel API](https://keithp.com/blogs/DRM-lease/)
- [Keith Packard: DRM-lease-4 (frame sequence)](https://keithp.com/blogs/DRM-lease-4/)
- [Linux Kernel DRM UAPI Documentation](https://docs.kernel.org/gpu/drm-uapi.html)
- [wlroots DRM lease implementation](https://github.com/swaywm/wlroots/blob/master/backend/drm/drm.c)
- [wlroots lease protocol types](https://github.com/swaywm/wlroots/blob/master/types/wlr_drm_lease_v1.c)
- [Sway DRM lease commit](https://github.com/swaywm/sway/commit/a2dd9830733f81127e3ff716a72d26a223ea0207)
- [KWin DRM leasing MR](https://invent.kde.org/plasma/kwin/-/merge_requests/662)
- [KWin Bug 487938: CRTC availability](https://www.mail-archive.com/kde-bugs-dist@kde.org/msg930452.html)
- [Monado Direct Mode Documentation](https://monado.freedesktop.org/direct-mode.html)
- [Monado comp_window_direct_wayland.c API](https://monado.pages.freedesktop.org/monado/comp__window__direct__wayland_8c.html)
- [Smithay DRM Lease Module](https://smithay.github.io/smithay/smithay/wayland/drm_lease/index.html)
- [Smithay Anvil Example (udev.rs)](https://github.com/Smithay/smithay/blob/master/anvil/src/udev.rs)
- [Smithay wayland-protocols DRM lease](https://smithay.github.io/wayland-rs/wayland_protocols/wp/drm_lease/index.html)
- [Linux Kernel non-desktop EDID quirks](https://github.com/torvalds/linux/blob/master/drivers/gpu/drm/drm_edid.c)
- [Phoronix: Linux 4.15 non-desktop quirk](https://www.phoronix.com/news/Linux-4.15-Non-Desktop-Quirk)
- [Phoronix: KDE Plasma 5.24 DRM leasing](https://www.phoronix.com/news/Plasma-5.24-DRM-Leasing)
- [Gamescope Architecture (DeepWiki)](https://deepwiki.com/ValveSoftware/gamescope/2-architecture)
- [WiVRn GitHub Repository](https://github.com/WiVRn/WiVRn)
- [Collabora: Moving the Linux desktop to another reality](https://www.collabora.com/news-and-blog/news-and-events/moving-the-linux-desktop-to-another-reality.html)
- [Kernel drm_connector.h (non-desktop property)](https://github.com/torvalds/linux/blob/master/include/drm/drm_connector.h)
