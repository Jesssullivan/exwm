# R7.1 / R7.3: KWin VR Rendering Patterns & OpenXR Extension Ecosystem

## Overview

This document covers two parallel Week 7 research tasks:

- **R7.1**: Analysis of KWin's VR merge request (MR !8671) -- how it integrates Qt Quick
  3D XR into a Wayland compositor, composites 2D windows into 3D VR, shares textures,
  and synchronizes frames.
- **R7.3**: Survey of OpenXR extensions relevant to a VR window manager, with runtime
  support, `openxrs` crate coverage, and project week mapping.

Additionally: `openxrs` crate API reference for core types used in Week 7 implementation.

---

## Part 1: KWin VR Rendering Patterns (R7.1)

Source: https://invent.kde.org/plasma/kwin/-/merge_requests/8671

### Architecture Summary

KWin's VR mode is implemented as a **plugin embedded directly inside the compositor**,
not a separate process. This was a deliberate design choice by the author (Stanislav
Aleksandrov) for performance reasons: input logic, rendering logic, and window placement
must be unified within KWin to achieve the clarity and speed required for VR.

The plugin uses **Qt Quick 3D XR** as the 3D rendering framework, which internally
manages the OpenXR session, swapchain, and frame submission loop. The compositor does
not interact with OpenXR directly -- Qt Quick 3D XR abstracts that away.

```
Wayland Client
    |
    v  (wl_buffer / DMA-BUF)
KWin Compositor Core
    |
    v  (DMA-BUF fd + metadata)
EGLImage Creation (EGL_EXT_image_dma_buf_import)
    |
    v  (EGLImageKHR)
GL Texture Binding (glEGLImageTargetTexture2DOES)
    |
    v  (GL_TEXTURE_2D)
Qt Quick 3D XR Scene Graph (XrView)
    |
    v  (stereo rendered frames)
OpenXR Swapchain Submission
    |
    v  (xrEndFrame)
VR Runtime (Monado / WiVRn / SteamVR)
    |
    v
HMD Display
```

### Qt Quick 3D XR Integration

KWin's VR plugin uses the `XrView` QML type (introduced in Qt 6.8) as the root of the
VR scene. XrView handles:

1. **OpenXR session lifecycle** -- creates the XR instance, session, and manages
   state transitions (IDLE -> READY -> FOCUSED -> STOPPING) internally.
2. **Frame loop** -- drives `xrWaitFrame` / `xrBeginFrame` / `xrEndFrame` behind the
   scenes, synchronized with Qt's rendering pipeline.
3. **Stereo rendering** -- renders the Qt Quick 3D scene twice per frame (left/right eye)
   using multi-view rendering optimizations introduced in Qt 6.8.
4. **Input** -- provides `rayPick()` and `rayPickAll()` for controller/gaze ray-casting,
   and `processTouch()` / `setTouchpoint()` for hand interaction.

Key XrView properties used by KWin:

| Property | Purpose |
|----------|---------|
| `referenceSpace` | Coordinate origin (LOCAL for seated, STAGE for room-scale) |
| `xrOrigin` | Scene root -- must be set for rendering to occur |
| `fixedFoveation` | Foveated rendering level (None/Low/Medium/High) |
| `passthroughEnabled` | Camera feed background (requires XR_FB_passthrough) |
| `depthSubmissionEnabled` | Expose depth to compositor for reprojection |
| `multiViewRenderingEnabled` | Optimize stereo rendering into single pass |

**Implication for our project:** We do NOT use Qt Quick 3D XR. Our Smithay-based
compositor must implement the OpenXR session lifecycle, frame loop, and stereo rendering
directly via the `openxrs` crate. This gives us more control but requires significantly
more code.

### How KWin Composites 2D Windows into 3D VR

Each Wayland toplevel surface becomes a textured rectangle (quad) in the 3D Qt Quick 3D
scene. The pipeline:

1. **Surface buffer acquisition**: Wayland clients submit `wl_buffer` objects backed by
   DMA-BUF (via `zwp_linux_dmabuf_v1`). KWin receives the fd, format, stride, offset,
   and DRM modifier.

2. **EGLImage creation**: The DMA-BUF is imported via `eglCreateImageKHR` with
   `EGL_LINUX_DMA_BUF_EXT` target. Required EGL extensions:
   - `EGL_EXT_image_dma_buf_import` (basic DMA-BUF import)
   - `EGL_EXT_image_dma_buf_import_modifiers` (tiled/compressed format support)

3. **GL texture binding**: `glEGLImageTargetTexture2DOES(GL_TEXTURE_2D, egl_image)`
   binds the EGLImage to an OpenGL texture, achieving zero-copy import.

4. **Qt Quick 3D scene node**: The GL texture becomes a material on a `Model` node
   (rectangle geometry) positioned in 3D space. The scene includes:
   - Window content as textured rectangles
   - Window decorations and shadows (rendered by KWin's decoration engine)
   - 3D spatial positioning per window

5. **Stereo rendering**: Qt Quick 3D XR renders the entire scene twice with appropriate
   view/projection matrices from `xrLocateViews()`.

**Key design decision**: Window textures are imported **directly** into the VR rendering
pipeline without intermediate copies. This zero-copy path is critical for VR latency
(copying 2x 2064x2208 RGBA at 90Hz = ~1.5 GB/s).

### Texture Sharing Mechanism

The texture sharing relies on the EGL/GLES pathway:

```
Client GPU memory (DMA-BUF)
    |
    |  [same GPU memory, no copy]
    v
eglCreateImageKHR(EGL_LINUX_DMA_BUF_EXT)
    |
    |  [EGLImageKHR handle]
    v
glEGLImageTargetTexture2DOES
    |
    |  [GL texture handle]
    v
Qt Quick 3D material sampler
    |
    v
VR renderer (OpenXR swapchain FBO)
```

Requirements:
- Both the Wayland client and KWin must use the **same GPU** (or a GPU that supports
  cross-device DMA-BUF import, which is rare).
- The DRM format modifier must be understood by the importing driver. Mesa >= 21.1
  supports `VK_EXT_image_drm_format_modifier` on the Vulkan side; the EGL side has
  had modifier support longer.
- For YUV buffers (video surfaces), `GL_TEXTURE_EXTERNAL_OES` and `samplerExternalOES`
  are required instead of `GL_TEXTURE_2D`.

**Mapping to Smithay**: Smithay's `GlesRenderer::import_dmabuf()` (via `ImportDma` trait)
does exactly the same EGLImage import internally. The GL texture ID is encapsulated in
`GlesTexture`. For our VR renderer, we need to either:

1. **Render into OpenXR swapchain directly**: Get GL texture from
   `xrEnumerateSwapchainImages`, create FBO, bind it as render target, use Smithay's
   renderer to draw into it.
2. **Blit from Smithay FBO to swapchain**: Render scene into Smithay's offscreen FBO,
   then `glBlitFramebuffer` to swapchain image. One extra copy per eye, but simpler
   integration.

### Frame Synchronization

KWin must synchronize two independent frame clocks:

1. **Wayland compositor frame clock**: Driven by `wl_surface.frame` callbacks and output
   VSync. Typically 60Hz for monitors.
2. **OpenXR frame clock**: Driven by `xrWaitFrame` and the HMD refresh rate. Typically
   72-120Hz.

Qt Quick 3D XR handles this by **taking over the render loop**. When VR mode is active,
the normal KWin compositor output path (DRM CRTC scanout) is bypassed. Instead:

1. `xrWaitFrame()` blocks until the runtime signals the next frame should begin.
   Returns `predictedDisplayTime` and `predictedDisplayPeriod`.
2. `xrBeginFrame()` marks the start of GPU work.
3. Qt Quick 3D renders the scene using the latest surface textures (updated whenever
   clients commit new buffers, asynchronously from the VR frame).
4. `xrEndFrame()` submits the `XrCompositionLayerProjection` with both eye views.

**Surface update race**: Wayland clients commit buffers asynchronously. The VR renderer
uses whatever texture is current at render time. If a client commits mid-frame, the new
buffer is picked up on the next VR frame. There is no explicit synchronization between
client commit and VR frame -- the DMA-BUF import ensures the GPU memory is shared and
the EGLImage fence handles GPU-side synchronization.

**For our compositor**: We must implement a similar dual-clock architecture:
- Smithay's calloop event loop handles Wayland protocol events (surface commits, input)
- A separate timer source (or dedicated thread) drives the OpenXR frame loop at HMD rate
- Surface textures are imported on commit, consumed on VR render -- no lock needed if
  import produces a new GL texture each time (old texture remains valid until destroyed)

### Input Routing

KWin VR mode supports:

- **Head-gaze**: A ray from the head pose forward direction. The intersection with window
  quads determines which window receives input. No VR controller required.
- **Keyboard**: Physical keyboard events are forwarded to the VR-focused window.
- **VR controllers**: Not yet implemented in the MR (listed as missing feature).

The head-gaze input model is particularly relevant to our project since we plan gaze-based
focus (Week 12) and head-gaze as a precursor to eye tracking.

Qt Quick 3D XR's `rayPick()` method performs the 3D ray-to-surface intersection. The
intersection point in UV space maps to pixel coordinates on the Wayland surface, which
are then synthesized as `wl_pointer` events.

### Comparison: KWin Approach vs Our Approach

| Aspect | KWin VR | Our Compositor |
|--------|---------|----------------|
| 3D framework | Qt Quick 3D XR (high-level) | Raw OpenGL via openxrs (low-level) |
| OpenXR management | Abstracted by Qt | Direct via openxrs crate |
| Texture import | EGLImage (same as Smithay) | EGLImage via Smithay ImportDma |
| Scene graph | Qt Quick 3D scene graph | Custom VrScene (Week 8) |
| Stereo rendering | Multi-view (Qt handles) | Manual per-eye render loop |
| Input routing | Qt rayPick() | Custom ray-cast (Week 10) |
| Frame sync | Qt manages XrWaitFrame | Our calloop timer + xrWaitFrame |
| Compositor integration | Plugin inside KWin | Built into compositor from start |
| Language | C++/QML | Rust |
| WM integration | KDE KWin window management | Emacs-driven via IPC |

**Key lesson**: KWin's approach validates that direct DMA-BUF import into a VR scene
graph is the correct architecture. The main difference is that Qt Quick 3D XR hides
enormous complexity (OpenXR lifecycle, stereo rendering, multi-view optimization) that
we must implement ourselves. However, this gives us control over frame timing,
composition layer selection, and custom rendering effects that Qt's abstraction does not
expose.

---

## Part 2: OpenXR Extension Ecosystem Survey (R7.3)

### Extension Catalog

#### XR_KHR_composition_layer_cylinder

**Purpose**: Renders a texture onto the interior of a cylindrical surface. Ideal for
curved virtual monitors that maintain uniform text readability across the surface.

**Specification**: `XrCompositionLayerCylinderKHR` contains:
- `space` -- reference space for positioning
- `eyeVisibility` -- which eye(s) see this layer
- `subImage` -- swapchain image region
- `pose` -- center point of the cylinder in reference space
- `radius` -- distance from viewer (0 or infinity = infinite cylinder)
- `centralAngle` -- arc width in radians
- `aspectRatio` -- height/width ratio

**Use case for us**: Week 8 cylindrical projection for text-heavy surfaces. Instead of
rendering a curved mesh in our custom renderer (stage 8.6), we could submit the surface
texture directly as a cylinder composition layer. The runtime composites it with correct
curvature -- potentially higher quality than our mesh approximation since the runtime
can use per-pixel reprojection.

**Runtime support**: Broad -- Monado (desktop/Android/Windows), SteamVR, Meta Quest
(1/2/3/3S/Pro), Meta PC, Varjo, HTC Vive Focus 3, Bytedance PICO, Windows Mixed Reality.

**openxrs crate**: Exposed as `InstanceExtensions::khr_composition_layer_cylinder`.

**Project week**: Week 8 (VR scene rendering, stage 8.6).

**Trade-off**: Composition layers are composited by the runtime after our frame submission.
This means the runtime handles reprojection, potentially reducing latency. However, each
layer is limited to a single swapchain image -- we cannot combine multiple surfaces into
one cylinder layer without first rendering them into a shared texture.

---

#### XR_KHR_composition_layer_equirect / XR_KHR_composition_layer_equirect2

**Purpose**: Renders a texture onto the interior of a sphere using equirectangular
projection. Equirect2 (preferred) uses angle-based parameters for precise control.

**Specification**: `XrCompositionLayerEquirect2KHR` contains:
- `pose` -- center of the sphere in reference space
- `radius` -- sphere radius
- `centralHorizontalAngle` -- horizontal arc in radians
- `upperVerticalAngle` / `lowerVerticalAngle` -- vertical extent

Original `XrCompositionLayerEquirectKHR` uses `scale`/`bias` UV mapping instead
(less intuitive).

**Use case for us**: Week 8 VR background environment (stage 8.10). A 360-degree
panoramic image can serve as the VR workspace background. Also useful for immersive
video playback.

**Runtime support**: Monado (desktop/Android/Windows), SteamVR, Varjo, Meta PC,
Bytedance PICO. Notably narrower support than cylinder -- HTC and some Meta Quest
models do not support equirect but do support equirect2.

**openxrs crate**: Exposed as `InstanceExtensions::khr_composition_layer_equirect`.
The equirect2 variant is also available as `khr_composition_layer_equirect2`.

**Project week**: Week 8 (VR background, stage 8.10).

---

#### XR_FB_passthrough

**Purpose**: Enables real-time camera passthrough from the HMD's external cameras,
composited as a layer behind or blended with virtual content. This is the foundation
for AR/MR experiences on Meta Quest devices.

**Specification**: The passthrough API creates a passthrough feature object and
passthrough layers. Passthrough is rendered by a dedicated runtime service into a
separate composition layer -- the application **cannot access raw camera images**.
The passthrough layer is submitted in `xrEndFrame` before the application's projection
layers to appear as background.

Key API:
- `session.create_passthrough(flags)` -> `Passthrough`
- `session.create_passthrough_layer(passthrough, flags, purpose)` -> `PassthroughLayerFB`
- Submit `PassthroughLayerFB` as composition layer in `xrEndFrame`

**Use case for us**: Week 8 VR background (stage 8.10, passthrough option) and potential
future AR mode where Wayland windows float over the real world. This is compelling for
desk-based work: see your physical keyboard, drink, desk while windows float in space.

**Runtime support**: Meta Quest (1/2/3/3S/Pro), Meta PC, Meta XR Simulator, Bytedance
PICO, HTC Vive Cosmos. **Not supported on Monado, SteamVR, or Varjo** -- this is a
Meta-originated extension.

**openxrs crate**: Exposed as `InstanceExtensions::fb_passthrough`. Session methods:
`session.create_passthrough()` and `session.create_passthrough_layer()`.

**Project week**: Week 8 (optional background mode). Week 18+ for full AR window manager.

**Portability concern**: FB-prefix extensions are Meta-specific. The broader ecosystem
alternative is `XR_META_passthrough` (successor) or future KHR-standardized passthrough.
For Monado/SteamVR, passthrough is not currently available through OpenXR. Our
implementation should gracefully degrade when this extension is absent.

---

#### XR_EXT_eye_gaze_interaction

**Purpose**: Provides eye gaze tracking as an input source. Returns a gaze ray (origin +
direction) that can be used for gaze-based interaction, focus targeting, and foveated
rendering.

**Specification**: Eye gaze is exposed as an OpenXR input action path:
`/user/eyes_ext/input/gaze_ext/pose`. The application creates an action of type
`XR_ACTION_TYPE_POSE_INPUT` bound to this path, creates an action space, and locates
the space each frame to get the gaze ray.

The gaze pose represents a ray from between the user's eyes in the gaze direction.
Position = origin (approximately midpoint between eyes), orientation = gaze direction.

**Use case for us**: Week 11-12 (eye tracking integration). This is the OpenXR-native
way to get eye gaze, bypassing our planned Pupil Labs integration for HMDs with built-in
eye trackers (Quest Pro, Varjo, PICO 4 Pro). The gaze ray from this extension directly
feeds our gaze-focus system (Week 12).

**Runtime support**: Varjo, Meta Quest Pro, HTC Vive Cosmos/Focus 3, Magic Leap ML2,
Monado (desktop), Bytedance PICO, Windows Mixed Reality (HoloLens 2). **Not supported
on base Quest 2/3 (no eye tracking hardware) or SteamVR**.

**openxrs crate**: Exposed as `InstanceExtensions::ext_eye_gaze_interaction`.

**Project week**: Week 11 (eye tracker integration), Week 12 (gaze-based focus).

**Integration note**: When this extension is available, it provides gaze data natively
through OpenXR -- no need for Pupil Labs ZMQ protocol. When unavailable (Quest 3 without
eye tracking), fall back to head-gaze (Week 10) or external eye tracker (Pupil Labs,
Week 11).

---

#### XR_EXT_hand_tracking

**Purpose**: Provides skeletal hand tracking -- 26 joints per hand with position,
orientation, radius, and linear/angular velocity. Enables controller-free VR interaction.

**Specification**: Key API:
- `session.create_hand_tracker(Hand::LEFT)` -> `HandTracker`
- `hand_tracker.locate_joints(space, time)` -> joint locations for all 26 joints
  (wrist, palm, thumb x4, index x5, middle x5, ring x5, little x5)
- Each joint has `XrHandJointLocationEXT { pose, radius }` and optionally velocity

Joint IDs follow a standard hierarchy:
`PALM`, `WRIST`, `THUMB_METACARPAL` through `LITTLE_TIP`.

**Use case for us**: Week 18 (hand tracking integration). Enables:
- Pinch-to-select windows
- Grab-and-move window repositioning
- Tap-to-click on virtual surfaces
- Gesture recognition for workspace switching

**Runtime support**: Universal -- all 23 listed runtimes support this extension.
This is the most widely supported optional extension in the ecosystem.

**openxrs crate**: Exposed as `InstanceExtensions::ext_hand_tracking`. Session method:
`session.create_hand_tracker(hand)`.

**Project week**: Week 18 (hand tracking and gesture input).

---

#### XR_FB_display_refresh_rate

**Purpose**: Allows querying and requesting display refresh rate changes. Mobile HMDs
(Quest) support multiple refresh rates (72Hz, 80Hz, 90Hz, 120Hz) with different power
and latency trade-offs.

**Specification**: Key API:
- `session.enumerate_display_refresh_rates()` -> `Vec<f32>` (e.g., [72.0, 80.0, 90.0, 120.0])
- `session.get_display_refresh_rate()` -> current rate
- `session.request_display_refresh_rate(rate)` -> request change

**Use case for us**: Adaptive refresh rate strategy:
- 120Hz for active interaction (typing, window manipulation)
- 72Hz for passive reading (power savings, reduced GPU load)
- Dynamic switching based on activity detection

**Runtime support**: Meta Quest (1/2/3/3S/Pro), Meta PC, Monado (Android/desktop),
SteamVR, Varjo, Bytedance PICO. Broad support.

**openxrs crate**: Exposed as `InstanceExtensions::fb_display_refresh_rate`. Session
methods: `enumerate_display_refresh_rates()`, `get_display_refresh_rate()`,
`request_display_refresh_rate()`.

**Project week**: Week 9 (VR output management, adaptive refresh). Could also be used in
Week 12 (reduce refresh when gaze is stable/reading).

---

#### XR_VARJO_foveated_rendering

**Purpose**: Enables foveated rendering on Varjo HMDs, which use a quad-view display
system with a high-resolution inset (focus) region and lower-resolution peripheral region.
Eye tracking steers the focus region.

**Specification**: Augments `XR_VARJO_quad_views`. The extension returns foveated view
configuration with different recommended resolutions for focus vs peripheral views.
`XrFoveatedViewConfigurationViewVARJO` has `foveatedRenderingActive` boolean.

**Use case for us**: Performance optimization on Varjo hardware. Render the gaze-targeted
area at full resolution, periphery at reduced resolution. Reduces GPU load by 30-50%.

**Runtime support**: Varjo only. This is a vendor-specific extension.

**openxrs crate**: Exposed as `InstanceExtensions::varjo_foveated_rendering`.

**Project week**: Week 12+ (foveated rendering optimization, optional).

**Note**: OpenXR 1.1 promoted foveated rendering concepts into the core specification.
The standardized `XR_EXT_foveated_rendering` (non-Varjo) should be preferred when
available. The `openxrs` crate also exposes `fb_foveation` for Meta's foveation API.

---

#### XR_EXT_performance_settings

**Purpose**: Allows applications to provide performance hints to the runtime and receive
notifications about thermal/performance state changes.

**Specification**: Four performance levels (CPU and GPU independently):
- **Power Savings**: Prioritize energy conservation
- **Sustained Low**: Balance resources and performance, prioritize energy
- **Sustained High**: Default -- prioritize consistent rendering (thermally sustainable)
- **Boost**: Maximum performance, not thermally sustainable (< 30 seconds)

Thermal notifications:
- **Normal**: Operating within sustainable range
- **Warning**: Approaching thermal limits
- **Critical**: Must reduce workload immediately

**Use case for us**: Runtime power/thermal management for standalone HMDs (Quest).
Strategy:
- Start in Sustained High for active work
- Drop to Sustained Low when user is reading (no interaction for N seconds)
- Boost briefly during window layout transitions
- React to Warning by reducing surface count or refresh rate

**Runtime support**: Meta Quest (1/2/3/3S/Pro), Bytedance PICO, HTC Vive Focus 3. Not
supported on Monado, SteamVR, or Varjo (desktop GPUs have their own thermal management).

**openxrs crate**: Exposed as `InstanceExtensions::ext_performance_settings`.

**Project week**: Week 9 (output management), Week 12 (adaptive performance based on
gaze activity).

---

### Extension Support Summary Matrix

| Extension | Monado | SteamVR | Quest | Varjo | WMR | openxrs | Week |
|-----------|--------|---------|-------|-------|-----|---------|------|
| `khr_composition_layer_cylinder` | Yes | Yes | Yes | Yes | Yes | Yes | 8 |
| `khr_composition_layer_equirect` | Yes | Yes | Partial | Yes | No | Yes | 8 |
| `fb_passthrough` | No | No | Yes | No | No | Yes | 8, 18+ |
| `ext_eye_gaze_interaction` | Yes | No | Pro only | Yes | HL2 | Yes | 11-12 |
| `ext_hand_tracking` | Yes | Yes | Yes | Yes | Yes | Yes | 18 |
| `fb_display_refresh_rate` | Yes | Yes | Yes | Yes | No | Yes | 9 |
| `varjo_foveated_rendering` | No | No | No | Yes | No | Yes | 12+ |
| `ext_performance_settings` | No | No | Yes | No | No | Yes | 9, 12 |

**Portability strategy**: Our compositor must work without any optional extension.
Core rendering uses only `XR_KHR_opengl_enable` (required) and standard composition
layers (`XrCompositionLayerProjection`). All extensions above are opt-in enhancements
detected at runtime via `entry.enumerate_extensions()`.

### Additional Extensions Worth Tracking

| Extension | Purpose | Week |
|-----------|---------|------|
| `XR_MND_headless` | Monado headless mode for CI testing | 7 |
| `XR_KHR_composition_layer_depth` | Depth buffer submission for reprojection | 8 |
| `XR_EXT_hand_tracking_data_source` | Controller vs camera hand tracking | 18 |
| `XR_FB_hand_tracking_mesh` | Hand mesh geometry for rendering | 18 |
| `XR_EXT_local_floor` | Floor-relative reference space | 7 |
| `XR_KHR_convert_timespec_time` | Convert OpenXR Time to POSIX timespec | 7 |
| `XR_EXT_debug_utils` | Debug naming for OpenXR objects | 7 |
| `XR_KHR_visibility_mask` | Optimize rendering by masking invisible pixels | 8 |

---

## Part 3: openxrs Crate API Reference

**Crate**: `openxr` v0.21.1 (latest as of February 2026)
**Repository**: https://github.com/Ralith/openxrs
**Low-level bindings**: `openxr-sys` v0.13.1
**License**: MIT OR Apache-2.0
**Documentation coverage**: 2.81% (minimal -- refer to OpenXR specification for details)

### Supported Graphics Bindings

| Binding | Type | Notes |
|---------|------|-------|
| `Vulkan` | Full support | Primary example in repo |
| `OpenGL` | Full support | Our target (Week 7) |
| `OpenGlEs` | Full support | Alternative for embedded |
| `Headless` | Session without graphics | Testing only |
| `AnyGraphics` | Type-erased session | For graphics-agnostic code |

### Entry

Entry point to the OpenXR API. Start here.

```rust
// Option 1: Static linking (requires "linked" feature)
let entry = xr::Entry::linked();

// Option 2: Dynamic loading (requires "loaded" feature, unsafe)
let entry = unsafe { xr::Entry::load()? };

// Option 3: Load from specific path (unsafe)
let entry = unsafe { xr::Entry::load_from(Path::new("/usr/lib/libopenxr_loader.so"))? };

// Option 4: From raw function pointer (unsafe)
let entry = unsafe { xr::Entry::from_get_instance_proc_addr(get_proc_addr)? };
```

Key methods:
- `entry.enumerate_extensions()` -> `Result<ExtensionSet>` -- discover available extensions
- `entry.enumerate_layers()` -> `Result<Vec<ApiLayerProperties>>` -- list API layers
- `entry.create_instance(app_info, extensions, layers)` -> `Result<Instance>`

Entry is `Clone + Send + Sync`.

### Instance

Root object mediating application's interaction with OpenXR.

```rust
let mut enabled_extensions = xr::ExtensionSet::default();
enabled_extensions.khr_opengl_enable = true;
// Enable optional extensions if available:
if available.ext_eye_gaze_interaction {
    enabled_extensions.ext_eye_gaze_interaction = true;
}

let instance = entry.create_instance(
    &xr::ApplicationInfo {
        application_name: "exwm-vr-compositor",
        application_version: 1,
        engine_name: "smithay",
        engine_version: 1,
    },
    &enabled_extensions,
    &[], // no API layers
)?;
```

Key methods:
- `instance.system(FormFactor::HEAD_MOUNTED_DISPLAY)` -> `Result<SystemId>`
- `instance.system_properties(system)` -> `Result<SystemProperties>`
- `instance.properties()` -> `Result<InstanceProperties>`
- `instance.create_session::<G>(system, info)` -> `Result<(Session<G>, FrameWaiter, FrameStream<G>)>`
- `instance.graphics_requirements::<G>(system)` -> `Result<G::Requirements>`
- `instance.enumerate_view_configurations(system)` -> `Result<Vec<ViewConfigurationType>>`
- `instance.enumerate_view_configuration_views(system, ty)` -> `Result<Vec<ViewConfigurationView>>`
- `instance.enumerate_environment_blend_modes(system, ty)` -> `Result<Vec<EnvironmentBlendMode>>`
- `instance.string_to_path(string)` -> `Result<Path>` (for action bindings)
- `instance.suggest_interaction_profile_bindings(profile, bindings)` -> `Result<()>`
- `instance.create_action_set(name, localized_name, priority)` -> `Result<ActionSet>`
- `instance.poll_event(storage)` -> `Result<Option<Event>>`
- `instance.supports_hand_tracking(system)` -> `Result<bool>`

Vulkan-specific (not used in Week 7 but relevant for future):
- `instance.create_vulkan_instance(...)` / `instance.vulkan_graphics_device(...)`

Instance is `Clone + Send + Sync`.

### Session

A rendering session using a particular graphics API.

```rust
// Create session with OpenGL binding
let (session, frame_waiter, frame_stream) = instance.create_session::<xr::OpenGL>(
    system_id,
    &xr::opengl::SessionCreateInfo::Wayland {
        display: wayland_display_ptr,
    },
)?;

// Session lifecycle
session.begin(xr::ViewConfigurationType::PRIMARY_STEREO)?;
// ... render frames ...
session.end()?;
session.request_exit()?;
```

Key methods:
- `session.begin(view_config_type)` -> `Result<()>`
- `session.end()` -> `Result<()>`
- `session.request_exit()` -> `Result<()>`
- `session.enumerate_swapchain_formats()` -> `Result<Vec<G::Format>>`
- `session.create_swapchain(info)` -> `Result<Swapchain<G>>`
- `session.create_reference_space(type, pose)` -> `Result<Space>`
- `session.enumerate_reference_spaces()` -> `Result<Vec<ReferenceSpaceType>>`
- `session.attach_action_sets(sets)` -> `Result<()>`
- `session.sync_actions(action_sets)` -> `Result<()>`
- `session.locate_views(config_type, time, space)` -> `Result<(ViewStateFlags, Vec<View>)>`

Extension methods (available when extension enabled):
- `session.create_hand_tracker(hand)` -> `Result<HandTracker>` (XR_EXT_hand_tracking)
- `session.create_passthrough(flags)` -> `Result<Passthrough>` (XR_FB_passthrough)
- `session.enumerate_display_refresh_rates()` -> `Result<Vec<f32>>` (XR_FB_display_refresh_rate)
- `session.get_display_refresh_rate()` -> `Result<f32>`
- `session.request_display_refresh_rate(rate)` -> `Result<()>`

### OpenGL Session Binding (Wayland)

```rust
// For Wayland (our target):
let session_info = xr::opengl::SessionCreateInfo::Wayland {
    display: wayland_display_ptr, // *mut wl_display
};

// For X11/GLX (fallback/development):
let session_info = xr::opengl::SessionCreateInfo::Xlib {
    x_display: x_display_ptr,
    visualid: visual_id,
    glx_fb_config: fb_config,
    glx_drawable: drawable,
    glx_context: context,
};
```

**Important**: The OpenGL `SessionCreateInfo` variants do NOT implement `Send` or `Sync`
due to raw pointers. Session creation must happen on the thread owning the GL context.

### Swapchain

A set of images rendered to using a particular graphics API.

```rust
let swapchain = session.create_swapchain(&xr::SwapchainCreateInfo {
    create_flags: xr::SwapchainCreateFlags::EMPTY,
    usage_flags: xr::SwapchainUsageFlags::COLOR_ATTACHMENT
               | xr::SwapchainUsageFlags::SAMPLED,
    format: gl::SRGB8_ALPHA8 as u32,  // preferred for VR
    sample_count: 1,
    width: recommended_width,
    height: recommended_height,
    face_count: 1,
    array_size: 1,  // 1 for non-array, 2 for multiview stereo
    mip_count: 1,
})?;

// Get backing images (GL texture IDs for OpenGL binding)
let images: Vec<u32> = swapchain.enumerate_images()?;
```

Key methods:
- `swapchain.enumerate_images()` -> `Result<Vec<G::SwapchainImage>>`
- `swapchain.acquire_image()` -> `Result<u32>` (image index)
- `swapchain.wait_image(timeout)` -> `Result<()>`
- `swapchain.release_image()` -> `Result<()>`

### FrameWaiter / FrameStream

Handle frame timing and submission. Separated from Session to enforce single-writer
frame semantics via `&mut self`.

```rust
// FrameWaiter: blocks until runtime signals next frame
let frame_state: xr::FrameState = frame_waiter.wait()?;
// frame_state.predicted_display_time: when this frame will be shown
// frame_state.predicted_display_period: interval between frames
// frame_state.should_render: whether to actually render

// FrameStream: begin/end frame submission
frame_stream.begin()?;

if frame_state.should_render {
    // ... render to swapchain ...
}

// Submit composition layers
frame_stream.end(
    frame_state.predicted_display_time,
    xr::EnvironmentBlendMode::OPAQUE,
    &[&xr::CompositionLayerProjection::new()
        .space(&stage_space)
        .views(&views)],
)?;
```

### Reference Spaces

```rust
// Seated experience (origin at initial head position)
let local = session.create_reference_space(
    xr::ReferenceSpaceType::LOCAL,
    xr::Posef::IDENTITY,
)?;

// Room-scale (origin at floor center)
let stage = session.create_reference_space(
    xr::ReferenceSpaceType::STAGE,
    xr::Posef::IDENTITY,
)?;

// Head-locked (moves with head, for HUD elements)
let view = session.create_reference_space(
    xr::ReferenceSpaceType::VIEW,
    xr::Posef::IDENTITY,
)?;

// Query play area bounds (STAGE only)
let bounds: Option<xr::Extent2Df> = session.reference_space_bounds_rect(
    xr::ReferenceSpaceType::STAGE,
)?;
```

### Complete Frame Loop Pattern

```rust
loop {
    // 1. Poll OpenXR events (session state changes, etc.)
    while let Some(event) = instance.poll_event(&mut event_storage)? {
        match event {
            xr::Event::SessionStateChanged(e) => {
                match e.state() {
                    xr::SessionState::READY => session.begin(PRIMARY_STEREO)?,
                    xr::SessionState::STOPPING => session.end()?,
                    xr::SessionState::EXITING => return Ok(()),
                    _ => {}
                }
            }
            _ => {}
        }
    }

    // 2. Wait for frame timing
    let frame_state = frame_waiter.wait()?;

    // 3. Begin frame
    frame_stream.begin()?;

    let layers = if frame_state.should_render {
        // 4. Get eye poses
        let (_, views) = session.locate_views(
            PRIMARY_STEREO,
            frame_state.predicted_display_time,
            &local_space,
        )?;

        // 5. For each eye: acquire -> render -> release
        for (eye, view) in views.iter().enumerate() {
            let image_index = swapchains[eye].acquire_image()?;
            swapchains[eye].wait_image(xr::Duration::from_nanos(100_000_000))?;

            // Bind FBO, set viewport, render scene with view.pose and view.fov
            render_eye(eye, image_index, &view.pose, &view.fov, &scene)?;

            swapchains[eye].release_image()?;
        }

        // 6. Build composition layer
        let projection_views = build_projection_views(&views, &swapchains);
        vec![xr::CompositionLayerProjection::new()
            .space(&local_space)
            .views(&projection_views)]
    } else {
        vec![] // submit empty frame when not rendering
    };

    // 7. End frame with composition layers
    let layer_refs: Vec<&_> = layers.iter().map(|l| l as &_).collect();
    frame_stream.end(
        frame_state.predicted_display_time,
        xr::EnvironmentBlendMode::OPAQUE,
        &layer_refs,
    )?;
}
```

---

## Part 4: Lessons for Our Compositor

### Architecture Decisions Validated

1. **Direct DMA-BUF import is the correct approach.** Both KWin and wxrd use zero-copy
   DMA-BUF import for window textures. Our Smithay `ImportDma` path is equivalent.

2. **OpenGL binding for initial implementation.** KWin uses OpenGL (via EGL/GLES in Qt).
   Our choice of `XR_KHR_opengl_enable` with Smithay's GLES renderer is validated.
   Vulkan can come later.

3. **Composition layers vs custom rendering.** For curved monitors, composition layer
   cylinders (`XR_KHR_composition_layer_cylinder`) should be evaluated against custom
   mesh rendering. Layers offer better quality (runtime reprojection) but less flexibility
   (one texture per layer, limited geometric control).

4. **Frame timing must be independent.** The VR frame loop runs at HMD rate (72-120Hz),
   independent of Wayland client commit rate. Surface texture updates are asynchronous.

### Gaps We Must Fill

1. **No Qt Quick 3D XR abstraction.** We implement everything Qt hides: OpenXR lifecycle,
   stereo rendering, ray-casting, multi-view optimization. This is ~2000 lines of code
   that KWin gets for free from Qt.

2. **Custom scene graph.** Week 8 must build a scene graph comparable to what Qt Quick 3D
   provides. Minimum: transform hierarchy, frustum culling, texture management.

3. **Input ray-casting.** Week 10 must implement ray-to-quad intersection for gaze and
   controller input. KWin uses Qt's `rayPick()` which handles arbitrary 3D geometry.

4. **Extension feature detection.** Our `VrState` must track which extensions were
   enabled and gate features accordingly. The `InstanceExtensions` struct from `openxrs`
   makes this straightforward.

### Recommended Extension Priority for Implementation

| Priority | Extension | Rationale |
|----------|-----------|-----------|
| P0 (Week 7) | `khr_opengl_enable` | Required for OpenGL binding |
| P0 (Week 7) | `mnd_headless` | CI testing without HMD |
| P0 (Week 7) | `ext_debug_utils` | Debug naming for development |
| P1 (Week 8) | `khr_composition_layer_cylinder` | Curved virtual monitors |
| P1 (Week 8) | `khr_composition_layer_depth` | Reprojection quality |
| P1 (Week 9) | `fb_display_refresh_rate` | Adaptive refresh |
| P2 (Week 11) | `ext_eye_gaze_interaction` | Native eye tracking |
| P2 (Week 12) | `ext_performance_settings` | Thermal management |
| P3 (Week 18) | `ext_hand_tracking` | Hand input |
| P3 (Week 8) | `fb_passthrough` | AR mode (Quest only) |
| P4 (Week 12+) | `varjo_foveated_rendering` | Varjo-specific optimization |

---

## References

- KWin VR MR: https://invent.kde.org/plasma/kwin/-/merge_requests/8671
- Qt Quick 3D XR docs: https://doc.qt.io/qt-6/qt-quick-3d-xr.html
- XrView QML type: https://doc.qt.io/qt-6/qml-qtquick3d-xr-xrview.html
- openxrs crate: https://docs.rs/openxr/latest/openxr/
- openxrs GitHub: https://github.com/Ralith/openxrs
- openxrs vulkan.rs example: https://github.com/Ralith/openxrs/blob/master/openxr/examples/vulkan.rs
- OpenXR 1.1 specification: https://registry.khronos.org/OpenXR/specs/1.1/html/xrspec.html
- OpenXR Runtime Extension Support Report: https://github.khronos.org/OpenXR-Inventory/runtime_extension_support.html
- XrCompositionLayerCylinderKHR spec: https://registry.khronos.org/OpenXR/specs/1.0/man/html/XrCompositionLayerCylinderKHR.html
- XR_FB_passthrough: https://developers.meta.com/horizon/documentation/native/android/mobile-passthrough/
- wxrd (Collabora VR compositor): https://www.collabora.com/news-and-blog/news-and-events/wxrd-a-standalone-wayland-compositor-for-xrdesktop.html
- xrdesktop architecture: https://www.collabora.com/news-and-blog/news-and-events/moving-the-linux-desktop-to-another-reality.html
- wxrc (Wayland XR compositor): https://sr.ht/~bl4ckb0ne/wxrc/
- OpenXR frame timing: https://registry.khronos.org/OpenXR/specs/1.0/man/html/xrWaitFrame.html
- Monado developer site: https://monado.freedesktop.org/
