# Smithay Anvil Reference Compositor: Patterns and Adaptation Notes

Research date: 2026-02-08
Source: https://github.com/Smithay/smithay `anvil/` directory (master branch)
Target: EXWM-VR compositor (Smithay 0.7, Emacs pgtk, IPC over Unix socket)

---

## Table of Contents

1. [State Management](#1-state-management)
2. [Handler Trait Implementations](#2-handler-trait-implementations)
3. [Event Loop Source Registration](#3-event-loop-source-registration)
4. [Rendering Pipeline](#4-rendering-pipeline)
5. [Input Handling](#5-input-handling)
6. [Multi-Output Support](#6-multi-output-support)
7. [Minimum Trait Set](#7-minimum-trait-set-for-a-functional-compositor)
8. [DRM Leasing for VR](#8-drm-leasing-for-vr)
9. [Adaptation Strategy for EXWM-VR](#9-adaptation-strategy-for-exwm-vr)

---

## 1. State Management

### 1.1 The Backend Trait

Anvil is generic over its backend. The `Backend` trait abstracts capabilities that
differ between windowed (winit/x11) and hardware (udev/DRM) modes:

```rust
pub trait Backend {
    const HAS_RELATIVE_MOTION: bool = false;
    const HAS_GESTURES: bool = false;
    fn seat_name(&self) -> String;
    fn reset_buffers(&mut self, output: &Output);
    fn early_import(&mut self, surface: &WlSurface);
    fn update_led_state(&mut self, led_state: LedState);
}
```

**Adaptation note**: For EXWM-VR, define a similar trait with VR-specific constants:

```rust
pub trait Backend {
    const HAS_RELATIVE_MOTION: bool = false;
    const HAS_GESTURES: bool = false;
    const HAS_VR_OUTPUT: bool = false;      // new: whether VR HMD is available
    fn seat_name(&self) -> String;
    fn reset_buffers(&mut self, output: &Output);
    fn early_import(&mut self, surface: &WlSurface);
    fn update_led_state(&mut self, led_state: LedState);
    fn emacs_ipc_socket(&self) -> Option<&UnixStream>;  // new: Emacs connection
}
```

### 1.2 AnvilState Struct

`AnvilState<BackendData: Backend>` is the central compositor state. It holds **everything**
in one struct, allowing calloop callbacks to receive `&mut AnvilState` without Rc/RefCell.

**Field categories** (approximately 30+ fields):

| Category | Fields | Purpose |
|----------|--------|---------|
| Core infrastructure | `backend_data`, `socket_name`, `display_handle`, `running`, `handle` | Event loop and display server |
| Desktop | `space: Space<WindowElement>`, `popups: PopupManager` | Window layout and popup tracking |
| Protocol states | `compositor_state`, `data_device_state`, `layer_shell_state`, `output_manager_state`, `primary_selection_state`, `data_control_state`, `seat_state`, `shm_state`, `xdg_shell_state`, `xdg_decoration_state`, `xdg_activation_state`, `presentation_state`, `fractional_scale_manager_state`, `viewporter_state`, `security_context_state`, `xdg_foreign_state` | Wayland protocol implementations |
| Input | `seat: Seat<Self>`, `pointer`, `clock`, `cursor_status`, `suppressed_keys` | Seat, pointer, keyboard state |
| XWayland (optional) | `xwm`, `xdisplay`, `xwayland_shell_state` | X11 compatibility |
| Debug | `renderdoc`, `show_window_preview` | Development aids |

**Adaptation note**: EXWM-VR should add fields for:
- `emacs_client: Option<EmacsIpcClient>` -- connection to Emacs pgtk
- `vr_output: Option<VrOutputState>` -- VR HMD state when leased
- `ipc_socket_path: PathBuf` -- Unix socket for IPC

### 1.3 AnvilState::init

The `init` function creates all Wayland globals and wires up the state:

```rust
pub fn init(
    display: Display<AnvilState<BackendData>>,
    handle: LoopHandle<'static, AnvilState<BackendData>>,
    backend_data: BackendData,
    listen_on_socket: bool,
) -> AnvilState<BackendData>
```

Key initialization sequence:
1. Create `DisplayHandle` from `Display`
2. Create each protocol state (`CompositorState::new`, `ShmState::new`, etc.)
3. Create `Seat` with keyboard and pointer capabilities
4. Optionally listen on a Wayland socket (`display_handle.add_socket_auto()`)
5. Optionally start XWayland
6. Return the assembled `AnvilState`

**Adaptation note**: For EXWM-VR, extend init to also:
- Open the Emacs IPC Unix socket
- Register the IPC socket as a calloop source
- Skip XWayland unless explicitly needed

### 1.4 UdevData (Hardware Backend)

For DRM/hardware rendering, anvil defines `UdevData` as the `BackendData`:

```rust
pub struct UdevData {
    pub session: LibSeatSession,
    dh: DisplayHandle,
    dmabuf_state: Option<(DmabufState, DmabufGlobal)>,
    syncobj_state: Option<DrmSyncobjState>,
    primary_gpu: DrmNode,
    gpus: GpuManager<GbmGlesBackend<GlesRenderer, DrmDeviceFd>>,
    backends: HashMap<DrmNode, BackendData>,
    pointer_images: Vec<(xcursor::parser::Image, MemoryRenderBuffer)>,
    pointer_element: PointerElement,
    debug_flags: DebugFlags,
    keyboards: Vec<smithay::reexports::input::Device>,
}
```

**Adaptation note**: EXWM-VR will use something similar but track which DRM node
is the VR HMD vs. desktop monitors, and maintain separate render state per output class.

### 1.5 WinitData (Windowed Backend)

For development/testing, anvil uses a simpler windowed backend:

```rust
pub struct WinitData {
    backend: WinitGraphicsBackend<GlesRenderer>,
    damage_tracker: OutputDamageTracker,
    dmabuf_state: (DmabufState, DmabufGlobal, Option<DmabufFeedback>),
    full_redraw: u8,
}
```

**Adaptation note**: Use `WinitData` during development to test compositor logic
without real hardware. Switch to `UdevData` for production.

---

## 2. Handler Trait Implementations

Smithay uses a "handler trait + delegate macro" pattern. You implement a `*Handler`
trait on your state type, then call a `delegate_*!` macro to wire up the Wayland
protocol dispatching.

### 2.1 Complete List of Anvil's Delegate Macros (26+)

```rust
delegate_compositor!(AnvilState<BackendData>);
delegate_shm!(AnvilState<BackendData>);
delegate_seat!(AnvilState<BackendData>);
delegate_data_device!(AnvilState<BackendData>);
delegate_output!(AnvilState<BackendData>);
delegate_primary_selection!(AnvilState<BackendData>);
delegate_data_control!(AnvilState<BackendData>);
delegate_xdg_shell!(AnvilState<BackendData>);
delegate_xdg_decoration!(AnvilState<BackendData>);
delegate_xdg_activation!(AnvilState<BackendData>);
delegate_xdg_foreign!(AnvilState<BackendData>);
delegate_layer_shell!(AnvilState<BackendData>);
delegate_presentation!(AnvilState<BackendData>);
delegate_fractional_scale!(AnvilState<BackendData>);
delegate_viewporter!(AnvilState<BackendData>);
delegate_tablet_manager!(AnvilState<BackendData>);
delegate_text_input_manager!(AnvilState<BackendData>);
delegate_input_method_manager!(AnvilState<BackendData>);
delegate_keyboard_shortcuts_inhibit!(AnvilState<BackendData>);
delegate_virtual_keyboard_manager!(AnvilState<BackendData>);
delegate_pointer_gestures!(AnvilState<BackendData>);
delegate_relative_pointer!(AnvilState<BackendData>);
delegate_pointer_constraints!(AnvilState<BackendData>);
delegate_security_context!(AnvilState<BackendData>);
delegate_xwayland_keyboard_grab!(AnvilState<BackendData>);
delegate_xwayland_shell!(AnvilState<BackendData>);
// Plus: single-pixel buffer, FIFO, commit timing, image capture
```

### 2.2 CompositorHandler (Core)

```rust
impl<BackendData: Backend> CompositorHandler for AnvilState<BackendData> {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.compositor_state
    }

    fn client_compositor_state<'a>(&self, client: &'a Client) -> &'a CompositorClientState {
        // Handles both regular clients and XWayland clients
        #[cfg(feature = "xwayland")]
        if let Some(state) = client.get_data::<XWaylandClientData>() {
            return &state.compositor_state;
        }
        if let Some(state) = client.get_data::<ClientState>() {
            return &state.compositor_state;
        }
        panic!("Unknown client data type")
    }

    fn new_surface(&mut self, surface: &WlSurface) { /* ... */ }

    fn commit(&mut self, surface: &WlSurface) {
        // 1. Process buffer handler
        on_commit_buffer_handler::<Self>(surface);
        // 2. Early import for GPU buffer access
        self.backend_data.early_import(surface);
        // 3. Handle window/popup/layer surface specifics
        // ...
    }
}
```

**Adaptation note**: The `commit` handler is where EXWM-VR would intercept surface
updates and forward frame metadata to Emacs via IPC (window title, app_id, geometry).

### 2.3 SeatHandler (Input Management)

```rust
impl<BackendData: Backend> SeatHandler for AnvilState<BackendData> {
    type KeyboardFocus = KeyboardFocusTarget;
    type PointerFocus = PointerFocusTarget;
    type TouchFocus = PointerFocusTarget;

    fn seat_state(&mut self) -> &mut SeatState<AnvilState<BackendData>> {
        &mut self.seat_state
    }

    fn focus_changed(&mut self, seat: &Seat<Self>, target: Option<&Self::KeyboardFocus>) {
        // Notify XDG activation state of focus changes
    }

    fn cursor_image(&mut self, _seat: &Seat<Self>, image: CursorImageStatus) {
        self.cursor_status = image;
    }
}
```

**Adaptation note**: Override `focus_changed` to send focus-change events to Emacs
so it can update its mode-line and buffer list.

### 2.4 XdgShellHandler (Window Lifecycle)

```rust
impl<BackendData: Backend> XdgShellHandler for AnvilState<BackendData> {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.xdg_shell_state
    }

    fn new_toplevel(&mut self, surface: ToplevelSurface) {
        // Create WindowElement, place at random position, add to space
        let window = WindowElement::new(surface);
        self.space.map_element(window, random_point, false);
    }

    fn new_popup(&mut self, surface: PopupSurface, positioner: PositionerState) {
        // Track popup, unconstrain position to stay on-screen
        self.popups.track_popup(PopupKind::Xdg(surface));
    }

    fn move_request(&mut self, surface: ToplevelSurface, seat: WlSeat, serial: Serial) {
        // Initiate pointer grab for window move
    }

    fn resize_request(&mut self, surface: ToplevelSurface, seat: WlSeat,
                      serial: Serial, edges: ResizeEdge) {
        // Initiate pointer grab for window resize
    }

    fn maximize_request(&mut self, surface: ToplevelSurface) {
        // Resize window to output geometry
    }

    fn fullscreen_request(&mut self, surface: ToplevelSurface, output: Option<WlOutput>) {
        // Set fullscreen state, resize to output
    }

    fn grab(&mut self, surface: PopupSurface, seat: WlSeat, serial: Serial) {
        // Establish popup grab for both keyboard and pointer
    }
}
```

**Adaptation note**: In EXWM-VR, `new_toplevel` should notify Emacs of the new
window so it can create a corresponding Emacs buffer. The placement logic should
defer to Emacs's window manager (tiling, floating, etc.) rather than random placement.

### 2.5 WlrLayerShellHandler (Panels, Overlays)

```rust
impl<BackendData: Backend> WlrLayerShellHandler for AnvilState<BackendData> {
    fn shell_state(&mut self) -> &mut WlrLayerShellState {
        &mut self.layer_shell_state
    }

    fn new_layer_surface(&mut self, surface: WlrLayerSurface,
                         wl_output: Option<WlOutput>, _layer: Layer,
                         namespace: String) {
        // Map layer surface to output, arrange layers
    }

    fn layer_destroyed(&mut self, surface: WlrLayerSurface) {
        // Clean up, re-arrange remaining layers
    }
}
```

### 2.6 DmabufHandler (GPU Buffer Import)

```rust
impl DmabufHandler for AnvilState<UdevData> {
    fn dmabuf_state(&mut self) -> &mut DmabufState {
        self.backend_data.dmabuf_state.as_mut().unwrap().0
    }

    fn dmabuf_imported(&mut self, _global: &DmabufGlobal,
                       dmabuf: Dmabuf, notifier: ImportNotifier) {
        // Try to import the dmabuf into the GPU
        // Signal success or failure via notifier
    }
}
```

**Adaptation note**: Critical for VR -- GPU buffers from client applications
(including Emacs pgtk rendering) are imported here for zero-copy compositing.

---

## 3. Event Loop Source Registration

Anvil uses calloop as its event loop. All I/O sources are registered via
`handle.insert_source()`. The calloop shared-data pattern passes `&mut AnvilState`
to every callback, eliminating Rc/RefCell.

### 3.1 Wayland Client Display (in AnvilState::init)

The Wayland display is not directly registered as a calloop source in modern
Smithay. Instead, `event_loop.dispatch()` drives the display processing.
The Wayland listening socket is created with:

```rust
if listen_on_socket {
    let socket_name = display_handle.add_socket_auto().unwrap();
    // socket_name is typically "wayland-0", "wayland-1", etc.
}
```

### 3.2 Libinput (udev backend)

```rust
event_loop.handle().insert_source(libinput_backend, move |mut event, _, data| {
    let state = &mut data.state;
    match event {
        InputEvent::DeviceAdded { ref mut device } => {
            // Track keyboards for LED state updates
            if device.has_capability(DeviceCapability::Keyboard) {
                state.backend_data.keyboards.push(device.clone());
            }
        }
        InputEvent::DeviceRemoved { ref device } => {
            // Remove from tracked keyboards
        }
        other => {
            // Delegate to generic input processing
            state.process_input_event(other);
        }
    }
});
```

**Adaptation note**: For EXWM-VR, add a parallel source for VR controller input
(e.g., from OpenXR runtime via a separate channel).

### 3.3 Session Notifier (VT switching, suspend/resume)

```rust
event_loop.handle().insert_source(notifier, move |event, &mut (), data| {
    match event {
        SessionEvent::PauseSession => {
            // Suspend all DRM backends
            for backend in data.state.backend_data.backends.values() {
                backend.drm.pause();
            }
        }
        SessionEvent::ActivateSession => {
            // Resume backends, reschedule renders
            for backend in data.state.backend_data.backends.values() {
                backend.drm.activate();
            }
        }
    }
});
```

### 3.4 Udev Device Hotplug

```rust
event_loop.handle().insert_source(udev_backend, move |event, _, data| {
    match event {
        UdevEvent::Added { device_id, path } => {
            data.device_added(device_id, path);
        }
        UdevEvent::Changed { device_id } => {
            data.device_changed(device_id);
        }
        UdevEvent::Removed { device_id } => {
            data.device_removed(device_id);
        }
    }
});
```

### 3.5 DRM VBlank Events

```rust
// Per-device registration when a DRM device is added:
event_loop.handle().insert_source(drm_notifier, move |event, metadata, data| {
    match event {
        DrmEvent::VBlank(crtc) => {
            // Trigger next frame render for this output
            data.schedule_render(node, crtc);
        }
        DrmEvent::Error(error) => {
            tracing::error!("DRM error: {}", error);
        }
    }
});
```

**Adaptation note**: For EXWM-VR, the Emacs IPC socket would be registered as an
additional calloop source:

```rust
// Proposed pattern for EXWM-VR
event_loop.handle().insert_source(
    Generic::new(ipc_socket, Interest::READ, Mode::Level),
    move |_event, _metadata, data| {
        // Read IPC messages from Emacs
        // Dispatch commands: focus window, move window, configure output, etc.
        data.state.handle_emacs_ipc();
        Ok(PostAction::Continue)
    },
);
```

### 3.6 Main Loop Structure (Winit Backend)

```rust
while state.running.load(Ordering::SeqCst) {
    // 1. Dispatch backend events (input, resize, etc.)
    let status = winit.dispatch_new_events(|event| { /* handle */ });

    // 2. Render frame with timing
    let frame_target = now + Duration::from_secs_f64(1_000.0 / refresh as f64);
    state.pre_repaint(&output, frame_target);
    let render_res = backend.bind().and_then(|(renderer, mut fb)| {
        render_output(&output, &space, elements, renderer, &mut fb,
                      &mut damage_tracker, age, show_window_preview)
    });
    state.post_repaint(&output);

    // 3. Dispatch calloop (processes Wayland client requests, timers, etc.)
    event_loop.dispatch(Some(Duration::from_millis(1)), &mut state)?;
}
```

---

## 4. Rendering Pipeline

### 4.1 Render Element Types

Anvil defines a hierarchy of render elements using Smithay's macro system:

```rust
// Low-level custom elements (cursor, debug overlays)
smithay::backend::renderer::element::render_elements! {
    pub CustomRenderElements<R> where R: ImportAll + ImportMem;
    Pointer=PointerRenderElement<R>,
    Surface=WaylandSurfaceRenderElement<R>,
    #[cfg(feature = "debug")]
    Fps=FpsElement<R::TextureId>,
}

// Top-level output composition
smithay::backend::renderer::element::render_elements! {
    pub OutputRenderElements<R, E> where R: ImportAll + ImportMem;
    Space=SpaceRenderElements<R, E>,      // Windows managed by Space
    Window=Wrap<E>,                        // Individual window elements
    Custom=CustomRenderElements<R>,        // Cursor, overlays
    Preview=CropRenderElement<             // Window preview thumbnails
        RelocateRenderElement<
            RescaleRenderElement<WindowRenderElement<R>>>>,
}
```

### 4.2 Element Assembly (output_elements)

```rust
pub fn output_elements<R>(
    output: &Output,
    space: &Space<WindowElement>,
    custom_elements: impl IntoIterator<Item = CustomRenderElements<R>>,
    renderer: &mut R,
    show_window_preview: bool,
) -> (Vec<OutputRenderElements<R, WindowRenderElement<R>>>, Color32F)
```

This function:
1. Checks for fullscreen surfaces (bypasses normal composition)
2. Collects custom elements (cursor, FPS counter)
3. Collects space elements (windows via `Space::render_elements_for_output`)
4. Optionally generates window preview tiles
5. Returns the element list and background clear color

### 4.3 Damage-Tracked Rendering (render_output)

```rust
pub fn render_output<'a, 'd, R>(
    output: &'a Output,
    space: &'a Space<WindowElement>,
    custom_elements: impl IntoIterator<Item = CustomRenderElements<R>>,
    renderer: &'a mut R,
    framebuffer: &'a mut R::Framebuffer<'_>,
    damage_tracker: &'d mut OutputDamageTracker,
    age: usize,
    show_window_preview: bool,
) -> Result<RenderOutputResult<'d>, OutputDamageTrackerError<R::Error>>
```

The `OutputDamageTracker` compares the current element list against previous frames
and only redraws damaged regions. The `age` parameter indicates how many frames old
the current front buffer is.

### 4.4 DRM Frame Submission (Udev Backend)

```rust
// In the udev backend's render path:
// 1. Pre-repaint timing
state.pre_repaint(&output, frame_target);

// 2. Assemble elements
let (elements, clear_color) = output_elements(
    &output, &space, custom_elements, renderer, show_window_preview);

// 3. Submit to DRM output
surface.drm_output.render_frame(renderer, &elements, clear_color, frame_mode);

// 4. Post-repaint feedback
state.post_repaint(&output);
```

The render scheduling uses a **60% frame delay** strategy: the compositor waits
for 60% of the frame duration before compositing, giving clients maximum time
to submit their buffers before the next vblank.

**Adaptation note**: For EXWM-VR, the Emacs pgtk window is just another Wayland
surface in the `Space`. It renders through the same pipeline. The VR HMD output
would need a separate render path (or DRM lease, see section 8).

---

## 5. Input Handling

### 5.1 Keyboard Event Pipeline

```rust
fn keyboard_key_to_action<B: InputBackend>(
    &mut self, evt: B::KeyboardKeyEvent
) -> KeyAction {
    let keycode = evt.key_code();
    let state = evt.state();
    let serial = SCOUNTER.next_serial();
    let keyboard = self.seat.get_keyboard().unwrap();

    // 1. Check exclusive layer shell surfaces first
    for layer in self.layer_shell_state.layer_surfaces().rev() {
        if layer.keyboard_interactivity == KeyboardInteractivity::Exclusive
            && (layer.layer == WlrLayer::Top || layer.layer == WlrLayer::Overlay)
        {
            // Route exclusively to this layer surface
        }
    }

    // 2. Process compositor shortcuts (if not inhibited)
    if let KeyState::Pressed = state {
        if !inhibited {
            let action = process_keyboard_shortcut(*modifiers, keysym);
            if action.is_some() {
                suppressed_keys.push(keysym);
                return FilterResult::Intercept(action);
            }
            return FilterResult::Forward;
        }
    }

    // 3. On release, forward only if not suppressed
    // ...
}
```

### 5.2 Focus Cascade

When pointer button is pressed, `update_keyboard_focus()` determines focus:

1. **Fullscreen surface** (if active on output) -- gets focus
2. **Overlay/Top layer shells** (if keyboard-interactive) -- get focus
3. **Regular windows** -- raised to top, get focus
4. **Bottom/Background layers** -- fallback

```rust
fn update_keyboard_focus(&mut self, location: Point<f64, Logical>, serial: Serial) {
    let output = self.space.outputs().find(|o| {
        self.space.output_geometry(o).unwrap().contains(location.to_i32_round())
    });
    // ... cascade through focus targets
}
```

### 5.3 Pointer Constraints (Lock/Confine)

For udev backend, pointer constraints (used by games, VR) are enforced:

```rust
with_pointer_constraint(&surface, &pointer, |constraint| match constraint {
    Some(constraint) if constraint.is_active() => {
        match &*constraint {
            PointerConstraint::Locked(_) => {
                // Only emit relative motion, no absolute position change
                pointer_locked = true;
            }
            PointerConstraint::Confined(confine) => {
                // Reject movement outside confine region
                pointer_confined = true;
                confine_region = confine.region().cloned();
            }
        }
    }
    _ => {}
});
```

### 5.4 Virtual Keyboard

```rust
impl<BackendData: Backend> VirtualKeyboardHandler for AnvilState<BackendData> {
    fn on_keyboard_event(
        &mut self, keycode: Keycode, state: KeyState,
        time: u32, keyboard: KeyboardHandle<Self>,
    ) {
        let serial = SERIAL_COUNTER.next_serial();
        keyboard.input(self, keycode, state, serial, time, |_, _, _| {
            FilterResult::Forward::<bool>
        });
    }
}
```

**Adaptation note**: Virtual keyboard is how Emacs can inject keystrokes into
Wayland clients. EXWM-VR should use this protocol to forward Emacs keybindings
to focused client surfaces.

---

## 6. Multi-Output Support

### 6.1 Per-Device Backend Data

Each DRM device (GPU) gets its own `BackendData` in a HashMap:

```rust
backends: HashMap<DrmNode, BackendData>
```

Each connected output (monitor) is tracked per-CRTC:

```rust
struct SurfaceData {
    device_id: DrmNode,
    render_node: Option<DrmNode>,
    output: Output,
    drm_output: DrmOutput</* ... */>,
    dmabuf_feedback: Option<SurfaceDmabufFeedback>,
    last_presentation_time: Option<Time<Monotonic>>,
    vblank_throttle_timer: Option<RegistrationToken>,
}
```

### 6.2 Connector Lifecycle

Anvil uses `DrmOutputManager` with a scanner to detect connector changes:

- **connector_connected**: Opens connector, finds CRTC, creates output, registers
  with `Space`, arranges outputs side-by-side
- **connector_disconnected**: Removes output from Space, repositions orphaned
  windows to remaining outputs
- **device_removed**: Disconnects all connectors on the device, cleans up GPU

### 6.3 Output Arrangement

Outputs are arranged left-to-right by default. When an output is removed,
`fixup_positions()` repositions windows that were on the removed output:

```rust
fn fixup_positions(&mut self) {
    // For each window, if its output no longer exists,
    // move it to the primary output or first available output
}
```

### 6.4 Independent Render Scheduling

Each output has independent vblank timing. The DRM event notifier fires per-CRTC,
and each output renders independently. This is important for mixed-refresh-rate
setups (e.g., 60Hz monitor + 90Hz VR HMD).

**Adaptation note**: EXWM-VR needs per-output render scheduling. The VR HMD
will have a much higher refresh rate (90-120Hz) than desktop monitors (60Hz).
Each output's render loop should be independent.

---

## 7. Minimum Trait Set for a Functional Compositor

Based on analysis of anvil and Smithay's documentation, here is the absolute
minimum set of handler traits needed for a compositor that can display windows:

### 7.1 Tier 1: Mandatory (compositor will not function without these)

| Trait | Delegate Macro | Purpose |
|-------|---------------|---------|
| `CompositorHandler` | `delegate_compositor!` | Surface lifecycle (create, commit, destroy) |
| `ShmHandler` | `delegate_shm!` | Shared memory buffer allocation (software rendering) |
| `SeatHandler` | `delegate_seat!` | Input device management (keyboard, pointer, touch) |
| `OutputHandler` | `delegate_output!` | Output advertisement to clients |
| `BufferHandler` | (part of compositor) | Buffer lifecycle management |

### 7.2 Tier 2: Required for Usable Shell

| Trait | Delegate Macro | Purpose |
|-------|---------------|---------|
| `XdgShellHandler` | `delegate_xdg_shell!` | Window management (toplevel, popup) |
| `DataDeviceHandler` | `delegate_data_device!` | Clipboard, drag-and-drop |

### 7.3 Tier 3: Needed for Real-World Usage

| Trait | Delegate Macro | Purpose |
|-------|---------------|---------|
| `DmabufHandler` | `delegate_dmabuf!` | GPU buffer import (zero-copy) |
| `WlrLayerShellHandler` | `delegate_layer_shell!` | Panels, overlays, wallpapers |
| `XdgDecorationHandler` | `delegate_xdg_decoration!` | Server/client-side decoration negotiation |
| `PresentationHandler` | `delegate_presentation!` | Frame timing feedback |
| `FractionalScaleHandler` | `delegate_fractional_scale!` | HiDPI scaling |
| `ViewporterHandler` | `delegate_viewporter!` | Surface viewport/crop |

### 7.4 Tier 4: VR-Specific

| Trait | Delegate Macro | Purpose |
|-------|---------------|---------|
| `DrmLeaseHandler` | `delegate_drm_lease!` | VR headset DRM lease protocol |
| `PointerConstraintsHandler` | `delegate_pointer_constraints!` | Pointer lock/confine for immersive apps |
| `RelativePointerHandler` | `delegate_relative_pointer!` | Relative motion for VR/3D |

### 7.5 Minimal Skeleton

```rust
use smithay::delegate_compositor;
use smithay::delegate_shm;
use smithay::delegate_seat;
use smithay::delegate_output;
use smithay::delegate_xdg_shell;
use smithay::delegate_data_device;

struct ExwmVrState {
    compositor_state: CompositorState,
    shm_state: ShmState,
    seat_state: SeatState<Self>,
    output_state: OutputManagerState,
    xdg_shell_state: XdgShellState,
    data_device_state: DataDeviceState,
    space: Space<WindowElement>,
    seat: Seat<Self>,
    // ... EXWM-VR specific fields
}

impl CompositorHandler for ExwmVrState { /* ... */ }
impl ShmHandler for ExwmVrState { /* ... */ }
impl SeatHandler for ExwmVrState { /* ... */ }
impl OutputHandler for ExwmVrState { /* ... */ }
impl XdgShellHandler for ExwmVrState { /* ... */ }
impl DataDeviceHandler for ExwmVrState { /* ... */ }
impl BufferHandler for ExwmVrState { /* ... */ }

delegate_compositor!(ExwmVrState);
delegate_shm!(ExwmVrState);
delegate_seat!(ExwmVrState);
delegate_output!(ExwmVrState);
delegate_xdg_shell!(ExwmVrState);
delegate_data_device!(ExwmVrState);
```

---

## 8. DRM Leasing for VR

### 8.1 How DRM Leases Work

DRM leasing allows the compositor to grant a VR runtime (Monado, SteamVR)
exclusive control over a VR headset's display connector. The kernel's
`drmModeCreateLease()` transfers ownership of specific connectors, CRTCs,
and planes to the lessee.

Key concepts:
- **Non-desktop connectors**: VR HMDs are identified via EDID as "non-desktop"
  outputs. The compositor should not render its desktop to these.
- **wp_drm_lease_v1 protocol**: Wayland protocol extension that lets clients
  request DRM leases for available non-desktop connectors.
- **Lease lifecycle**: Compositor advertises leasable connectors; client requests
  a lease; compositor grants it; the leased connector "disappears" from the
  compositor's perspective until the lease ends.

### 8.2 Anvil's DrmLeaseHandler

```rust
impl DrmLeaseHandler for AnvilState<UdevData> {
    fn drm_lease_state(&mut self, node: DrmNode) -> &mut DrmLeaseState {
        // Return lease state for the given DRM node
    }

    fn lease_request(&mut self, node: DrmNode, request: DrmLeaseRequest)
        -> Result<DrmLeaseBuilder, LeaseRejected>
    {
        // Validate which connectors can be leased
        // Non-desktop connectors are always leasable
        // Build and return a lease
    }

    fn new_active_lease(&mut self, node: DrmNode, lease: DrmLease) {
        // Track active lease
    }

    fn lease_destroyed(&mut self, node: DrmNode, lease_id: u32) {
        // Clean up, potentially re-scan connectors
    }
}
```

**Adaptation note**: For EXWM-VR, this is the primary VR integration point.
When a VR runtime like Monado connects as a Wayland client and requests a DRM
lease, the compositor grants it. The VR runtime then drives the HMD directly
at 90+ Hz with minimal latency. The compositor continues rendering Emacs and
other windows to desktop monitors independently.

---

## 9. Adaptation Strategy for EXWM-VR

### 9.1 Architecture Overview

```
+------------------+     Unix Socket IPC     +------------------+
|  Emacs (pgtk)    | <--------------------> |  EXWM-VR         |
|  - Window mgmt   |    Commands/Events      |  Compositor      |
|  - Key bindings  |                         |  (Smithay 0.7)   |
|  - Buffer list   |                         |                  |
+------------------+                         |  - Wayland proto |
       |                                     |  - DRM/KMS       |
       | Wayland protocol                    |  - Input (libinput)|
       +------------------------------------>|  - DRM lease (VR)|
                                             +------------------+
                                                    |
                              +---------------------+-------------------+
                              |                                         |
                      +-------+-------+                         +-------+-------+
                      | Desktop Output|                         | VR HMD Output |
                      | (60Hz, DRM)   |                         | (90Hz, leased)|
                      +---------------+                         +---------------+
```

### 9.2 Key Differences from Anvil

| Aspect | Anvil | EXWM-VR |
|--------|-------|---------|
| Window placement | Random position | Emacs-directed (tiling/floating via IPC) |
| Focus policy | Click-to-focus | Emacs-directed (follow Emacs buffer selection) |
| Keyboard routing | Compositor shortcuts + forward | Most keys to Emacs; Emacs decides forwarding |
| Shell | Built-in XDG shell handling | Emacs is the shell; compositor is a thin servant |
| VR | Not primary concern | Core feature via DRM lease |
| IPC | None | Unix socket to Emacs for bidirectional control |

### 9.3 Implementation Phases

**Phase 1 -- Minimal compositor**: Implement Tier 1+2 traits, create a `Space`,
accept Wayland clients, render to a winit window for testing.

**Phase 2 -- Emacs IPC**: Add Unix socket calloop source, define IPC protocol
(JSON or protobuf), let Emacs control window placement and focus.

**Phase 3 -- DRM backend**: Switch to udev backend, handle real outputs with
libinput, add session management.

**Phase 4 -- VR integration**: Implement `DrmLeaseHandler`, identify non-desktop
connectors, advertise them for VR runtimes.

### 9.4 Calloop Source Registration Summary

The EXWM-VR compositor needs these calloop sources:

```rust
// 1. Libinput (keyboard, pointer, touch)
handle.insert_source(libinput_backend, |event, _, state| { ... });

// 2. Session notifier (VT switch, suspend/resume)
handle.insert_source(session_notifier, |event, _, state| { ... });

// 3. Udev hotplug (monitor/HMD connect/disconnect)
handle.insert_source(udev_backend, |event, _, state| { ... });

// 4. DRM vblank (per output, for render scheduling)
handle.insert_source(drm_notifier, |event, _, state| { ... });

// 5. Emacs IPC socket (NEW -- bidirectional command channel)
handle.insert_source(ipc_source, |event, _, state| { ... });

// 6. Timer sources (render scheduling, idle timeout)
handle.insert_source(timer, |event, _, state| { ... });
```

---

## References

- Smithay repository: https://github.com/Smithay/smithay
- Anvil source: https://github.com/Smithay/smithay/tree/master/anvil
- Anvil state.rs: https://github.com/Smithay/smithay/blob/master/anvil/src/state.rs
- Anvil udev.rs: https://github.com/Smithay/smithay/blob/master/anvil/src/udev.rs
- Anvil input_handler.rs: https://github.com/Smithay/smithay/blob/master/anvil/src/input_handler.rs
- Anvil render.rs: https://github.com/Smithay/smithay/blob/master/anvil/src/render.rs
- Anvil shell/mod.rs: https://github.com/Smithay/smithay/blob/master/anvil/src/shell/mod.rs
- Anvil shell/xdg.rs: https://github.com/Smithay/smithay/blob/master/anvil/src/shell/xdg.rs
- Anvil winit.rs: https://github.com/Smithay/smithay/blob/master/anvil/src/winit.rs
- Smithay API docs: https://docs.rs/smithay
- Smithay wayland module: https://docs.rs/smithay/latest/smithay/wayland/index.html
- DRM leasing for VR: https://drewdevault.com/2019/08/09/DRM-leasing-and-VR-for-Wayland.html
- Calloop: https://smithay.github.io/calloop-v-0-7.html
- Smithay v0.3 announcement: https://smithay.github.io/smithay-v-0-3.html
