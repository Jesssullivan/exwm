# Portable Logic Extraction Plan

Per-module analysis of X11-coupled vs portable logic for the 5 transform candidates.

## exwm-workspace.el (1764 lines) → ewwm-workspace.el

**Estimated portable: ~40%**

| Function/Area | Portable | Notes |
|---------------|----------|-------|
| Workspace list management | Yes | List operations, indexing |
| `exwm-workspace-switch` logic | Yes | Workspace selection, hook dispatch |
| `exwm-workspace-number` config | Yes | Configuration, bounds checking |
| Buffer-to-workspace assignment | Yes | `exwm--id-buffer-alist` lookup pattern |
| Workspace name/index mapping | Yes | `exwm-workspace-index-map` |
| Switch history tracking | Yes | History ring for workspace switching |
| Frame geometry via root window | No | `xcb:ConfigureWindow` on workspace frames |
| `xcb:MapWindow`/`xcb:UnmapWindow` | No | X11 window visibility |
| Strut handling (`_NET_WM_STRUT`) | No | EWMH X11 property |
| Minibuffer workspace attachment | Partial | Concept portable, X11 frame ops not |
| RandR workspace-to-output mapping | No | Replaced by `wl_output` in compositor |

## exwm-input.el (1216 lines) → ewwm-input.el

**Estimated portable: ~35%**

| Function/Area | Portable | Notes |
|---------------|----------|-------|
| `exwm-input-set-key` | Yes | Key binding registration |
| `exwm-input-prefix-keys` config | Yes | Prefix key list |
| Simulation key mapping | Yes | Key translation table |
| Line-mode/char-mode concept | Yes | Modal input switching |
| `exwm-input--global-keys` dispatch | Yes | Keymap lookup and dispatch |
| `xcb:GrabKey`/`xcb:UngrabKey` | No | Replaced by compositor key interception via IPC |
| `xcb:AllowEvents` | No | X11 event replay |
| X11 keysym translation | No | Replaced by xkbcommon in compositor |
| `xcb:SetInputFocus` | No | Focus set via compositor IPC |
| Mouse move/resize via X11 grab | No | Replaced by compositor pointer handling |
| Echo area timer/focus update | Partial | Timer logic portable, X11 focus ops not |

## exwm-manage.el (834 lines) → ewwm-manage.el

**Estimated portable: ~30%**

| Function/Area | Portable | Notes |
|---------------|----------|-------|
| Buffer creation on window manage | Yes | `generate-new-buffer`, buffer setup |
| Buffer cleanup on unmanage | Yes | `kill-buffer`, alist cleanup |
| Class/title/instance tracking | Yes | Buffer-local variable pattern |
| Manage rules framework | Yes | Rule matching, predicate dispatch |
| `xcb:MapRequest` handling | No | Replaced by IPC surface-create event |
| `xcb:PropertyNotify` | No | Replaced by IPC property-change event |
| ICCCM WM_STATE management | No | X11-only protocol |
| EWMH property reading/writing | No | X11-only protocol |
| Window type classification | Partial | Concept portable, X11 atoms not |
| Transient-for tracking | Partial | Concept exists in xdg-shell parent |

## exwm-layout.el (664 lines) → ewwm-layout.el

**Estimated portable: ~45%**

| Function/Area | Portable | Notes |
|---------------|----------|-------|
| Tiling algorithm (geometry calc) | Yes | Pure math: divide screen area among windows |
| Fullscreen toggle logic | Yes | State tracking, mode switching |
| Layout mode switching | Yes | Tiling/monocle/floating selection |
| Zoom/enlarge/shrink | Yes | Proportional adjustment |
| `xcb:ConfigureWindow` | No | Geometry sent via IPC to compositor |
| `xcb:MapWindow`/`xcb:UnmapWindow` | No | Visibility via IPC |
| Border width via X11 | No | Border rendering in compositor |
| Show/hide logic | Partial | Decision portable, X11 ops not |

## exwm-floating.el (761 lines) → ewwm-floating.el

**Estimated portable: ~25%**

| Function/Area | Portable | Notes |
|---------------|----------|-------|
| Floating toggle concept | Yes | State tracking, buffer-local flag |
| Position/size tracking | Yes | Geometry storage in buffer-locals |
| Float/unfloat hooks | Yes | Hook dispatch pattern |
| `xcb:ReparentWindow` | No | No reparenting in Wayland |
| Container window creation | No | X11 pattern, not needed |
| Mouse move/resize via X11 grab | No | Handled by compositor |
| Border drawing | No | Compositor renders decorations |
| Frame creation for float | Partial | Emacs frame concept portable, X11 frame ops not |
