//! IPC message dispatch — parse s-expressions and route to handlers.

use crate::state::EwwmState;
use lexpr::Value;
use tracing::{debug, warn};

/// Parse an s-expression message and dispatch to the appropriate handler.
/// Returns an optional response string (s-expression).
pub fn handle_message(state: &mut EwwmState, client_id: u64, raw: &str) -> Option<String> {
    let value = match lexpr::from_str(raw) {
        Ok(v) => v,
        Err(e) => {
            warn!(client_id, "malformed s-expression: {}", e);
            return Some(error_response(0, &format!("malformed s-expression: {e}")));
        }
    };

    let msg_type = get_keyword(&value, "type");
    let msg_id = get_int(&value, "id").unwrap_or(0);

    // Check authentication (hello must be first message)
    let is_authenticated = state
        .ipc_server
        .clients
        .get(&client_id)
        .map(|c| c.authenticated)
        .unwrap_or(false);

    match msg_type.as_deref() {
        Some("hello") => handle_hello(state, client_id, msg_id, &value),
        _ if !is_authenticated => Some(error_response(msg_id, "hello handshake required")),
        Some("ping") => handle_ping(state, msg_id, &value),
        Some("surface-list") => handle_surface_list(state, msg_id),
        Some("surface-focus") => handle_surface_focus(state, msg_id, &value),
        Some("surface-close") => handle_surface_close(state, msg_id, &value),
        Some("surface-move") => handle_surface_move(state, msg_id, &value),
        Some("surface-resize") => handle_surface_resize(state, msg_id, &value),
        Some("surface-fullscreen") => handle_surface_fullscreen(state, msg_id, &value),
        Some("surface-float") => handle_surface_float(state, msg_id, &value),
        Some("workspace-switch") => handle_workspace_switch(state, msg_id, &value),
        Some("workspace-list") => handle_workspace_list(state, msg_id),
        Some("workspace-move-surface") => handle_workspace_move_surface(state, msg_id, &value),
        Some("layout-set") => handle_layout_set(state, msg_id, &value),
        Some("layout-cycle") => handle_layout_cycle(state, msg_id),
        Some("key-grab") => handle_key_grab(state, msg_id, &value),
        Some("key-ungrab") => handle_key_ungrab(state, msg_id, &value),
        Some("vr-status") => handle_vr_status(state, msg_id),
        Some("vr-set-reference-space") => handle_vr_set_reference_space(state, msg_id, &value),
        Some("vr-restart") => handle_vr_restart(state, msg_id),
        Some("vr-get-frame-timing") => handle_vr_get_frame_timing(state, msg_id),
        Some("vr-scene-status") => handle_vr_scene_status(state, msg_id),
        Some("vr-scene-set-layout") => handle_vr_scene_set_layout(state, msg_id, &value),
        Some("vr-scene-set-ppu") => handle_vr_scene_set_ppu(state, msg_id, &value),
        Some("vr-scene-set-background") => handle_vr_scene_set_background(state, msg_id, &value),
        Some("vr-scene-set-projection") => handle_vr_scene_set_projection(state, msg_id, &value),
        Some("vr-scene-focus") => handle_vr_scene_focus(state, msg_id, &value),
        Some("vr-scene-move") => handle_vr_scene_move(state, msg_id, &value),
        Some("vr-display-info") => handle_vr_display_info(state, msg_id),
        Some("vr-display-set-mode") => handle_vr_display_set_mode(state, msg_id, &value),
        Some("vr-display-select-hmd") => handle_vr_display_select_hmd(state, msg_id, &value),
        Some("vr-display-set-refresh-rate") => handle_vr_display_set_refresh_rate(state, msg_id, &value),
        Some("vr-display-auto-detect") => handle_vr_display_auto_detect(state, msg_id),
        Some("vr-display-list-connectors") => handle_vr_display_list_connectors(state, msg_id),
        Some("vr-pointer-state") => handle_vr_pointer_state(state, msg_id),
        Some("vr-click") => handle_vr_click(state, msg_id, &value),
        Some("vr-grab") => handle_vr_grab(state, msg_id),
        Some("vr-grab-release") => handle_vr_grab_release(state, msg_id),
        Some("vr-adjust-depth") => handle_vr_adjust_depth(state, msg_id, &value),
        Some("vr-set-follow") => handle_vr_set_follow(state, msg_id, &value),
        Some("vr-set-gaze-offset") => handle_vr_set_gaze_offset(state, msg_id, &value),
        Some("vr-calibrate-confirm") => handle_vr_calibrate_confirm(state, msg_id),
        // Eye tracking (Week 11)
        Some("gaze-status") => handle_gaze_status(state, msg_id),
        Some("gaze-set-source") => handle_gaze_set_source(state, msg_id, &value),
        Some("gaze-calibrate-start") => handle_gaze_calibrate_start(state, msg_id, &value),
        Some("gaze-calibrate-point") => handle_gaze_calibrate_point(state, msg_id, &value),
        Some("gaze-set-visualization") => handle_gaze_set_visualization(state, msg_id, &value),
        Some("gaze-set-smoothing") => handle_gaze_set_smoothing(state, msg_id, &value),
        Some("gaze-simulate") => handle_gaze_simulate(state, msg_id, &value),
        Some("gaze-health") => handle_gaze_health(state, msg_id),
        // Gaze focus (Week 12)
        Some("gaze-focus-config") => handle_gaze_focus_config(state, msg_id),
        Some("gaze-focus-status") => handle_gaze_focus_status(state, msg_id),
        Some("gaze-focus-set-policy") => handle_gaze_focus_set_policy(state, msg_id, &value),
        Some("gaze-focus-set-dwell") => handle_gaze_focus_set_dwell(state, msg_id, &value),
        Some("gaze-focus-set-cooldown") => handle_gaze_focus_set_cooldown(state, msg_id, &value),
        Some("gaze-focus-analytics") => handle_gaze_focus_analytics(state, msg_id),
        Some("gaze-focus-back") => handle_gaze_focus_back(state, msg_id),
        // Blink/wink (Week 13)
        Some("wink-status") => handle_wink_status(state, msg_id),
        Some("wink-config") => handle_wink_config(state, msg_id),
        Some("wink-calibrate-start") => handle_wink_calibrate_start(state, msg_id, &value),
        Some("wink-set-confidence") => handle_wink_set_confidence(state, msg_id, &value),
        // Gaze zones (Week 13)
        Some("gaze-zone-status") => handle_gaze_zone_status(state, msg_id),
        Some("gaze-zone-config") => handle_gaze_zone_config(state, msg_id),
        Some("gaze-zone-set-dwell") => handle_gaze_zone_set_dwell(state, msg_id, &value),
        // Fatigue (Week 13)
        Some("fatigue-status") => handle_fatigue_status(state, msg_id),
        Some("fatigue-config") => handle_fatigue_config(state, msg_id),
        Some("fatigue-metrics") => handle_fatigue_metrics(state, msg_id),
        Some("fatigue-reset") => handle_fatigue_reset(state, msg_id),
        // Headless backend (Week 16)
        Some("headless-status") => handle_headless_status(state, msg_id),
        Some("headless-set-resolution") => handle_headless_set_resolution(state, msg_id, &value),
        Some("headless-add-output") => handle_headless_add_output(state, msg_id),
        Some("headless-remove-output") => handle_headless_remove_output(state, msg_id),
        // Auto-type & secure input (Week 14)
        Some("autotype") => handle_autotype(state, msg_id, &value),
        Some("autotype-status") => handle_autotype_status(state, msg_id),
        Some("autotype-abort") => handle_autotype_abort(state, msg_id),
        Some("autotype-pause") => handle_autotype_pause(state, msg_id, &value),
        Some("autotype-resume") => handle_autotype_resume(state, msg_id),
        Some("secure-input-mode") => handle_secure_input_mode(state, msg_id, &value),
        Some("secure-input-status") => handle_secure_input_status(state, msg_id),
        Some("gaze-away-monitor") => handle_gaze_away_monitor(state, msg_id, &value),
        // Gaze scroll & link hints (Week 17)
        Some("gaze-scroll-status") => handle_gaze_scroll_status(state, msg_id),
        Some("gaze-scroll-config") => handle_gaze_scroll_config(state, msg_id, &value),
        Some("gaze-scroll-set-speed") => handle_gaze_scroll_set_speed(state, msg_id, &value),
        Some("link-hints-load") => handle_link_hints_load(state, msg_id, &value),
        Some("link-hints-confirm") => handle_link_hints_confirm(state, msg_id),
        Some("link-hints-clear") => handle_link_hints_clear(state, msg_id),
        Some("link-hints-status") => handle_link_hints_status(state, msg_id),
        // Hand tracking (Week 18)
        Some("hand-tracking-status") => handle_hand_tracking_status(state, msg_id),
        Some("hand-tracking-config") => handle_hand_tracking_config(state, msg_id, &value),
        Some("hand-tracking-joint") => handle_hand_tracking_joint(state, msg_id, &value),
        Some("hand-tracking-skeleton") => handle_hand_tracking_skeleton(state, msg_id, &value),
        Some("hand-tracking-distance") => handle_hand_tracking_distance(state, msg_id, &value),
        // Gesture (Week 18)
        Some("gesture-status") => handle_gesture_status(state, msg_id),
        Some("gesture-config") => handle_gesture_config(state, msg_id, &value),
        Some("gesture-bind") => handle_gesture_bind(state, msg_id, &value),
        Some("gesture-unbind") => handle_gesture_unbind(state, msg_id, &value),
        Some("gesture-bindings") => handle_gesture_bindings(state, msg_id),
        // Virtual keyboard (Week 18)
        Some("keyboard-show") => handle_keyboard_show(state, msg_id),
        Some("keyboard-hide") => handle_keyboard_hide(state, msg_id),
        Some("keyboard-toggle") => handle_keyboard_toggle(state, msg_id),
        Some("keyboard-layout") => handle_keyboard_layout(state, msg_id, &value),
        Some("keyboard-status") => handle_keyboard_status(state, msg_id),
        Some(other) => Some(error_response(
            msg_id,
            &format!("unknown message type: {other}"),
        )),
        None => Some(error_response(msg_id, "missing :type field")),
    }
}

// ── Handlers ────────────────────────────────────────────────

fn handle_hello(
    state: &mut EwwmState,
    client_id: u64,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let version = get_int(value, "version").unwrap_or(0);
    if version != 1 {
        return Some(error_response(
            msg_id,
            &format!("unsupported protocol version: {version}"),
        ));
    }

    let client_name = get_string(value, "client").unwrap_or_default();
    debug!(client_id, client_name, "hello handshake");

    if let Some(client) = state.ipc_server.clients.get_mut(&client_id) {
        client.authenticated = true;
    }

    let vr_flag = if state.vr_state.enabled { "t" } else { "nil" };
    Some(format!(
        "(:type :hello :id {} :version 1 :server \"ewwm-compositor\" :features (:xwayland t :vr {}))",
        msg_id, vr_flag
    ))
}

fn handle_ping(_state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let client_ts = get_int(value, "timestamp").unwrap_or(0);
    let server_ts = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0);

    Some(format!(
        "(:type :response :id {} :status :ok :client-timestamp {} :server-timestamp {})",
        msg_id, client_ts, server_ts
    ))
}

fn handle_surface_list(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let mut surfaces_sexp = String::from("(");

    for (id, data) in &state.surfaces {
        let app_id = data.app_id.as_deref().unwrap_or("");
        let title = data.title.as_deref().unwrap_or("");
        let x11_flag = if data.is_x11 { "t" } else { "nil" };
        let x11_class = data.x11_class.as_deref().unwrap_or("");
        let x11_instance = data.x11_instance.as_deref().unwrap_or("");
        // Get geometry from Space
        let geo = state
            .space
            .elements()
            .find_map(|w| {
                state.space.element_geometry(w).map(|g| {
                    (g.loc.x, g.loc.y, g.size.w, g.size.h)
                })
            })
            .unwrap_or((0, 0, 0, 0));

        surfaces_sexp.push_str(&format!(
            "(:id {} :app-id \"{}\" :title \"{}\" :x11 {} :x11-class \"{}\" :x11-instance \"{}\" :geometry (:x {} :y {} :w {} :h {}) :workspace {} :floating {} :focused nil)",
            id,
            escape_string(app_id),
            escape_string(title),
            x11_flag,
            escape_string(x11_class),
            escape_string(x11_instance),
            geo.0, geo.1, geo.2, geo.3,
            data.workspace,
            if data.floating { "t" } else { "nil" },
        ));
    }
    surfaces_sexp.push(')');

    Some(format!(
        "(:type :response :id {} :status :ok :surfaces {})",
        msg_id, surfaces_sexp
    ))
}

fn handle_surface_focus(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    if !state.surfaces.contains_key(&surface_id) {
        return Some(error_response(
            msg_id,
            &format!("unknown surface: {surface_id}"),
        ));
    }

    // Find the Window in space and raise it
    let window = state
        .space
        .elements()
        .find(|w| {
            // For now, find by position in the surfaces map
            true // TODO: proper window-to-surface-id correlation
        })
        .cloned();

    if let Some(w) = window {
        state.space.raise_element(&w, true);
        let keyboard = state.seat.get_keyboard().unwrap();
        let serial = smithay::utils::SERIAL_COUNTER.next_serial();
        let wl_surface = w.toplevel().map(|t| t.wl_surface().clone());
        if let Some(surface) = wl_surface {
            keyboard.set_focus(state, Some(surface), serial);
        }
    }

    Some(ok_response(msg_id))
}

fn handle_surface_close(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    if !state.surfaces.contains_key(&surface_id) {
        return Some(error_response(
            msg_id,
            &format!("unknown surface: {surface_id}"),
        ));
    }

    // Send close request to the toplevel
    // TODO: correlate surface_id to Window more robustly
    for w in state.space.elements() {
        if let Some(toplevel) = w.toplevel() {
            toplevel.send_close();
            break;
        }
    }

    Some(ok_response(msg_id))
}

fn handle_surface_move(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };
    let x = get_int(value, "x").unwrap_or(0) as i32;
    let y = get_int(value, "y").unwrap_or(0) as i32;

    if !state.surfaces.contains_key(&surface_id) {
        return Some(error_response(
            msg_id,
            &format!("unknown surface: {surface_id}"),
        ));
    }

    // Find window and remap at new location
    let window = state.space.elements().next().cloned(); // TODO: proper lookup
    if let Some(w) = window {
        state.space.map_element(w, (x, y), false);
    }

    Some(ok_response(msg_id))
}

fn handle_surface_resize(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };
    let w = get_int(value, "w").unwrap_or(800) as i32;
    let h = get_int(value, "h").unwrap_or(600) as i32;

    if !state.surfaces.contains_key(&surface_id) {
        return Some(error_response(
            msg_id,
            &format!("unknown surface: {surface_id}"),
        ));
    }

    // Resize via pending state
    for win in state.space.elements() {
        if let Some(toplevel) = win.toplevel() {
            toplevel.with_pending_state(|s| {
                s.size = Some(smithay::utils::Size::from((w, h)));
            });
            toplevel.send_pending_configure();
            break; // TODO: proper lookup
        }
    }

    Some(ok_response(msg_id))
}

fn handle_surface_fullscreen(
    _state: &mut EwwmState,
    msg_id: i64,
    _value: &Value,
) -> Option<String> {
    // Stub — fullscreen toggle
    Some(ok_response(msg_id))
}

fn handle_surface_float(_state: &mut EwwmState, msg_id: i64, _value: &Value) -> Option<String> {
    // Stub — float toggle
    Some(ok_response(msg_id))
}

fn handle_workspace_switch(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let workspace = get_int(value, "workspace").unwrap_or(0);
    debug!(workspace, "workspace switch");
    state.active_workspace = workspace as usize;
    Some(ok_response(msg_id))
}

fn handle_workspace_list(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let mut workspaces = String::from("(");
    for i in 0..4 {
        let active = if i == state.active_workspace {
            "t"
        } else {
            "nil"
        };
        workspaces.push_str(&format!(
            "(:index {} :name \"{}\" :surfaces () :active {})",
            i,
            i + 1,
            active
        ));
    }
    workspaces.push(')');

    Some(format!(
        "(:type :response :id {} :status :ok :workspaces {})",
        msg_id, workspaces
    ))
}

fn handle_workspace_move_surface(
    _state: &mut EwwmState,
    msg_id: i64,
    _value: &Value,
) -> Option<String> {
    // Stub — move surface to workspace
    Some(ok_response(msg_id))
}

fn handle_layout_set(_state: &mut EwwmState, msg_id: i64, _value: &Value) -> Option<String> {
    // Stub — set layout algorithm
    Some(ok_response(msg_id))
}

fn handle_layout_cycle(_state: &mut EwwmState, msg_id: i64) -> Option<String> {
    // Stub — cycle layout
    Some(ok_response(msg_id))
}

fn handle_key_grab(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let key = match get_string(value, "key") {
        Some(k) => k,
        None => return Some(error_response(msg_id, "missing :key")),
    };
    debug!(key, "registering key grab");
    state.grabbed_keys.insert(key);
    Some(ok_response(msg_id))
}

fn handle_key_ungrab(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let key = match get_string(value, "key") {
        Some(k) => k,
        None => return Some(error_response(msg_id, "missing :key")),
    };
    debug!(key, "removing key grab");
    state.grabbed_keys.remove(&key);
    Some(ok_response(msg_id))
}

fn handle_vr_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let session = state.vr_state.session_state_str();
    let hmd = state.vr_state.hmd_name();
    let headless = if state.vr_state.is_headless() { "t" } else { "nil" };
    let frame_stats = state.vr_state.frame_stats_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :session :{} :hmd \"{}\" :headless {} :frame-stats {})",
        msg_id, session, escape_string(hmd), headless, frame_stats
    ))
}

fn handle_vr_set_reference_space(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let space_type = get_keyword(value, "space-type");
    match space_type.as_deref() {
        Some("local") => {
            state.vr_state.set_reference_space(crate::vr::ReferenceSpaceType::Local);
            Some(ok_response(msg_id))
        }
        Some("stage") => {
            state.vr_state.set_reference_space(crate::vr::ReferenceSpaceType::Stage);
            Some(ok_response(msg_id))
        }
        Some("view") => {
            state.vr_state.set_reference_space(crate::vr::ReferenceSpaceType::View);
            Some(ok_response(msg_id))
        }
        _ => Some(error_response(msg_id, "invalid :space-type (use local, stage, or view)")),
    }
}

fn handle_vr_restart(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.shutdown();
    // Re-initialize is deferred to the next frame tick
    Some(ok_response(msg_id))
}

fn handle_vr_get_frame_timing(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let timing = state.vr_state.frame_stats_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :timing {})",
        msg_id, timing
    ))
}

fn handle_vr_scene_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.scene.scene_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :scene {})",
        msg_id, sexp
    ))
}

fn handle_vr_scene_set_layout(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::scene::VrLayoutMode;

    let layout = get_keyword(value, "layout");
    let mode = match layout.as_deref() {
        Some("arc") => VrLayoutMode::Arc,
        Some("stack") => VrLayoutMode::Stack,
        Some("freeform") => VrLayoutMode::Freeform,
        Some(g) if g.starts_with("grid-") => {
            let cols = g[5..].parse::<u32>().unwrap_or(2);
            VrLayoutMode::Grid { columns: cols }
        }
        Some("grid") => VrLayoutMode::Grid {
            columns: get_int(value, "columns").unwrap_or(2) as u32,
        },
        _ => return Some(error_response(msg_id, "invalid :layout (use arc, grid, stack, freeform)")),
    };

    state.vr_state.scene.set_layout(mode);
    Some(ok_response(msg_id))
}

fn handle_vr_scene_set_ppu(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let ppu = match get_int(value, "ppu") {
        Some(p) if p > 0 => p as f32,
        _ => return Some(error_response(msg_id, "invalid :ppu (must be positive integer)")),
    };

    let surface_id = get_int(value, "surface-id");
    match surface_id {
        Some(id) => state.vr_state.scene.set_surface_ppu(id as u64, ppu),
        None => state.vr_state.scene.set_global_ppu(ppu),
    }

    Some(ok_response(msg_id))
}

fn handle_vr_scene_set_background(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::scene::VrBackground;

    let bg = get_keyword(value, "background");
    let background = match bg.as_deref() {
        Some("dark") => VrBackground::Dark,
        Some("gradient") => VrBackground::Gradient,
        Some("grid") => VrBackground::Grid,
        Some("passthrough") => VrBackground::Passthrough,
        _ => return Some(error_response(msg_id, "invalid :background (use dark, gradient, grid, passthrough)")),
    };

    state.vr_state.scene.background = background;
    Some(ok_response(msg_id))
}

fn handle_vr_scene_set_projection(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::scene::ProjectionType;

    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    let proj = get_keyword(value, "projection");
    let projection = match proj.as_deref() {
        Some("flat") => ProjectionType::Flat,
        Some("cylinder") => ProjectionType::Cylinder,
        _ => return Some(error_response(msg_id, "invalid :projection (use flat, cylinder)")),
    };

    state.vr_state.scene.set_projection(surface_id, projection);
    Some(ok_response(msg_id))
}

fn handle_vr_scene_focus(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let surface_id = get_int(value, "surface-id").map(|id| id as u64);
    state.vr_state.scene.set_focus(surface_id);
    Some(ok_response(msg_id))
}

fn handle_vr_scene_move(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::scene::{Transform3D, Vec3, Quat};

    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    let x = get_int(value, "x").unwrap_or(0) as f32 / 100.0; // cm to meters
    let y = get_int(value, "y").unwrap_or(0) as f32 / 100.0;
    let z = get_int(value, "z").unwrap_or(-200) as f32 / 100.0;

    let transform = Transform3D {
        position: Vec3::new(x, y, z),
        rotation: Quat::IDENTITY,
        scale: Vec3::ONE,
    };

    state.vr_state.scene.set_transform(surface_id, transform);
    Some(ok_response(msg_id))
}

// ── VR Display handlers ───────────────────────────────────

fn handle_vr_display_info(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.hmd_manager.display_info_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :display {})",
        msg_id, sexp
    ))
}

fn handle_vr_display_set_mode(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::drm_lease::VrDisplayMode;

    let mode_str = get_keyword(value, "mode");
    let mode = match mode_str.as_deref().and_then(VrDisplayMode::from_str) {
        Some(m) => m,
        None => return Some(error_response(msg_id, "invalid :mode (use headset, preview, headless, off)")),
    };

    state.vr_state.hmd_manager.set_display_mode(mode);
    Some(ok_response(msg_id))
}

fn handle_vr_display_select_hmd(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let connector_id = match get_int(value, "connector-id") {
        Some(id) => id as u32,
        None => return Some(error_response(msg_id, "missing :connector-id")),
    };

    if state.vr_state.hmd_manager.select_hmd(connector_id) {
        Some(ok_response(msg_id))
    } else {
        Some(error_response(msg_id, &format!("connector {} not found or not an HMD", connector_id)))
    }
}

fn handle_vr_display_set_refresh_rate(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let target = match get_int(value, "rate") {
        Some(r) if r > 0 => r as u32,
        _ => return Some(error_response(msg_id, "invalid :rate (must be positive integer)")),
    };

    let actual = state.vr_state.hmd_manager.set_target_refresh_rate(target);
    Some(format!(
        "(:type :response :id {} :status :ok :target {} :actual {})",
        msg_id, target, actual
    ))
}

fn handle_vr_display_auto_detect(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let mode = state.vr_state.hmd_manager.auto_detect_mode();
    Some(format!(
        "(:type :response :id {} :status :ok :mode :{})",
        msg_id,
        mode.as_str()
    ))
}

fn handle_vr_display_list_connectors(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let mut list = String::from("(");
    for conn in &state.vr_state.hmd_manager.connectors {
        list.push_str(&conn.to_sexp());
    }
    list.push(')');

    Some(format!(
        "(:type :response :id {} :status :ok :connectors {})",
        msg_id, list
    ))
}

// ── VR Interaction handlers ────────────────────────────────

fn handle_vr_pointer_state(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.interaction.pointer_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :pointer {})",
        msg_id, sexp
    ))
}

fn handle_vr_click(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::vr_interaction::ClickType;

    let button_str = get_keyword(value, "button").unwrap_or_else(|| "left".to_string());
    let click = match ClickType::from_str(&button_str) {
        Some(c) => c,
        None => return Some(error_response(msg_id, "invalid :button (use left, right, middle, double)")),
    };

    let target = state.vr_state.interaction.current_hit.map(|h| h.surface_id);
    let ptr = &state.vr_state.interaction.active_pointer;
    let (px, py) = ptr.as_ref().map(|p| (p.pixel_x, p.pixel_y)).unwrap_or((0, 0));

    Some(format!(
        "(:type :response :id {} :status :ok :button :{} :surface-id {} :x {} :y {})",
        msg_id,
        click.as_str(),
        target.map(|id| id.to_string()).unwrap_or_else(|| "nil".to_string()),
        px, py
    ))
}

fn handle_vr_grab(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.interaction.start_grab(&state.vr_state.scene) {
        Some(sid) => Some(format!(
            "(:type :response :id {} :status :ok :surface-id {})",
            msg_id, sid
        )),
        None => Some(error_response(msg_id, "no surface under ray to grab")),
    }
}

fn handle_vr_grab_release(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.interaction.end_grab() {
        Some((sid, pos)) => Some(format!(
            "(:type :response :id {} :status :ok :surface-id {} :position (:x {:.3} :y {:.3} :z {:.3}))",
            msg_id, sid, pos.x, pos.y, pos.z
        )),
        None => Some(error_response(msg_id, "no active grab")),
    }
}

fn handle_vr_adjust_depth(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::vr_interaction::{adjust_depth, DEPTH_MIN, DEPTH_MAX};

    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    let delta = get_int(value, "delta").unwrap_or(-20) as f32 / 100.0; // cm to meters

    if let Some(node) = state.vr_state.scene.nodes.get_mut(&surface_id) {
        let new_z = adjust_depth(node.transform.position.z, delta, DEPTH_MIN, DEPTH_MAX);
        node.transform.position.z = new_z;
        Some(format!(
            "(:type :response :id {} :status :ok :surface-id {} :distance {:.2})",
            msg_id, surface_id, -new_z
        ))
    } else {
        Some(error_response(msg_id, &format!("unknown surface: {}", surface_id)))
    }
}

fn handle_vr_set_follow(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::vr_interaction::FollowMode;

    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    let mode_str = get_keyword(value, "mode");
    let mode = match mode_str.as_deref().and_then(FollowMode::from_str) {
        Some(m) => m,
        None => return Some(error_response(msg_id, "invalid :mode (use none, lazy, sticky, locked)")),
    };

    state.vr_state.interaction.set_follow_mode(surface_id, mode);
    Some(ok_response(msg_id))
}

fn handle_vr_set_gaze_offset(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::scene::Vec3;

    let x = get_int(value, "x").unwrap_or(15) as f32 / 100.0;
    let y = get_int(value, "y").unwrap_or(-10) as f32 / 100.0;
    let z = get_int(value, "z").unwrap_or(-5) as f32 / 100.0;

    state.vr_state.interaction.gaze_config.offset = Vec3::new(x, y, z);
    Some(ok_response(msg_id))
}

fn handle_vr_calibrate_confirm(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let head_pose = state.vr_state.interaction.head_pose;
    let done = state.vr_state.interaction.calibration.record_point(
        crate::vr::vr_interaction::HeadPose {
            position: head_pose.position,
            rotation: head_pose.rotation,
        },
    );

    if done {
        // Compute new offset
        if let Some(offset) = state.vr_state.interaction.calibration.compute_offset() {
            let rms = state.vr_state.interaction.calibration.rms_error_deg(&offset);
            state.vr_state.interaction.gaze_config.offset = offset;
            Some(format!(
                "(:type :response :id {} :status :ok :calibration :complete :rms-error {:.1} :offset (:x {:.3} :y {:.3} :z {:.3}))",
                msg_id, rms, offset.x, offset.y, offset.z
            ))
        } else {
            Some(error_response(msg_id, "calibration failed: insufficient data"))
        }
    } else {
        let next = state.vr_state.interaction.calibration.current_target;
        Some(format!(
            "(:type :response :id {} :status :ok :calibration :point-recorded :next {})",
            msg_id, next
        ))
    }
}

// ── Eye tracking handlers (Week 11) ───────────────────────

fn handle_gaze_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.eye_tracking.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gaze {})",
        msg_id, status
    ))
}

fn handle_gaze_set_source(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let source_str = get_keyword(value, "source").unwrap_or_default();
    use crate::vr::eye_tracking::GazeSource;
    let source = if source_str == "auto" {
        None
    } else {
        match GazeSource::from_str(&source_str) {
            Some(s) => Some(s),
            None => return Some(error_response(msg_id, &format!("unknown gaze source: {source_str}"))),
        }
    };
    state.vr_state.eye_tracking.set_source(source);
    Some(ok_response(msg_id))
}

fn handle_gaze_calibrate_start(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let points = get_int(value, "points").unwrap_or(5) as usize;
    state.vr_state.eye_tracking.start_calibration(points);
    Some(format!(
        "(:type :response :id {} :status :ok :calibration :started :points {})",
        msg_id, points
    ))
}

fn handle_gaze_calibrate_point(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::scene::Vec3;
    let tx = get_int(value, "target-x").unwrap_or(0) as f32 / 100.0;
    let ty = get_int(value, "target-y").unwrap_or(0) as f32 / 100.0;
    let tz = get_int(value, "target-z").unwrap_or(-200) as f32 / 100.0;
    let target = Vec3::new(tx, ty, tz);

    let gaze_dir = state
        .vr_state
        .eye_tracking
        .current_gaze
        .map(|g| g.ray.direction)
        .unwrap_or(Vec3::new(0.0, 0.0, -1.0));

    let timestamp = state
        .vr_state
        .eye_tracking
        .current_gaze
        .map(|g| g.timestamp_s)
        .unwrap_or(0.0);

    let complete = state
        .vr_state
        .eye_tracking
        .record_calibration_point(target, gaze_dir, timestamp);

    if complete {
        let rms = state
            .vr_state
            .eye_tracking
            .calibration
            .rms_error()
            .unwrap_or(0.0);
        Some(format!(
            "(:type :response :id {} :status :ok :calibration :complete :rms-error {:.1})",
            msg_id, rms
        ))
    } else {
        let next = state
            .vr_state
            .eye_tracking
            .calibration
            .current_point_index()
            .unwrap_or(0);
        Some(format!(
            "(:type :response :id {} :status :ok :calibration :point-recorded :next {})",
            msg_id, next
        ))
    }
}

fn handle_gaze_set_visualization(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let vis_str = get_keyword(value, "mode").unwrap_or_default();
    use crate::vr::eye_tracking::GazeVisualization;
    match GazeVisualization::from_str(&vis_str) {
        Some(vis) => {
            state.vr_state.eye_tracking.set_visualization(vis);
            Some(ok_response(msg_id))
        }
        None => Some(error_response(msg_id, &format!("unknown visualization: {vis_str}"))),
    }
}

fn handle_gaze_set_smoothing(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let alpha = get_int(value, "alpha").unwrap_or(30) as f32 / 100.0;
    state.vr_state.eye_tracking.set_smoothing(alpha);
    Some(ok_response(msg_id))
}

fn handle_gaze_simulate(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let mode_str = get_keyword(value, "mode").unwrap_or_default();
    use crate::vr::eye_tracking::SimulatedGazeMode;
    if mode_str == "off" || mode_str == "nil" {
        state.vr_state.eye_tracking.set_simulate(None);
        return Some(ok_response(msg_id));
    }
    match SimulatedGazeMode::from_str(&mode_str) {
        Some(mode) => {
            state.vr_state.eye_tracking.set_simulate(Some(mode));
            Some(ok_response(msg_id))
        }
        None => Some(error_response(msg_id, &format!("unknown simulate mode: {mode_str}"))),
    }
}

fn handle_gaze_health(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let h = &state.vr_state.eye_tracking.health;
    Some(format!(
        "(:type :response :id {} :status :ok :health (:rate {:.0} :expected-rate {:.0} :confidence {:.2} :tracking-lost {} :calibration-error {} :consecutive-lost {}))",
        msg_id,
        h.actual_rate_hz,
        h.expected_rate_hz,
        h.avg_confidence,
        if h.tracking_lost { "t" } else { "nil" },
        h.calibration_error_deg.map(|e| format!("{:.1}", e)).unwrap_or_else(|| "nil".to_string()),
        h.consecutive_lost_frames,
    ))
}

// ── Gaze focus handlers (Week 12) ──────────────────────────

fn handle_gaze_focus_config(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let config = state.vr_state.gaze_focus.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_gaze_focus_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.gaze_focus.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :focus {})",
        msg_id, status
    ))
}

fn handle_gaze_focus_set_policy(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::gaze_focus::FocusPolicy;

    let policy_str = get_keyword(value, "policy").unwrap_or_default();
    match FocusPolicy::from_str(&policy_str) {
        Some(policy) => {
            state.vr_state.gaze_focus.set_policy(policy);
            Some(ok_response(msg_id))
        }
        None => Some(error_response(
            msg_id,
            &format!("invalid :policy (use gaze-only, gaze-primary, gaze-assist, disabled): {policy_str}"),
        )),
    }
}

fn handle_gaze_focus_set_dwell(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let threshold = match get_int(value, "threshold-ms") {
        Some(t) if t >= 50 && t <= 2000 => t as f64,
        _ => return Some(error_response(msg_id, "invalid :threshold-ms (50-2000)")),
    };

    state.vr_state.gaze_focus.set_dwell_threshold(threshold);
    Some(format!(
        "(:type :response :id {} :status :ok :threshold-ms {:.0})",
        msg_id, threshold
    ))
}

fn handle_gaze_focus_set_cooldown(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let cooldown = match get_int(value, "cooldown-ms") {
        Some(c) if c >= 0 && c <= 5000 => c as f64,
        _ => return Some(error_response(msg_id, "invalid :cooldown-ms (0-5000)")),
    };

    state.vr_state.gaze_focus.set_cooldown(cooldown);
    Some(format!(
        "(:type :response :id {} :status :ok :cooldown-ms {:.0})",
        msg_id, cooldown
    ))
}

fn handle_gaze_focus_analytics(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let analytics = state.vr_state.gaze_focus.analytics.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :analytics {})",
        msg_id, analytics
    ))
}

fn handle_gaze_focus_back(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.gaze_focus.focus_back() {
        Some(surface_id) => Some(format!(
            "(:type :response :id {} :status :ok :surface-id {})",
            msg_id, surface_id
        )),
        None => Some(error_response(msg_id, "no focus history available")),
    }
}

// ── Blink/wink handlers (Week 13) ──────────────────────────

fn handle_wink_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.blink_wink.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :wink {})",
        msg_id, status
    ))
}

fn handle_wink_config(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let config = state.vr_state.blink_wink.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_wink_calibrate_start(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let trials = get_int(value, "trials").unwrap_or(10);
    state.vr_state.blink_wink.calibration.reset();
    Some(format!(
        "(:type :response :id {} :status :ok :calibration :started :trials {})",
        msg_id, trials
    ))
}

fn handle_wink_set_confidence(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let threshold = get_int(value, "threshold").unwrap_or(70) as f32 / 100.0;
    if !(0.0..=1.0).contains(&threshold) {
        return Some(error_response(msg_id, "invalid :threshold (0-100)"));
    }
    state.vr_state.blink_wink.blink_detector.confidence_threshold = threshold;
    Some(format!(
        "(:type :response :id {} :status :ok :threshold {:.2})",
        msg_id, threshold
    ))
}

// ── Gaze zone handlers (Week 13) ──────────────────────────

fn handle_gaze_zone_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.zone_detector.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :zone {})",
        msg_id, status
    ))
}

fn handle_gaze_zone_config(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let config = state.vr_state.zone_detector.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_gaze_zone_set_dwell(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let dwell_ms = match get_int(value, "dwell-ms") {
        Some(d) if d >= 50 && d <= 2000 => d as f64,
        _ => return Some(error_response(msg_id, "invalid :dwell-ms (50-2000)")),
    };

    state.vr_state.zone_detector.config.dwell_ms = dwell_ms;
    Some(format!(
        "(:type :response :id {} :status :ok :dwell-ms {:.0})",
        msg_id, dwell_ms
    ))
}

// ── Fatigue handlers (Week 13) ─────────────────────────────

fn handle_fatigue_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.fatigue_monitor.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :fatigue {})",
        msg_id, status
    ))
}

fn handle_fatigue_config(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let config = state.vr_state.fatigue_monitor.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_fatigue_metrics(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let metrics = state.vr_state.fatigue_monitor.metrics_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :metrics {})",
        msg_id, metrics
    ))
}

fn handle_fatigue_reset(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.fatigue_monitor.teardown();
    Some(ok_response(msg_id))
}

// ── Auto-type handlers (Week 14) ───────────────────────────

fn handle_autotype(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let text = match get_string(value, "text") {
        Some(t) => t,
        None => return Some(error_response(msg_id, "missing :text")),
    };
    let surface_id = get_int(value, "surface-id").unwrap_or(0) as u64;
    let delay_ms = get_int(value, "delay-ms");
    let verify = get_keyword(value, "verify-surface");

    if let Some(d) = delay_ms {
        state.autotype.config.delay_ms = d as u64;
    }
    if let Some(v) = verify {
        state.autotype.config.verify_surface = v != "nil";
    }

    match state.autotype.start_typing(&text, surface_id) {
        Ok(()) => Some(format!(
            "(:type :response :id {} :status :ok :chars {} :surface-id {})",
            msg_id,
            text.len(),
            surface_id
        )),
        Err(msg) => Some(error_response(msg_id, &msg)),
    }
}

fn handle_autotype_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.autotype.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :autotype {})",
        msg_id, status
    ))
}

fn handle_autotype_abort(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.autotype.abort();
    Some(ok_response(msg_id))
}

fn handle_autotype_pause(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::autotype::PauseReason;

    let reason_str = get_keyword(value, "reason").unwrap_or_else(|| "user-requested".to_string());
    let reason = match reason_str.as_str() {
        "gaze-away" => PauseReason::GazeAway,
        _ => PauseReason::UserRequested,
    };
    state.autotype.pause(reason);
    Some(ok_response(msg_id))
}

fn handle_autotype_resume(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.autotype.resume();
    Some(ok_response(msg_id))
}

// ── Secure input handlers (Week 14) ────────────────────────

fn handle_secure_input_mode(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let enable = get_keyword(value, "enable")
        .map(|v| v != "nil")
        .unwrap_or(true);

    if enable {
        let reason = get_string(value, "reason").unwrap_or_else(|| "ipc".to_string());
        let surface_id = get_int(value, "surface-id").unwrap_or(0) as u64;
        let timeout = get_int(value, "timeout");
        if let Some(t) = timeout {
            state.secure_input.config.auto_exit_timeout_secs = t as u64;
        }
        state.secure_input.enter(&reason, surface_id);
        Some(ok_response(msg_id))
    } else {
        state.secure_input.exit();
        Some(ok_response(msg_id))
    }
}

fn handle_secure_input_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.secure_input.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :secure-input {})",
        msg_id, status
    ))
}

fn handle_gaze_away_monitor(
    _state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let enable = get_keyword(value, "enable")
        .map(|v| v != "nil")
        .unwrap_or(true);
    let surface_id = get_int(value, "surface-id").unwrap_or(0);
    let _pause_ms = get_int(value, "pause-ms").unwrap_or(500);
    let _resume_ms = get_int(value, "resume-ms").unwrap_or(300);
    let _abort_ms = get_int(value, "abort-ms").unwrap_or(5000);

    debug!(
        enable,
        surface_id,
        "gaze-away monitor {}",
        if enable { "started" } else { "stopped" }
    );

    // Gaze-away monitoring state is tracked on the Emacs side;
    // the compositor acknowledges the request and will emit
    // gaze-target-changed events as needed.
    Some(ok_response(msg_id))
}

// ── Headless backend handlers (Week 16) ────────────────────

fn handle_headless_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let active = if state.headless_active { "t" } else { "nil" };
    let surface_count = state.surfaces.len();
    let ipc_client_count = state.ipc_server.clients.len();
    Some(format!(
        "(:type :response :id {} :status :ok :headless {} :outputs {} :resolution \"{}x{}\" :surfaces {} :ipc-clients {})",
        msg_id,
        active,
        state.headless_output_count,
        state.headless_width,
        state.headless_height,
        surface_count,
        ipc_client_count,
    ))
}

fn handle_headless_set_resolution(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if !state.headless_active {
        return Some(error_response(msg_id, "not running in headless mode"));
    }

    let w = match get_int(value, "w") {
        Some(v) if v > 0 && v <= 7680 => v as i32,
        _ => return Some(error_response(msg_id, "invalid :w (must be 1-7680)")),
    };
    let h = match get_int(value, "h") {
        Some(v) if v > 0 && v <= 4320 => v as i32,
        _ => return Some(error_response(msg_id, "invalid :h (must be 1-4320)")),
    };

    state.headless_width = w;
    state.headless_height = h;

    debug!(w, h, "headless resolution updated");
    Some(format!(
        "(:type :response :id {} :status :ok :resolution \"{}x{}\")",
        msg_id, w, h
    ))
}

fn handle_headless_add_output(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    if !state.headless_active {
        return Some(error_response(msg_id, "not running in headless mode"));
    }

    let new_index = state.headless_output_count;
    state.headless_output_count += 1;

    // Create the virtual output in the Smithay space
    let mode = smithay::output::Mode {
        size: (state.headless_width, state.headless_height).into(),
        refresh: 60_000,
    };
    let output = smithay::output::Output::new(
        format!("headless-{}", new_index),
        smithay::output::PhysicalProperties {
            size: (0, 0).into(),
            subpixel: smithay::output::Subpixel::Unknown,
            make: "EWWM".into(),
            model: "Headless".into(),
        },
    );
    let x_offset = (new_index as i32) * state.headless_width;
    output.change_current_state(
        Some(mode),
        Some(smithay::utils::Transform::Normal),
        None,
        Some((x_offset, 0).into()),
    );
    output.set_preferred(mode);
    state.space.map_output(&output, (x_offset, 0));

    debug!(index = new_index, "added headless output");
    Some(format!(
        "(:type :response :id {} :status :ok :output-index {} :outputs {})",
        msg_id, new_index, state.headless_output_count
    ))
}

fn handle_headless_remove_output(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    if !state.headless_active {
        return Some(error_response(msg_id, "not running in headless mode"));
    }

    if state.headless_output_count <= 1 {
        return Some(error_response(msg_id, "cannot remove last output"));
    }

    state.headless_output_count -= 1;
    let removed_index = state.headless_output_count;

    // Find and unmap the output from the space
    let target_name = format!("headless-{}", removed_index);
    let output = state
        .space
        .outputs()
        .find(|o| o.name() == target_name)
        .cloned();
    if let Some(o) = output {
        state.space.unmap_output(&o);
    }

    debug!(index = removed_index, "removed headless output");
    Some(format!(
        "(:type :response :id {} :status :ok :removed-index {} :outputs {})",
        msg_id, removed_index, state.headless_output_count
    ))
}

// ── Gaze scroll handlers (Week 17) ─────────────────────────

fn handle_gaze_scroll_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.gaze_scroll.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gaze-scroll {})",
        msg_id, status
    ))
}

fn handle_gaze_scroll_config(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if let Some(enabled_str) = get_keyword(value, "enable") {
        state.vr_state.gaze_scroll.config.enabled = enabled_str != "nil";
    }
    if let Some(edge) = get_int(value, "edge-pct") {
        let pct = (edge as f32 / 100.0).clamp(0.01, 0.50);
        state.vr_state.gaze_scroll.config.edge_pct = pct;
    }
    if let Some(speed) = get_int(value, "speed") {
        let s = (speed as f32).clamp(0.1, 50.0);
        state.vr_state.gaze_scroll.config.speed = s;
    }
    if let Some(delay) = get_int(value, "activation-delay-ms") {
        state.vr_state.gaze_scroll.config.activation_delay_ms = (delay as f64).max(0.0);
    }
    if let Some(horiz_str) = get_keyword(value, "horizontal") {
        state.vr_state.gaze_scroll.config.horizontal = horiz_str != "nil";
    }

    let status = state.vr_state.gaze_scroll.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gaze-scroll {})",
        msg_id, status
    ))
}

fn handle_gaze_scroll_set_speed(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let speed = match get_int(value, "speed") {
        Some(s) if s > 0 => s as f32,
        _ => return Some(error_response(msg_id, "invalid :speed (must be positive)")),
    };

    state.vr_state.gaze_scroll.config.speed = speed.clamp(0.1, 50.0);
    Some(format!(
        "(:type :response :id {} :status :ok :speed {:.1})",
        msg_id, state.vr_state.gaze_scroll.config.speed
    ))
}

// ── Link hint handlers (Week 17) ───────────────────────────

fn handle_link_hints_load(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let json = match get_string(value, "hints") {
        Some(j) => j,
        None => return Some(error_response(msg_id, "missing :hints (JSON array)")),
    };
    let surface_id = get_int(value, "surface-id").unwrap_or(0) as u64;

    match state.vr_state.link_hints.load_hints(&json, surface_id) {
        Ok(count) => Some(format!(
            "(:type :response :id {} :status :ok :hint-count {} :surface-id {})",
            msg_id, count, surface_id
        )),
        Err(e) => Some(error_response(msg_id, &format!("hint load failed: {}", e))),
    }
}

fn handle_link_hints_confirm(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.link_hints.confirm() {
        Some(crate::vr::link_hints::LinkHintEvent::Confirmed { hint_id, url }) => {
            Some(format!(
                "(:type :response :id {} :status :ok :hint-id {} :url \"{}\")",
                msg_id, hint_id, escape_string(&url)
            ))
        }
        _ => Some(error_response(msg_id, "no hint currently highlighted")),
    }
}

fn handle_link_hints_clear(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.link_hints.clear();
    Some(ok_response(msg_id))
}

fn handle_link_hints_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.link_hints.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :link-hints {})",
        msg_id, status
    ))
}

// ── Hand tracking handlers (Week 18) ───────────────────────

fn handle_hand_tracking_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.hand_tracking.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :hand-tracking {})",
        msg_id, status
    ))
}

fn handle_hand_tracking_config(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if let Some(enabled) = get_bool(value, "enabled") {
        state.vr_state.hand_tracking.config.enabled = enabled;
    }
    if let Some(min_conf) = get_float(value, "min-confidence") {
        state.vr_state.hand_tracking.config.min_confidence = min_conf as f32;
    }
    if let Some(smoothing) = get_float(value, "smoothing") {
        state.vr_state.hand_tracking.config.smoothing = smoothing as f32;
    }
    if let Some(prediction) = get_float(value, "prediction-ms") {
        state.vr_state.hand_tracking.config.prediction_ms = prediction as f32;
    }

    let status = state.vr_state.hand_tracking.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :hand-tracking {})",
        msg_id, status
    ))
}

fn handle_hand_tracking_joint(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let hand_str = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };
    let joint_name = match get_string(value, "joint") {
        Some(j) => j,
        None => return Some(error_response(msg_id, "missing :joint")),
    };

    match state.vr_state.hand_tracking.get_joint(&hand_str, &joint_name) {
        Some(joint) => {
            let pos = joint.position;
            let rot = joint.orientation;
            Some(format!(
                "(:type :response :id {} :status :ok :hand :{} :joint \"{}\" :position (:x {:.4} :y {:.4} :z {:.4}) :orientation (:x {:.4} :y {:.4} :z {:.4} :w {:.4}) :radius {:.4})",
                msg_id, hand_str, escape_string(&joint_name),
                pos.x, pos.y, pos.z,
                rot.x, rot.y, rot.z, rot.w,
                joint.radius,
            ))
        }
        None => Some(error_response(
            msg_id,
            &format!("joint not found: {} {}", hand_str, joint_name),
        )),
    }
}

fn handle_hand_tracking_skeleton(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let hand_str = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };

    match state.vr_state.hand_tracking.get_skeleton(&hand_str) {
        Some(joints) => {
            let mut sexp = String::from("(");
            for joint in &joints {
                let pos = joint.position;
                let rot = joint.orientation;
                sexp.push_str(&format!(
                    "(:name \"{}\" :position (:x {:.4} :y {:.4} :z {:.4}) :orientation (:x {:.4} :y {:.4} :z {:.4} :w {:.4}) :radius {:.4})",
                    escape_string(&joint.name),
                    pos.x, pos.y, pos.z,
                    rot.x, rot.y, rot.z, rot.w,
                    joint.radius,
                ));
            }
            sexp.push(')');
            Some(format!(
                "(:type :response :id {} :status :ok :hand :{} :joint-count {} :joints {})",
                msg_id, hand_str, joints.len(), sexp
            ))
        }
        None => Some(error_response(
            msg_id,
            &format!("hand not tracked: {}", hand_str),
        )),
    }
}

fn handle_hand_tracking_distance(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let hand_str = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };
    let joint_a = match get_string(value, "joint-a") {
        Some(j) => j,
        None => return Some(error_response(msg_id, "missing :joint-a")),
    };
    let joint_b = match get_string(value, "joint-b") {
        Some(j) => j,
        None => return Some(error_response(msg_id, "missing :joint-b")),
    };

    match state.vr_state.hand_tracking.joint_distance_by_name(&hand_str, &joint_a, &joint_b) {
        Some(distance) => Some(format!(
            "(:type :response :id {} :status :ok :hand :{} :joint-a \"{}\" :joint-b \"{}\" :distance {:.4})",
            msg_id, hand_str,
            escape_string(&joint_a),
            escape_string(&joint_b),
            distance,
        )),
        None => Some(error_response(
            msg_id,
            &format!("could not compute distance: {} {} <-> {}", hand_str, joint_a, joint_b),
        )),
    }
}

// ── Gesture handlers (Week 18) ─────────────────────────────

fn handle_gesture_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.gesture.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gesture {})",
        msg_id, status
    ))
}

fn handle_gesture_config(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if let Some(pinch) = get_float(value, "pinch-threshold") {
        state.vr_state.gesture.config.pinch_threshold_m = pinch as f32;
    }
    if let Some(grab) = get_float(value, "grab-threshold") {
        state.vr_state.gesture.config.grab_threshold_m = grab as f32;
    }
    if let Some(swipe) = get_float(value, "swipe-min-velocity") {
        state.vr_state.gesture.config.swipe_min_velocity = swipe as f32;
    }
    if let Some(debounce) = get_float(value, "debounce-ms") {
        state.vr_state.gesture.config.debounce_ms = debounce;
    }
    if let Some(enabled) = get_bool(value, "enabled") {
        state.vr_state.gesture.config.enabled = enabled;
    }

    let status = state.vr_state.gesture.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gesture {})",
        msg_id, status
    ))
}

fn handle_gesture_bind(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let gesture = match get_string(value, "gesture") {
        Some(g) => g,
        None => return Some(error_response(msg_id, "missing :gesture")),
    };
    let hand = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };
    let action = match get_string(value, "action") {
        Some(a) => a,
        None => return Some(error_response(msg_id, "missing :action")),
    };

    state.vr_state.gesture.add_binding(&gesture, &hand, &action);
    let count = state.vr_state.gesture.binding_count();
    Some(format!(
        "(:type :response :id {} :status :ok :gesture \"{}\" :hand :{} :action \"{}\" :bindings {})",
        msg_id,
        escape_string(&gesture),
        hand,
        escape_string(&action),
        count,
    ))
}

fn handle_gesture_unbind(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let gesture = match get_string(value, "gesture") {
        Some(g) => g,
        None => return Some(error_response(msg_id, "missing :gesture")),
    };
    let hand = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };

    let removed = state.vr_state.gesture.remove_binding(&gesture, &hand);
    if removed {
        Some(ok_response(msg_id))
    } else {
        Some(error_response(
            msg_id,
            &format!("no binding for gesture {} hand {}", gesture, hand),
        ))
    }
}

fn handle_gesture_bindings(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let bindings = state.vr_state.gesture.bindings_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :bindings {})",
        msg_id, bindings
    ))
}

// ── Virtual keyboard handlers (Week 18) ────────────────────

fn handle_keyboard_show(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.virtual_keyboard.show();
    Some(ok_response(msg_id))
}

fn handle_keyboard_hide(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.virtual_keyboard.hide();
    Some(ok_response(msg_id))
}

fn handle_keyboard_toggle(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.virtual_keyboard.toggle();
    let visible = if state.vr_state.virtual_keyboard.visible { "t" } else { "nil" };
    Some(format!(
        "(:type :response :id {} :status :ok :visible {})",
        msg_id, visible
    ))
}

fn handle_keyboard_layout(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let layout_str = match get_string(value, "layout") {
        Some(l) => l,
        None => return Some(error_response(msg_id, "missing :layout")),
    };

    match state.vr_state.virtual_keyboard.set_layout_by_name(&layout_str) {
        Ok(()) => Some(format!(
            "(:type :response :id {} :status :ok :layout \"{}\")",
            msg_id, escape_string(&layout_str)
        )),
        Err(msg) => Some(error_response(
            msg_id,
            &format!("invalid :layout (use qwerty, dvorak, colemak): {}", msg),
        )),
    }
}

fn handle_keyboard_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.virtual_keyboard.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :keyboard {})",
        msg_id, status
    ))
}

// ── Helpers ────────────────────────────────────────────────

fn ok_response(id: i64) -> String {
    format!("(:type :response :id {} :status :ok)", id)
}

fn error_response(id: i64, reason: &str) -> String {
    format!(
        "(:type :response :id {} :status :error :reason \"{}\")",
        id,
        escape_string(reason)
    )
}

/// Escape a string for s-expression output.
fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Extract a keyword value from an s-expression plist.
/// Looks for `:key` followed by a value in a flat list.
fn get_keyword(value: &Value, key: &str) -> Option<String> {
    let keyword = format!(":{}", key);
    if let Value::List(items) = value {
        // Iterate pairs: (:key value :key value ...)
        // lexpr parses (:type :hello) as a list of cons/atoms
        let flat: Vec<&Value> = flatten_list(value);
        for i in 0..flat.len().saturating_sub(1) {
            if let Value::Keyword(k) = flat[i] {
                if k.as_ref() == key {
                    return match flat[i + 1] {
                        Value::Keyword(v) => Some(v.to_string()),
                        Value::Symbol(v) => Some(v.to_string()),
                        Value::String(v) => Some(v.to_string()),
                        Value::Number(n) => Some(n.to_string()),
                        _ => Some(flat[i + 1].to_string()),
                    };
                }
            }
        }
    }
    None
}

/// Extract an integer value from an s-expression plist.
fn get_int(value: &Value, key: &str) -> Option<i64> {
    get_keyword(value, key).and_then(|s| s.parse().ok())
}

/// Extract a string value from an s-expression plist.
fn get_string(value: &Value, key: &str) -> Option<String> {
    get_keyword(value, key)
}

/// Extract a boolean value from an s-expression plist.
/// Treats "t" as true, "nil" as false.
fn get_bool(value: &Value, key: &str) -> Option<bool> {
    get_keyword(value, key).map(|s| s != "nil")
}

/// Extract a floating-point value from an s-expression plist.
fn get_float(value: &Value, key: &str) -> Option<f64> {
    get_keyword(value, key).and_then(|s| s.parse().ok())
}

/// Flatten a possibly nested list/cons structure into a Vec of leaf values.
fn flatten_list(value: &Value) -> Vec<&Value> {
    let mut result = Vec::new();
    fn walk<'a>(v: &'a Value, out: &mut Vec<&'a Value>) {
        match v {
            Value::Cons(pair) => {
                walk(pair.car(), out);
                walk(pair.cdr(), out);
            }
            Value::Null => {} // end of list
            other => out.push(other),
        }
    }
    walk(value, &mut result);
    result
}

/// Format an IPC event s-expression.
pub fn format_event(event_type: &str, fields: &[(&str, &str)]) -> String {
    let mut s = format!("(:type :event :event :{}", event_type);
    for (key, val) in fields {
        s.push_str(&format!(" :{} {}", key, val));
    }
    s.push(')');
    s
}
