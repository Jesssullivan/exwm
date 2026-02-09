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
