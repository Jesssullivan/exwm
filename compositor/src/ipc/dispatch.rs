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
