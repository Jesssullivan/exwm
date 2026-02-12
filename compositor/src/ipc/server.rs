//! IPC server: Unix socket listener + per-client state + message framing.

use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::os::unix::io::AsRawFd;
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::time::Instant;

use calloop::generic::Generic;
use calloop::{Interest, LoopHandle, Mode, PostAction};
use tracing::{debug, error, info, warn};

use super::dispatch;
use super::recorder::{IpcRecorder, MessageDirection};

/// Maximum message payload size (1 MiB).
const MAX_MESSAGE_SIZE: u32 = 1_048_576;

/// Maximum write buffer before dropping old events (64 KiB).
const MAX_WRITE_BUFFER: usize = 65_536;

/// Default rate limit: messages per second per client.
const DEFAULT_RATE_LIMIT: u32 = 200;

/// Rate limit window duration in seconds.
const RATE_LIMIT_WINDOW_SECS: u64 = 1;

/// Per-client rate limiter.
pub struct RateLimiter {
    window_start: Instant,
    message_count: u32,
    pub max_per_second: u32,
}

impl RateLimiter {
    fn new(max_per_second: u32) -> Self {
        Self {
            window_start: Instant::now(),
            message_count: 0,
            max_per_second,
        }
    }

    /// Check if a message is allowed.  Returns true if within rate limit.
    fn check(&mut self) -> bool {
        let now = Instant::now();
        let elapsed = now.duration_since(self.window_start);
        if elapsed.as_secs() >= RATE_LIMIT_WINDOW_SECS {
            // New window
            self.window_start = now;
            self.message_count = 1;
            true
        } else {
            self.message_count += 1;
            self.message_count <= self.max_per_second
        }
    }
}

/// Read peer credentials from a Unix socket using SO_PEERCRED (Linux)
/// or return None on unsupported platforms.
fn get_peer_cred(stream: &UnixStream) -> Option<(Option<u32>, Option<i32>)> {
    #[cfg(target_os = "linux")]
    {
        let fd = stream.as_raw_fd();
        let mut cred: libc::ucred = unsafe { std::mem::zeroed() };
        let mut len = std::mem::size_of::<libc::ucred>() as libc::socklen_t;
        let ret = unsafe {
            libc::getsockopt(
                fd,
                libc::SOL_SOCKET,
                libc::SO_PEERCRED,
                &mut cred as *mut _ as *mut libc::c_void,
                &mut len,
            )
        };
        if ret == 0 {
            Some((Some(cred.uid), Some(cred.pid)))
        } else {
            None
        }
    }
    #[cfg(not(target_os = "linux"))]
    {
        let _ = stream;
        Some((None, None))
    }
}

/// Per-client IPC connection state.
pub struct IpcClient {
    pub stream: UnixStream,
    pub read_buf: Vec<u8>,
    pub write_buf: Vec<u8>,
    pub authenticated: bool,
    pub id: u64,
    /// Peer UID from SO_PEERCRED (Unix only).
    pub peer_uid: Option<u32>,
    /// Peer PID from SO_PEERCRED (Unix only).
    pub peer_pid: Option<i32>,
    /// Per-client rate limiter.
    pub rate_limiter: RateLimiter,
}

impl IpcClient {
    fn new(stream: UnixStream, id: u64) -> Self {
        stream.set_nonblocking(true).ok();

        // Read peer credentials via SO_PEERCRED (Linux only, stable API).
        let (peer_uid, peer_pid) = get_peer_cred(&stream).unwrap_or_else(|| {
            warn!(id, "failed to read peer credentials");
            (None, None)
        });

        if let Some(uid) = peer_uid {
            debug!(id, peer_uid = uid, peer_pid = ?peer_pid, "peer credentials");
        }

        Self {
            stream,
            read_buf: Vec::with_capacity(4096),
            write_buf: Vec::new(),
            authenticated: false,
            id,
            peer_uid,
            peer_pid,
            rate_limiter: RateLimiter::new(DEFAULT_RATE_LIMIT),
        }
    }

    /// Attempt to flush pending writes.
    pub fn flush_writes(&mut self) -> io::Result<()> {
        while !self.write_buf.is_empty() {
            match self.stream.write(&self.write_buf) {
                Ok(0) => return Err(io::Error::new(io::ErrorKind::WriteZero, "write zero")),
                Ok(n) => {
                    self.write_buf.drain(..n);
                }
                Err(e) if e.kind() == io::ErrorKind::WouldBlock => break,
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    /// Enqueue a framed message (length prefix + payload) for sending.
    pub fn enqueue_message(&mut self, payload: &str) {
        let bytes = payload.as_bytes();
        let len = bytes.len() as u32;
        self.write_buf.extend_from_slice(&len.to_be_bytes());
        self.write_buf.extend_from_slice(bytes);
    }

    /// Enqueue an event, applying backpressure if buffer is too large.
    pub fn enqueue_event(&mut self, payload: &str) {
        if self.write_buf.len() > MAX_WRITE_BUFFER {
            warn!(client_id = self.id, "write buffer overflow, dropping event");
            return;
        }
        self.enqueue_message(payload);
    }

    /// Try to extract complete framed messages from the read buffer.
    /// Returns a Vec of complete message payloads (as Strings).
    pub fn extract_messages(&mut self) -> Vec<String> {
        let mut messages = Vec::new();
        loop {
            if self.read_buf.len() < 4 {
                break;
            }
            let len = u32::from_be_bytes([
                self.read_buf[0],
                self.read_buf[1],
                self.read_buf[2],
                self.read_buf[3],
            ]);
            if len > MAX_MESSAGE_SIZE {
                // Protocol violation — drop this client
                error!(client_id = self.id, len, "message exceeds maximum size");
                self.read_buf.clear();
                break;
            }
            let total = 4 + len as usize;
            if self.read_buf.len() < total {
                break; // Incomplete message, wait for more data
            }
            let payload = String::from_utf8_lossy(&self.read_buf[4..total]).to_string();
            self.read_buf.drain(..total);
            messages.push(payload);
        }
        messages
    }
}

/// IPC server managing the listener socket and all client connections.
pub struct IpcServer {
    pub socket_path: PathBuf,
    pub clients: HashMap<u64, IpcClient>,
    next_client_id: u64,
    pub ipc_trace: bool,
    pub recorder: IpcRecorder,
}

impl IpcServer {
    /// Create IPC server (does not bind yet — call `bind` after).
    pub fn new(socket_path: PathBuf) -> Self {
        Self {
            socket_path,
            clients: HashMap::new(),
            next_client_id: 1,
            ipc_trace: false,
            recorder: IpcRecorder::new(),
        }
    }

    /// Compute the default socket path.
    pub fn default_socket_path() -> PathBuf {
        let runtime_dir = std::env::var("XDG_RUNTIME_DIR")
            .unwrap_or_else(|_| format!("/tmp/ewwm-{}", unsafe { libc::getuid() }));
        PathBuf::from(runtime_dir).join("ewwm-ipc.sock")
    }

    /// Bind the listener socket and register with calloop.
    pub fn bind(
        socket_path: &Path,
        loop_handle: &LoopHandle<'static, crate::state::EwwmState>,
    ) -> anyhow::Result<()> {
        // Remove stale socket
        if socket_path.exists() {
            std::fs::remove_file(socket_path)?;
        }

        let listener = UnixListener::bind(socket_path)?;
        listener.set_nonblocking(true)?;

        // Set socket permissions to 0700
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            std::fs::set_permissions(socket_path, std::fs::Permissions::from_mode(0o700))?;
        }

        info!(?socket_path, "IPC server listening");

        // Register listener with calloop
        let source = Generic::new(listener, Interest::READ, Mode::Level);
        loop_handle.insert_source(source, |_event, listener, state| {
            // Accept new connections
            loop {
                match listener.accept() {
                    Ok((stream, _addr)) => {
                        let client_id = state.ipc_server.next_client_id;
                        state.ipc_server.next_client_id += 1;

                        info!(client_id, "IPC client connected");

                        let client = IpcClient::new(stream, client_id);
                        state.ipc_server.clients.insert(client_id, client);
                    }
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => break,
                    Err(e) => {
                        error!("accept error: {}", e);
                        break;
                    }
                }
            }
            Ok(PostAction::Continue)
        })?;

        Ok(())
    }

    /// Poll all clients for readable data, dispatch messages, flush writes.
    /// Called once per event loop iteration.
    pub fn poll_clients(state: &mut crate::state::EwwmState) {
        let client_ids: Vec<u64> = state.ipc_server.clients.keys().copied().collect();
        let mut disconnected = Vec::new();

        for client_id in client_ids {
            // Read available data
            let mut buf = [0u8; 4096];
            let read_result = {
                let client = state.ipc_server.clients.get_mut(&client_id).unwrap();
                match client.stream.read(&mut buf) {
                    Ok(0) => Err(io::Error::new(io::ErrorKind::ConnectionReset, "eof")),
                    Ok(n) => {
                        client.read_buf.extend_from_slice(&buf[..n]);
                        Ok(())
                    }
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => Ok(()),
                    Err(e) => Err(e),
                }
            };

            if let Err(e) = read_result {
                debug!(client_id, "client disconnected: {}", e);
                disconnected.push(client_id);
                continue;
            }

            // Extract and dispatch complete messages
            let messages = {
                let client = state.ipc_server.clients.get_mut(&client_id).unwrap();
                client.extract_messages()
            };

            for msg_str in messages {
                // Rate limit check
                let rate_ok = state
                    .ipc_server
                    .clients
                    .get_mut(&client_id)
                    .map(|c| c.rate_limiter.check())
                    .unwrap_or(false);

                if !rate_ok {
                    warn!(client_id, "rate limit exceeded, dropping message");
                    let resp = format!(
                        "(:type :response :id 0 :status :error :reason \"rate limit exceeded\")"
                    );
                    if let Some(client) = state.ipc_server.clients.get_mut(&client_id) {
                        client.enqueue_message(&resp);
                    }
                    continue;
                }

                if state.ipc_server.ipc_trace {
                    info!(client_id, "<< {}", msg_str);
                }
                state.ipc_server.recorder.record(
                    client_id,
                    MessageDirection::Request,
                    &msg_str,
                );
                let response = dispatch::handle_message(state, client_id, &msg_str);
                if let Some(ref resp) = response {
                    if state.ipc_server.ipc_trace {
                        info!(client_id, ">> {}", resp);
                    }
                    state.ipc_server.recorder.record(
                        client_id,
                        MessageDirection::Response,
                        resp,
                    );
                    if let Some(client) = state.ipc_server.clients.get_mut(&client_id) {
                        client.enqueue_message(resp);
                    }
                }
            }

            // Flush writes
            if let Some(client) = state.ipc_server.clients.get_mut(&client_id) {
                if let Err(e) = client.flush_writes() {
                    debug!(client_id, "write error: {}", e);
                    disconnected.push(client_id);
                }
            }
        }

        // Clean up disconnected clients
        for id in disconnected {
            info!(client_id = id, "removing disconnected IPC client");
            state.ipc_server.clients.remove(&id);
        }
    }

    /// Broadcast an event to all authenticated clients.
    pub fn broadcast_event(state: &mut crate::state::EwwmState, event: &str) {
        if state.ipc_server.ipc_trace {
            info!("broadcast >> {}", event);
        }
        state.ipc_server.recorder.record(
            0,
            MessageDirection::Event,
            event,
        );
        for client in state.ipc_server.clients.values_mut() {
            if client.authenticated {
                client.enqueue_event(event);
            }
        }
    }
}
