//! DRM lease management for VR headset displays.
//!
//! Implements DRM connector detection and lease lifecycle:
//! - Enumerate DRM connectors, identify non-desktop (HMD) outputs
//! - Track lease state: advertise -> request -> grant -> revoke
//! - HMD hotplug detection (connect/disconnect)
//! - Display mode negotiation (resolution, refresh rate)
//! - Multi-HMD awareness and selection

use std::collections::HashMap;
use std::time::Instant;
use tracing::{debug, info, warn};

// ── DRM Connector ────────────────────────────────────────────

/// Type of DRM connector.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectorType {
    DisplayPort,
    Hdmi,
    UsbC,
    Virtual,
    Unknown,
}

impl ConnectorType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::DisplayPort => "DP",
            Self::Hdmi => "HDMI",
            Self::UsbC => "USB-C",
            Self::Virtual => "Virtual",
            Self::Unknown => "Unknown",
        }
    }
}

/// Display mode (resolution + refresh rate).
#[derive(Debug, Clone)]
pub struct DisplayMode {
    pub width: u32,
    pub height: u32,
    pub refresh_hz: u32,
    pub preferred: bool,
}

impl DisplayMode {
    pub fn resolution_str(&self) -> String {
        format!("{}x{}@{}Hz", self.width, self.height, self.refresh_hz)
    }
}

/// Information about a DRM connector (desktop or non-desktop).
#[derive(Debug, Clone)]
pub struct DrmConnectorInfo {
    pub connector_id: u32,
    pub connector_name: String,
    pub connector_type: ConnectorType,
    pub non_desktop: bool,
    pub connected: bool,
    pub manufacturer: String,
    pub model: String,
    pub serial: String,
    pub modes: Vec<DisplayMode>,
}

impl DrmConnectorInfo {
    /// Get the preferred (highest resolution, highest refresh) mode.
    pub fn preferred_mode(&self) -> Option<&DisplayMode> {
        self.modes
            .iter()
            .find(|m| m.preferred)
            .or_else(|| {
                self.modes.iter().max_by_key(|m| {
                    (m.width as u64 * m.height as u64, m.refresh_hz)
                })
            })
    }

    /// Get available refresh rates for the highest resolution.
    pub fn available_refresh_rates(&self) -> Vec<u32> {
        if let Some(best) = self.preferred_mode() {
            let w = best.width;
            let h = best.height;
            self.modes
                .iter()
                .filter(|m| m.width == w && m.height == h)
                .map(|m| m.refresh_hz)
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Generate IPC s-expression for this connector.
    pub fn to_sexp(&self) -> String {
        let rates: Vec<String> = self.available_refresh_rates()
            .iter()
            .map(|r| r.to_string())
            .collect();
        let preferred = self.preferred_mode();
        let (w, h, hz) = preferred
            .map(|m| (m.width, m.height, m.refresh_hz))
            .unwrap_or((0, 0, 0));

        format!(
            "(:connector \"{}\" :type :{} :name \"{}\" :model \"{}\" :non-desktop {} :connected {} :resolution (:w {} :h {}) :refresh-rate {} :refresh-rates ({}))",
            self.connector_name,
            self.connector_type.as_str(),
            self.manufacturer,
            self.model,
            if self.non_desktop { "t" } else { "nil" },
            if self.connected { "t" } else { "nil" },
            w, h, hz,
            rates.join(" "),
        )
    }
}

// ── Lease state ──────────────────────────────────────────────

/// State of a DRM lease for an HMD connector.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LeaseStatus {
    /// Connector available, no active lease.
    Available,
    /// Lease requested by a client.
    Requested,
    /// Lease granted, client has DRM fd.
    Active,
    /// Lease being revoked.
    Revoking,
}

/// Tracking data for an active DRM lease.
#[derive(Debug)]
pub struct LeaseState {
    pub connector_id: u32,
    pub lessee_id: u32,
    pub client_pid: Option<u32>,
    pub status: LeaseStatus,
    pub granted_at: Option<Instant>,
}

impl LeaseState {
    pub fn new(connector_id: u32) -> Self {
        Self {
            connector_id,
            lessee_id: 0,
            client_pid: None,
            status: LeaseStatus::Available,
            granted_at: None,
        }
    }

    /// Grant the lease.
    pub fn grant(&mut self, lessee_id: u32, client_pid: Option<u32>) {
        self.lessee_id = lessee_id;
        self.client_pid = client_pid;
        self.status = LeaseStatus::Active;
        self.granted_at = Some(Instant::now());
        info!(
            "DRM lease granted: connector {} -> lessee {} (pid {:?})",
            self.connector_id, lessee_id, client_pid
        );
    }

    /// Revoke the lease.
    pub fn revoke(&mut self) {
        info!(
            "DRM lease revoked: connector {} (lessee {})",
            self.connector_id, self.lessee_id
        );
        self.status = LeaseStatus::Available;
        self.lessee_id = 0;
        self.client_pid = None;
        self.granted_at = None;
    }

    /// Check if lease is currently active.
    pub fn is_active(&self) -> bool {
        self.status == LeaseStatus::Active
    }
}

// ── VR Display Mode ──────────────────────────────────────────

/// Operating mode for VR display output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VrDisplayMode {
    /// Direct mode via DRM lease to physical HMD.
    Headset,
    /// Desktop window preview (no HMD required).
    Preview,
    /// Headless mode (no display output, for testing).
    Headless,
    /// VR disabled.
    Off,
}

impl VrDisplayMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Headset => "headset",
            Self::Preview => "preview",
            Self::Headless => "headless",
            Self::Off => "off",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "headset" => Some(Self::Headset),
            "preview" => Some(Self::Preview),
            "headless" => Some(Self::Headless),
            "off" => Some(Self::Off),
            _ => None,
        }
    }
}

impl Default for VrDisplayMode {
    fn default() -> Self {
        Self::Off
    }
}

// ── HMD Manager ──────────────────────────────────────────────

/// Manages HMD discovery, DRM leases, and display mode.
pub struct HmdManager {
    /// All detected DRM connectors.
    pub connectors: Vec<DrmConnectorInfo>,
    /// Non-desktop (HMD) connectors only.
    pub hmd_connectors: Vec<DrmConnectorInfo>,
    /// Lease state per connector ID.
    pub leases: HashMap<u32, LeaseState>,
    /// Currently selected HMD connector ID.
    pub selected_hmd: Option<u32>,
    /// Current display mode.
    pub display_mode: VrDisplayMode,
    /// Target refresh rate.
    pub target_refresh_rate: u32,
    /// Active refresh rate.
    pub active_refresh_rate: u32,
    /// Hotplug debounce: ignore events within this window.
    pub hotplug_debounce_ms: u64,
    /// Last hotplug event time.
    last_hotplug: Option<Instant>,
}

impl Default for HmdManager {
    fn default() -> Self {
        Self {
            connectors: Vec::new(),
            hmd_connectors: Vec::new(),
            leases: HashMap::new(),
            selected_hmd: None,
            display_mode: VrDisplayMode::Off,
            target_refresh_rate: 90,
            active_refresh_rate: 0,
            hotplug_debounce_ms: 500,
            last_hotplug: None,
        }
    }
}

impl HmdManager {
    pub fn new() -> Self {
        info!("HMD manager initialized");
        Self::default()
    }

    /// Update connector list from DRM device enumeration.
    /// In a real implementation, this would call DRM ioctls.
    pub fn update_connectors(&mut self, connectors: Vec<DrmConnectorInfo>) {
        // Separate desktop and non-desktop connectors
        self.hmd_connectors = connectors
            .iter()
            .filter(|c| c.non_desktop && c.connected)
            .cloned()
            .collect();

        self.connectors = connectors;

        info!(
            "HMD manager: {} total connectors, {} HMD connectors",
            self.connectors.len(),
            self.hmd_connectors.len()
        );

        // Auto-select best HMD if none selected
        if self.selected_hmd.is_none() && !self.hmd_connectors.is_empty() {
            self.auto_select_hmd();
        }

        // Initialize lease state for HMD connectors
        for hmd in &self.hmd_connectors {
            self.leases
                .entry(hmd.connector_id)
                .or_insert_with(|| LeaseState::new(hmd.connector_id));
        }
    }

    /// Auto-select the best HMD (highest resolution).
    fn auto_select_hmd(&mut self) {
        let best = self
            .hmd_connectors
            .iter()
            .max_by_key(|c| {
                c.preferred_mode()
                    .map(|m| m.width as u64 * m.height as u64)
                    .unwrap_or(0)
            });

        if let Some(hmd) = best {
            self.selected_hmd = Some(hmd.connector_id);
            info!(
                "HMD manager: auto-selected {} ({})",
                hmd.connector_name, hmd.model
            );
        }
    }

    /// Manually select an HMD by connector ID.
    pub fn select_hmd(&mut self, connector_id: u32) -> bool {
        if self.hmd_connectors.iter().any(|c| c.connector_id == connector_id) {
            // Revoke existing lease if switching
            if let Some(old_id) = self.selected_hmd {
                if old_id != connector_id {
                    if let Some(lease) = self.leases.get_mut(&old_id) {
                        if lease.is_active() {
                            lease.revoke();
                        }
                    }
                }
            }
            self.selected_hmd = Some(connector_id);
            info!("HMD manager: manually selected connector {}", connector_id);
            true
        } else {
            warn!("HMD manager: connector {} not found or not an HMD", connector_id);
            false
        }
    }

    /// Get the currently selected HMD connector info.
    pub fn selected_hmd_info(&self) -> Option<&DrmConnectorInfo> {
        self.selected_hmd.and_then(|id| {
            self.hmd_connectors.iter().find(|c| c.connector_id == id)
        })
    }

    /// Set the display mode.
    pub fn set_display_mode(&mut self, mode: VrDisplayMode) {
        let old = self.display_mode;
        self.display_mode = mode;
        info!("HMD manager: display mode {:?} -> {:?}", old, mode);
    }

    /// Determine the best display mode based on available hardware.
    pub fn auto_detect_mode(&mut self) -> VrDisplayMode {
        let mode = if !self.hmd_connectors.is_empty() {
            VrDisplayMode::Headset
        } else if std::env::var("XRT_COMPOSITOR_FORCE_HEADLESS").is_ok() {
            VrDisplayMode::Headless
        } else {
            VrDisplayMode::Preview
        };
        self.set_display_mode(mode);
        mode
    }

    /// Set the target refresh rate. Returns the closest available rate.
    pub fn set_target_refresh_rate(&mut self, target_hz: u32) -> u32 {
        self.target_refresh_rate = target_hz;

        // Find closest available rate on selected HMD
        if let Some(hmd) = self.selected_hmd_info() {
            let rates = hmd.available_refresh_rates();
            if let Some(&closest) = rates.iter().min_by_key(|&&r| {
                (r as i64 - target_hz as i64).unsigned_abs()
            }) {
                self.active_refresh_rate = closest;
                info!(
                    "HMD manager: target {}Hz -> active {}Hz",
                    target_hz, closest
                );
                return closest;
            }
        }

        self.active_refresh_rate = target_hz;
        target_hz
    }

    /// Handle a hotplug event (connector connected/disconnected).
    /// Returns true if the event was processed (not debounced).
    pub fn handle_hotplug(&mut self, connector_id: u32, connected: bool) -> bool {
        // Debounce
        if let Some(last) = self.last_hotplug {
            if last.elapsed().as_millis() < self.hotplug_debounce_ms as u128 {
                debug!("HMD manager: hotplug debounced for connector {}", connector_id);
                return false;
            }
        }
        self.last_hotplug = Some(Instant::now());

        if connected {
            info!("HMD manager: connector {} connected", connector_id);
            // Update connector state
            if let Some(conn) = self.connectors.iter_mut().find(|c| c.connector_id == connector_id) {
                conn.connected = true;
                if conn.non_desktop {
                    let info = conn.clone();
                    if !self.hmd_connectors.iter().any(|c| c.connector_id == connector_id) {
                        self.hmd_connectors.push(info);
                    }
                    // Auto-select if no HMD selected
                    if self.selected_hmd.is_none() {
                        self.selected_hmd = Some(connector_id);
                    }
                }
            }
        } else {
            info!("HMD manager: connector {} disconnected", connector_id);
            // Revoke active lease
            if let Some(lease) = self.leases.get_mut(&connector_id) {
                if lease.is_active() {
                    lease.revoke();
                }
            }
            // Update connector state
            if let Some(conn) = self.connectors.iter_mut().find(|c| c.connector_id == connector_id) {
                conn.connected = false;
            }
            self.hmd_connectors.retain(|c| c.connector_id != connector_id);

            // If disconnected HMD was selected, fall back
            if self.selected_hmd == Some(connector_id) {
                self.selected_hmd = None;
                if !self.hmd_connectors.is_empty() {
                    self.auto_select_hmd();
                } else {
                    // No HMDs left, switch to preview
                    self.set_display_mode(VrDisplayMode::Preview);
                }
            }
        }

        true
    }

    /// Get display info as IPC s-expression.
    pub fn display_info_sexp(&self) -> String {
        let hmd_name = self
            .selected_hmd_info()
            .map(|h| h.model.as_str())
            .unwrap_or("none");
        let connector = self
            .selected_hmd_info()
            .map(|h| h.connector_name.as_str())
            .unwrap_or("none");

        let mut hmds = String::from("(");
        for hmd in &self.hmd_connectors {
            hmds.push_str(&hmd.to_sexp());
        }
        hmds.push(')');

        format!(
            "(:mode :{} :hmd \"{}\" :connector \"{}\" :refresh-rate {} :target-refresh-rate {} :hmds {})",
            self.display_mode.as_str(),
            hmd_name,
            connector,
            self.active_refresh_rate,
            self.target_refresh_rate,
            hmds,
        )
    }

    /// Get number of detected HMD connectors.
    pub fn hmd_count(&self) -> usize {
        self.hmd_connectors.len()
    }
}

// ── Tests ────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn mock_hmd_connector(id: u32, name: &str, model: &str) -> DrmConnectorInfo {
        DrmConnectorInfo {
            connector_id: id,
            connector_name: name.to_string(),
            connector_type: ConnectorType::DisplayPort,
            non_desktop: true,
            connected: true,
            manufacturer: "TestVR".to_string(),
            model: model.to_string(),
            serial: "SN001".to_string(),
            modes: vec![
                DisplayMode {
                    width: 2880,
                    height: 1600,
                    refresh_hz: 90,
                    preferred: true,
                },
                DisplayMode {
                    width: 2880,
                    height: 1600,
                    refresh_hz: 120,
                    preferred: false,
                },
                DisplayMode {
                    width: 2880,
                    height: 1600,
                    refresh_hz: 144,
                    preferred: false,
                },
            ],
        }
    }

    fn mock_desktop_connector(id: u32) -> DrmConnectorInfo {
        DrmConnectorInfo {
            connector_id: id,
            connector_name: format!("HDMI-{}", id),
            connector_type: ConnectorType::Hdmi,
            non_desktop: false,
            connected: true,
            manufacturer: "Dell".to_string(),
            model: "U2723QE".to_string(),
            serial: "DESK001".to_string(),
            modes: vec![DisplayMode {
                width: 3840,
                height: 2160,
                refresh_hz: 60,
                preferred: true,
            }],
        }
    }

    #[test]
    fn test_non_desktop_detection() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![
            mock_desktop_connector(1),
            mock_hmd_connector(2, "DP-3", "Valve Index"),
            mock_desktop_connector(3),
        ]);
        assert_eq!(mgr.hmd_count(), 1);
        assert_eq!(mgr.hmd_connectors[0].model, "Valve Index");
    }

    #[test]
    fn test_auto_select_hmd() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![
            mock_hmd_connector(1, "DP-1", "Low Res HMD"),
            mock_hmd_connector(2, "DP-2", "High Res HMD"),
        ]);
        // Both have same resolution, so first one (or max by key) is selected
        assert!(mgr.selected_hmd.is_some());
    }

    #[test]
    fn test_manual_select_hmd() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![
            mock_hmd_connector(1, "DP-1", "HMD A"),
            mock_hmd_connector(2, "DP-2", "HMD B"),
        ]);
        assert!(mgr.select_hmd(2));
        assert_eq!(mgr.selected_hmd, Some(2));
    }

    #[test]
    fn test_select_invalid_hmd() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![mock_hmd_connector(1, "DP-1", "HMD")]);
        assert!(!mgr.select_hmd(99));
    }

    #[test]
    fn test_lease_grant_revoke() {
        let mut lease = LeaseState::new(1);
        assert!(!lease.is_active());

        lease.grant(42, Some(1234));
        assert!(lease.is_active());
        assert_eq!(lease.lessee_id, 42);

        lease.revoke();
        assert!(!lease.is_active());
        assert_eq!(lease.lessee_id, 0);
    }

    #[test]
    fn test_refresh_rate_selection() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![mock_hmd_connector(1, "DP-1", "Index")]);

        // Exact match
        let rate = mgr.set_target_refresh_rate(90);
        assert_eq!(rate, 90);

        // Closest match
        let rate = mgr.set_target_refresh_rate(100);
        assert_eq!(rate, 90); // 90 is closer than 120
    }

    #[test]
    fn test_hotplug_connect() {
        let mut mgr = HmdManager::new();
        let hmd = mock_hmd_connector(1, "DP-1", "Index");
        mgr.connectors.push(hmd.clone());

        // Simulate disconnect first
        mgr.connectors[0].connected = false;

        // Reconnect
        let processed = mgr.handle_hotplug(1, true);
        assert!(processed);
    }

    #[test]
    fn test_hotplug_disconnect_fallback() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![mock_hmd_connector(1, "DP-1", "Index")]);
        mgr.set_display_mode(VrDisplayMode::Headset);

        // Disconnect the only HMD
        mgr.handle_hotplug(1, false);
        assert_eq!(mgr.display_mode, VrDisplayMode::Preview);
        assert!(mgr.selected_hmd.is_none());
    }

    #[test]
    fn test_auto_detect_mode_no_hmd() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![mock_desktop_connector(1)]);
        let mode = mgr.auto_detect_mode();
        assert_eq!(mode, VrDisplayMode::Preview);
    }

    #[test]
    fn test_auto_detect_mode_with_hmd() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![
            mock_desktop_connector(1),
            mock_hmd_connector(2, "DP-2", "Index"),
        ]);
        let mode = mgr.auto_detect_mode();
        assert_eq!(mode, VrDisplayMode::Headset);
    }

    #[test]
    fn test_display_info_sexp() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![mock_hmd_connector(1, "DP-1", "Index")]);
        mgr.set_display_mode(VrDisplayMode::Headset);
        mgr.set_target_refresh_rate(90);

        let sexp = mgr.display_info_sexp();
        assert!(sexp.contains(":mode :headset"));
        assert!(sexp.contains(":hmd \"Index\""));
        assert!(sexp.contains(":refresh-rate 90"));
    }

    #[test]
    fn test_connector_sexp() {
        let hmd = mock_hmd_connector(1, "DP-1", "Valve Index");
        let sexp = hmd.to_sexp();
        assert!(sexp.contains(":non-desktop t"));
        assert!(sexp.contains(":model \"Valve Index\""));
        assert!(sexp.contains("90 120 144"));
    }

    #[test]
    fn test_display_mode_from_str() {
        assert_eq!(VrDisplayMode::from_str("headset"), Some(VrDisplayMode::Headset));
        assert_eq!(VrDisplayMode::from_str("preview"), Some(VrDisplayMode::Preview));
        assert_eq!(VrDisplayMode::from_str("headless"), Some(VrDisplayMode::Headless));
        assert_eq!(VrDisplayMode::from_str("off"), Some(VrDisplayMode::Off));
        assert_eq!(VrDisplayMode::from_str("invalid"), None);
    }

    #[test]
    fn test_multi_hmd_enumeration() {
        let mut mgr = HmdManager::new();
        mgr.update_connectors(vec![
            mock_desktop_connector(1),
            mock_hmd_connector(2, "DP-2", "Valve Index"),
            mock_hmd_connector(3, "DP-3", "Quest Link"),
            mock_desktop_connector(4),
        ]);
        assert_eq!(mgr.hmd_count(), 2);
        assert!(mgr.selected_hmd.is_some());
    }
}
