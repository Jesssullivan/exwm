//! Gaze zone modifier system — maps gaze position within a surface to
//! Emacs modifier keys and commands based on spatial zones.
//!
//! Divides each surface into 9 zones (4 corners, 4 edges, center) and
//! activates zone-specific modifiers after a configurable dwell time.
//! No openxrs dependency; compiles unconditionally.

use tracing::{debug, info, warn};

// ── GazeZone ────────────────────────────────────────────────

/// Spatial zone within a surface, determined by gaze pixel coordinates.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GazeZone {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
    TopEdge,
    BottomEdge,
    LeftEdge,
    RightEdge,
    Center,
}

impl GazeZone {
    /// String representation for IPC and logging.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::TopLeft => "top-left",
            Self::TopRight => "top-right",
            Self::BottomLeft => "bottom-left",
            Self::BottomRight => "bottom-right",
            Self::TopEdge => "top-edge",
            Self::BottomEdge => "bottom-edge",
            Self::LeftEdge => "left-edge",
            Self::RightEdge => "right-edge",
            Self::Center => "center",
        }
    }

    /// Parse a zone from its string representation.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "top-left" => Some(Self::TopLeft),
            "top-right" => Some(Self::TopRight),
            "bottom-left" => Some(Self::BottomLeft),
            "bottom-right" => Some(Self::BottomRight),
            "top-edge" => Some(Self::TopEdge),
            "bottom-edge" => Some(Self::BottomEdge),
            "left-edge" => Some(Self::LeftEdge),
            "right-edge" => Some(Self::RightEdge),
            "center" => Some(Self::Center),
            _ => None,
        }
    }

    /// Default Emacs modifier or command string for this zone.
    pub fn default_modifier(&self) -> &'static str {
        match self {
            Self::TopLeft => "C-x",
            Self::TopRight => "M-x",
            Self::BottomLeft => "C-",
            Self::BottomRight => "M-",
            Self::TopEdge => "scroll-up",
            Self::BottomEdge => "scroll-down",
            Self::LeftEdge => "C-b",
            Self::RightEdge => "C-f",
            Self::Center => "",
        }
    }
}

// ── ZoneConfig ──────────────────────────────────────────────

/// Configuration for zone geometry and timing.
#[derive(Debug, Clone)]
pub struct ZoneConfig {
    /// Fraction of surface width/height defining corners (0.15 = 15%).
    pub corner_fraction: f32,
    /// Fraction of surface depth for edge zones.
    pub edge_depth: f32,
    /// Milliseconds of sustained gaze in zone before activation.
    pub dwell_ms: f64,
    /// Milliseconds before a zone can re-activate after deactivation.
    pub lock_ms: f64,
    /// Alpha value for semi-transparent zone overlay.
    pub overlay_alpha: f32,
    /// Milliseconds for overlay fade-in animation.
    pub fade_in_ms: f64,
    /// Milliseconds for overlay fade-out animation.
    pub fade_out_ms: f64,
}

impl Default for ZoneConfig {
    fn default() -> Self {
        Self {
            corner_fraction: 0.15,
            edge_depth: 0.15,
            dwell_ms: 200.0,
            lock_ms: 500.0,
            overlay_alpha: 0.15,
            fade_in_ms: 100.0,
            fade_out_ms: 200.0,
        }
    }
}

// ── ZoneColors ──────────────────────────────────────────────

/// Per-zone RGBA overlay colors for visual feedback.
#[derive(Debug, Clone)]
pub struct ZoneColors {
    /// Blue tint for top-left corner.
    pub top_left: [f32; 4],
    /// Green tint for top-right corner.
    pub top_right: [f32; 4],
    /// Orange tint for bottom-left corner.
    pub bottom_left: [f32; 4],
    /// Purple tint for bottom-right corner.
    pub bottom_right: [f32; 4],
}

impl Default for ZoneColors {
    fn default() -> Self {
        Self {
            top_left: [0.2, 0.4, 0.8, 0.15],
            top_right: [0.2, 0.8, 0.4, 0.15],
            bottom_left: [0.9, 0.6, 0.2, 0.15],
            bottom_right: [0.6, 0.3, 0.8, 0.15],
        }
    }
}

// ── ZoneEvent ───────────────────────────────────────────────

/// Events emitted by the zone detection pipeline.
#[derive(Debug, Clone)]
pub enum ZoneEvent {
    /// Gaze entered a new zone on a surface.
    ZoneEntered {
        zone: GazeZone,
        surface_id: u64,
    },
    /// Zone dwell threshold reached — modifier should be applied.
    ZoneActivated {
        zone: GazeZone,
        surface_id: u64,
        modifier: String,
    },
    /// Previously active zone was exited.
    ZoneDeactivated {
        zone: GazeZone,
        surface_id: u64,
    },
    /// Progress toward zone activation (for overlay animation).
    ZoneDwellProgress {
        zone: GazeZone,
        elapsed_ms: f64,
        threshold_ms: f64,
    },
}

impl ZoneEvent {
    /// Convert the event to an IPC s-expression.
    pub fn to_sexp(&self) -> String {
        match self {
            Self::ZoneEntered { zone, surface_id } => {
                format!(
                    "(:type :event :event :zone-entered :zone :{} :surface-id {})",
                    zone.as_str(),
                    surface_id,
                )
            }
            Self::ZoneActivated {
                zone,
                surface_id,
                modifier,
            } => {
                format!(
                    "(:type :event :event :zone-activated :zone :{} :surface-id {} :modifier \"{}\")",
                    zone.as_str(),
                    surface_id,
                    modifier,
                )
            }
            Self::ZoneDeactivated { zone, surface_id } => {
                format!(
                    "(:type :event :event :zone-deactivated :zone :{} :surface-id {})",
                    zone.as_str(),
                    surface_id,
                )
            }
            Self::ZoneDwellProgress {
                zone,
                elapsed_ms,
                threshold_ms,
            } => {
                format!(
                    "(:type :event :event :zone-dwell-progress :zone :{} :elapsed-ms {:.0} :threshold-ms {:.0})",
                    zone.as_str(),
                    elapsed_ms,
                    threshold_ms,
                )
            }
        }
    }
}

// ── ZoneDetector ────────────────────────────────────────────

/// Tracks gaze position within surfaces and emits zone-based modifier events.
pub struct ZoneDetector {
    /// Zone geometry and timing configuration.
    pub config: ZoneConfig,
    /// Per-zone overlay colors.
    pub colors: ZoneColors,
    /// Zone the gaze is currently in (before dwell confirmation).
    pub current_zone: Option<GazeZone>,
    /// Accumulated dwell time in the current zone (milliseconds).
    pub dwell_elapsed_ms: f64,
    /// Zone that has been activated (dwell threshold met).
    pub active_zone: Option<GazeZone>,
    /// Remaining lock time before zone can re-activate (milliseconds).
    pub lock_remaining_ms: f64,
    /// Whether zone detection is enabled.
    pub enabled: bool,
}

impl ZoneDetector {
    pub fn new() -> Self {
        info!("Gaze zone detector initialized");
        Self {
            config: ZoneConfig::default(),
            colors: ZoneColors::default(),
            current_zone: None,
            dwell_elapsed_ms: 0.0,
            active_zone: None,
            lock_remaining_ms: 0.0,
            enabled: true,
        }
    }

    /// Classify pixel coordinates into a zone based on surface dimensions.
    pub fn classify(
        &self,
        pixel_x: f32,
        pixel_y: f32,
        surface_width: f32,
        surface_height: f32,
    ) -> GazeZone {
        if surface_width <= 0.0 || surface_height <= 0.0 {
            return GazeZone::Center;
        }

        let frac_x = pixel_x / surface_width;
        let frac_y = pixel_y / surface_height;

        let cf = self.config.corner_fraction;
        let ed = self.config.edge_depth;

        let in_left = frac_x < cf;
        let in_right = frac_x > (1.0 - cf);
        let in_top = frac_y < cf;
        let in_bottom = frac_y > (1.0 - cf);

        // Corners take priority
        if in_left && in_top {
            return GazeZone::TopLeft;
        }
        if in_right && in_top {
            return GazeZone::TopRight;
        }
        if in_left && in_bottom {
            return GazeZone::BottomLeft;
        }
        if in_right && in_bottom {
            return GazeZone::BottomRight;
        }

        // Edges (excluding corners already matched)
        if frac_y < ed {
            return GazeZone::TopEdge;
        }
        if frac_y > (1.0 - ed) {
            return GazeZone::BottomEdge;
        }
        if frac_x < ed {
            return GazeZone::LeftEdge;
        }
        if frac_x > (1.0 - ed) {
            return GazeZone::RightEdge;
        }

        GazeZone::Center
    }

    /// Update zone tracking with a new gaze position and delta time.
    ///
    /// Returns a `ZoneEvent` when a state transition occurs (enter, activate,
    /// deactivate, or dwell progress).
    pub fn update(
        &mut self,
        pixel_x: f32,
        pixel_y: f32,
        surface_width: f32,
        surface_height: f32,
        surface_id: u64,
        dt_ms: f64,
    ) -> Option<ZoneEvent> {
        if !self.enabled {
            return None;
        }

        // Tick the lock timer
        if self.lock_remaining_ms > 0.0 {
            self.lock_remaining_ms -= dt_ms;
            if self.lock_remaining_ms < 0.0 {
                self.lock_remaining_ms = 0.0;
            }
        }

        let zone = self.classify(pixel_x, pixel_y, surface_width, surface_height);

        // Zone changed — handle transition
        if self.current_zone != Some(zone) {
            // Deactivate previous zone if it was active
            let deactivation_event = if let Some(prev_zone) = self.active_zone {
                self.active_zone = None;
                self.lock_remaining_ms = self.config.lock_ms;
                debug!("Zone deactivated: {}, lock {:.0}ms", prev_zone.as_str(), self.config.lock_ms);
                Some(ZoneEvent::ZoneDeactivated {
                    zone: prev_zone,
                    surface_id,
                })
            } else {
                None
            };

            // If there was a deactivation, emit that first
            if deactivation_event.is_some() {
                self.current_zone = Some(zone);
                self.dwell_elapsed_ms = 0.0;
                return deactivation_event;
            }

            // Enter new zone
            self.current_zone = Some(zone);
            self.dwell_elapsed_ms = 0.0;
            debug!("Zone entered: {} on surface {}", zone.as_str(), surface_id);
            return Some(ZoneEvent::ZoneEntered {
                zone,
                surface_id,
            });
        }

        // Same zone — accumulate dwell time
        self.dwell_elapsed_ms += dt_ms;

        // Already activated — no further events
        if self.active_zone.is_some() {
            return None;
        }

        // Check activation lock
        if self.lock_remaining_ms > 0.0 {
            return None;
        }

        // Check dwell threshold
        if self.dwell_elapsed_ms >= self.config.dwell_ms {
            let modifier = zone.default_modifier().to_string();
            self.active_zone = Some(zone);
            info!(
                "Zone activated: {} on surface {}, modifier \"{}\"",
                zone.as_str(),
                surface_id,
                modifier,
            );
            return Some(ZoneEvent::ZoneActivated {
                zone,
                surface_id,
                modifier,
            });
        }

        // Dwell progress
        Some(ZoneEvent::ZoneDwellProgress {
            zone,
            elapsed_ms: self.dwell_elapsed_ms,
            threshold_ms: self.config.dwell_ms,
        })
    }

    /// Reset all zone tracking state.
    pub fn reset(&mut self) {
        self.current_zone = None;
        self.dwell_elapsed_ms = 0.0;
        self.active_zone = None;
        self.lock_remaining_ms = 0.0;
    }

    /// Generate IPC status s-expression.
    pub fn status_sexp(&self) -> String {
        let current = self
            .current_zone
            .map(|z| format!(":{}", z.as_str()))
            .unwrap_or_else(|| "nil".to_string());
        let active = self
            .active_zone
            .map(|z| format!(":{}", z.as_str()))
            .unwrap_or_else(|| "nil".to_string());
        format!(
            "(:enabled {} :current-zone {} :active-zone {} :dwell-elapsed-ms {:.0} :lock-remaining-ms {:.0})",
            if self.enabled { "t" } else { "nil" },
            current,
            active,
            self.dwell_elapsed_ms,
            self.lock_remaining_ms,
        )
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        format!(
            "(:corner-fraction {:.2} :edge-depth {:.2} :dwell-ms {:.0} :lock-ms {:.0} :overlay-alpha {:.2} :fade-in-ms {:.0} :fade-out-ms {:.0})",
            self.config.corner_fraction,
            self.config.edge_depth,
            self.config.dwell_ms,
            self.config.lock_ms,
            self.config.overlay_alpha,
            self.config.fade_in_ms,
            self.config.fade_out_ms,
        )
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_classify_corners() {
        let det = ZoneDetector::new();
        // 1000x1000 surface, corners are 15% = 150px

        // Top-left: (10, 10) well within 0..150, 0..150
        assert_eq!(det.classify(10.0, 10.0, 1000.0, 1000.0), GazeZone::TopLeft);

        // Top-right: (950, 10)
        assert_eq!(det.classify(950.0, 10.0, 1000.0, 1000.0), GazeZone::TopRight);

        // Bottom-left: (10, 950)
        assert_eq!(det.classify(10.0, 950.0, 1000.0, 1000.0), GazeZone::BottomLeft);

        // Bottom-right: (950, 950)
        assert_eq!(det.classify(950.0, 950.0, 1000.0, 1000.0), GazeZone::BottomRight);
    }

    #[test]
    fn test_classify_edges() {
        let det = ZoneDetector::new();
        // 1000x1000 surface, edge_depth 15% = 150px
        // Edges exclude corners, so x must be in [150..850] for top/bottom edges

        // Top edge: (500, 50) — center-x, near top
        assert_eq!(det.classify(500.0, 50.0, 1000.0, 1000.0), GazeZone::TopEdge);

        // Bottom edge: (500, 950) — but 950 is also > 850 (corner region for y)
        // (500, 950): frac_x=0.5, frac_y=0.95. in_bottom=true but not in_left/in_right.
        // frac_y > (1-0.15)=0.85, so BottomEdge
        assert_eq!(det.classify(500.0, 920.0, 1000.0, 1000.0), GazeZone::BottomEdge);

        // Left edge: (50, 500) — near left, center-y
        assert_eq!(det.classify(50.0, 500.0, 1000.0, 1000.0), GazeZone::LeftEdge);

        // Right edge: (960, 500) — near right, center-y
        assert_eq!(det.classify(960.0, 500.0, 1000.0, 1000.0), GazeZone::RightEdge);
    }

    #[test]
    fn test_classify_center() {
        let det = ZoneDetector::new();
        // Center: (500, 500) — middle of 1000x1000 surface
        assert_eq!(det.classify(500.0, 500.0, 1000.0, 1000.0), GazeZone::Center);

        // Also center: (300, 300) — outside corners and edges
        assert_eq!(det.classify(300.0, 300.0, 1000.0, 1000.0), GazeZone::Center);

        // Degenerate surface — should default to Center
        assert_eq!(det.classify(0.0, 0.0, 0.0, 0.0), GazeZone::Center);
    }

    #[test]
    fn test_zone_dwell_activation() {
        let mut det = ZoneDetector::new();
        det.config.dwell_ms = 100.0;
        det.config.lock_ms = 500.0;

        // First update enters the zone
        let evt = det.update(10.0, 10.0, 1000.0, 1000.0, 1, 0.0);
        assert!(matches!(evt, Some(ZoneEvent::ZoneEntered { .. })));

        // Accumulate 50ms — should get progress
        let evt = det.update(10.0, 10.0, 1000.0, 1000.0, 1, 50.0);
        assert!(matches!(evt, Some(ZoneEvent::ZoneDwellProgress { .. })));

        // Accumulate another 60ms (total 110ms > 100ms threshold)
        let evt = det.update(10.0, 10.0, 1000.0, 1000.0, 1, 60.0);
        match evt {
            Some(ZoneEvent::ZoneActivated { zone, modifier, .. }) => {
                assert_eq!(zone, GazeZone::TopLeft);
                assert_eq!(modifier, "C-x");
            }
            other => panic!("Expected ZoneActivated, got {:?}", other),
        }
    }

    #[test]
    fn test_zone_lock_prevents_rapid() {
        let mut det = ZoneDetector::new();
        det.config.dwell_ms = 50.0;
        det.config.lock_ms = 500.0;

        // Enter and activate top-left
        det.update(10.0, 10.0, 1000.0, 1000.0, 1, 0.0);
        det.update(10.0, 10.0, 1000.0, 1000.0, 1, 60.0);

        // Move to center (deactivates top-left, sets lock)
        let evt = det.update(500.0, 500.0, 1000.0, 1000.0, 1, 10.0);
        assert!(matches!(evt, Some(ZoneEvent::ZoneDeactivated { .. })));

        // Move back to top-left — enters zone
        let evt = det.update(10.0, 10.0, 1000.0, 1000.0, 1, 10.0);
        assert!(matches!(evt, Some(ZoneEvent::ZoneEntered { .. })));

        // Try to dwell 60ms — should NOT activate because lock is still active (500ms lock, only 20ms elapsed)
        let evt = det.update(10.0, 10.0, 1000.0, 1000.0, 1, 60.0);
        // Lock prevents activation, so no ZoneActivated event
        assert!(
            !matches!(evt, Some(ZoneEvent::ZoneActivated { .. })),
            "Lock should prevent rapid re-activation"
        );

        // After lock expires (wait 500ms total), should be able to activate
        let evt = det.update(10.0, 10.0, 1000.0, 1000.0, 1, 500.0);
        // Dwell elapsed is now 560ms > 50ms threshold, lock expired
        assert!(
            matches!(evt, Some(ZoneEvent::ZoneActivated { .. })),
            "Should activate after lock expires"
        );
    }

    #[test]
    fn test_zone_event_sexp() {
        let evt = ZoneEvent::ZoneEntered {
            zone: GazeZone::TopLeft,
            surface_id: 42,
        };
        let sexp = evt.to_sexp();
        assert!(sexp.contains(":event :zone-entered"));
        assert!(sexp.contains(":zone :top-left"));
        assert!(sexp.contains(":surface-id 42"));

        let evt = ZoneEvent::ZoneActivated {
            zone: GazeZone::BottomRight,
            surface_id: 7,
            modifier: "M-".to_string(),
        };
        let sexp = evt.to_sexp();
        assert!(sexp.contains(":event :zone-activated"));
        assert!(sexp.contains(":zone :bottom-right"));
        assert!(sexp.contains(":modifier \"M-\""));

        let evt = ZoneEvent::ZoneDwellProgress {
            zone: GazeZone::Center,
            elapsed_ms: 100.0,
            threshold_ms: 200.0,
        };
        let sexp = evt.to_sexp();
        assert!(sexp.contains(":elapsed-ms 100"));
        assert!(sexp.contains(":threshold-ms 200"));
    }

    #[test]
    fn test_default_modifiers() {
        assert_eq!(GazeZone::TopLeft.default_modifier(), "C-x");
        assert_eq!(GazeZone::TopRight.default_modifier(), "M-x");
        assert_eq!(GazeZone::BottomLeft.default_modifier(), "C-");
        assert_eq!(GazeZone::BottomRight.default_modifier(), "M-");
        assert_eq!(GazeZone::TopEdge.default_modifier(), "scroll-up");
        assert_eq!(GazeZone::BottomEdge.default_modifier(), "scroll-down");
        assert_eq!(GazeZone::LeftEdge.default_modifier(), "C-b");
        assert_eq!(GazeZone::RightEdge.default_modifier(), "C-f");
        assert_eq!(GazeZone::Center.default_modifier(), "");
    }

    #[test]
    fn test_zone_roundtrip() {
        let zones = vec![
            "top-left", "top-right", "bottom-left", "bottom-right",
            "top-edge", "bottom-edge", "left-edge", "right-edge",
            "center",
        ];
        for s in &zones {
            let z = GazeZone::from_str(s).expect(&format!("Should parse '{}'", s));
            assert_eq!(z.as_str(), *s);
        }
        assert_eq!(GazeZone::from_str("invalid"), None);
    }

    #[test]
    fn test_status_and_config_sexp() {
        let det = ZoneDetector::new();
        let status = det.status_sexp();
        assert!(status.contains(":enabled t"));
        assert!(status.contains(":current-zone nil"));
        assert!(status.contains(":active-zone nil"));

        let config = det.config_sexp();
        assert!(config.contains(":corner-fraction 0.15"));
        assert!(config.contains(":dwell-ms 200"));
        assert!(config.contains(":lock-ms 500"));
    }
}
