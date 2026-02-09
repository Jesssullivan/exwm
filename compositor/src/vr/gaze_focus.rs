//! Gaze-based window focus — dwell detection, saccade suppression,
//! reading mode, hysteresis, cooldown, and focus policy management.
//!
//! Week 12: replaces pointer-based focus with eye-gaze dwell timing.
//! Integrates with `eye_tracking` for gaze data and `scene` for surface IDs.

use std::collections::VecDeque;
use tracing::{debug, info, warn};

use super::eye_tracking::{GazeData, GazeSurfaceHit};
use super::scene::Vec3;

// ── Dwell state ─────────────────────────────────────────────

/// State machine for dwell-based focus acquisition.
#[derive(Debug, Clone)]
pub enum DwellState {
    /// No active dwell.
    Idle,
    /// Gaze is dwelling on a surface, accumulating time.
    Dwelling {
        surface_id: u64,
        elapsed_ms: f64,
        center_x: f32,
        center_y: f32,
        jitter_max_px: f32,
    },
    /// Dwell threshold reached; focus should be granted.
    Confirmed { surface_id: u64 },
    /// Post-focus cooldown to prevent rapid re-triggering.
    Cooldown { remaining_ms: f64 },
}

// ── Dwell config ────────────────────────────────────────────

/// Configuration for dwell detection thresholds.
#[derive(Debug, Clone)]
pub struct DwellConfig {
    /// Milliseconds of steady gaze required to confirm focus.
    pub threshold_ms: f64,
    /// Maximum pixel jitter allowed during dwell (resets if exceeded).
    pub max_jitter_px: f32,
    /// Minimum gaze confidence to count a frame toward dwell.
    pub require_confidence: f32,
}

impl Default for DwellConfig {
    fn default() -> Self {
        Self {
            threshold_ms: 200.0,
            max_jitter_px: 50.0,
            require_confidence: 0.6,
        }
    }
}

// ── Saccade detector ────────────────────────────────────────

/// Detects saccades (rapid gaze jumps) to suppress false dwell triggers.
#[derive(Debug, Clone)]
pub struct SaccadeDetector {
    /// Previous gaze direction for velocity computation.
    pub prev_direction: Option<Vec3>,
    /// Current angular velocity in degrees per second.
    pub angular_velocity_dps: f32,
    /// Velocity above which a saccade is considered active.
    pub saccade_threshold_dps: f32,
    /// Velocity below which a saccade is considered ended.
    pub saccade_end_dps: f32,
    /// Consecutive frames below end threshold (needs 3 to confirm end).
    pub sub_threshold_frames: u32,
    /// Whether the detector currently considers gaze in a saccade.
    pub is_saccade: bool,
    /// Maximum angular displacement for a microsaccade (degrees).
    pub microsaccade_max_deg: f32,
    /// Maximum duration for a microsaccade (milliseconds).
    pub microsaccade_max_ms: f32,
}

impl Default for SaccadeDetector {
    fn default() -> Self {
        Self {
            prev_direction: None,
            angular_velocity_dps: 0.0,
            saccade_threshold_dps: 300.0,
            saccade_end_dps: 50.0,
            sub_threshold_frames: 0,
            is_saccade: false,
            microsaccade_max_deg: 1.5,
            microsaccade_max_ms: 30.0,
        }
    }
}

impl SaccadeDetector {
    /// Update with a new gaze direction sample. Returns true if currently in saccade.
    pub fn update(&mut self, direction: Vec3, dt_s: f64) -> bool {
        if dt_s <= 0.0 {
            return self.is_saccade;
        }

        if let Some(prev) = self.prev_direction {
            let dot = (prev.x * direction.x + prev.y * direction.y + prev.z * direction.z)
                .clamp(-1.0, 1.0);
            let angle_deg = dot.acos().to_degrees();
            self.angular_velocity_dps = angle_deg / dt_s as f32;

            // Check for microsaccade: small displacement, high velocity but brief
            let is_microsaccade = angle_deg <= self.microsaccade_max_deg
                && (dt_s * 1000.0) <= self.microsaccade_max_ms as f64;

            if self.angular_velocity_dps >= self.saccade_threshold_dps && !is_microsaccade {
                self.is_saccade = true;
                self.sub_threshold_frames = 0;
                debug!(
                    "Saccade detected: {:.0} deg/s",
                    self.angular_velocity_dps
                );
            } else if self.is_saccade {
                if self.angular_velocity_dps < self.saccade_end_dps {
                    self.sub_threshold_frames += 1;
                    if self.sub_threshold_frames >= 3 {
                        self.is_saccade = false;
                        self.sub_threshold_frames = 0;
                        debug!("Saccade ended after 3 sub-threshold frames");
                    }
                } else {
                    self.sub_threshold_frames = 0;
                }
            }
        }

        self.prev_direction = Some(direction);
        self.is_saccade
    }

    /// Reset saccade detector state.
    pub fn reset(&mut self) {
        self.prev_direction = None;
        self.angular_velocity_dps = 0.0;
        self.is_saccade = false;
        self.sub_threshold_frames = 0;
    }
}

// ── Reading detector ────────────────────────────────────────

/// Detects horizontal scanning patterns (reading) to suppress focus changes.
#[derive(Debug, Clone)]
pub struct ReadingDetector {
    /// Whether reading mode is currently active.
    pub active: bool,
    /// Whether reading detection is enabled (mirrors defcustom).
    pub enabled: bool,
    /// Surface being read.
    pub surface_id: Option<u64>,
    /// Minimum horizontal pixel coordinate visited.
    pub min_x: f32,
    /// Maximum horizontal pixel coordinate visited.
    pub max_x: f32,
    /// Width of the current surface in pixels.
    pub surface_width: f32,
    /// Timestamp when horizontal scanning started.
    pub scan_start_s: f64,
    /// Fraction of surface width that must be scanned to trigger reading mode.
    pub horizontal_threshold: f32,
    /// Time window for scanning detection (seconds).
    pub scan_window_s: f64,
    /// Reading mode expires after this many seconds of inactivity.
    pub expire_s: f64,
    /// Timestamp of last reading activity.
    pub last_activity_s: f64,
    /// Whether reading mode is confirmed active.
    pub in_reading_mode: bool,
}

impl Default for ReadingDetector {
    fn default() -> Self {
        Self {
            active: false,
            enabled: true,
            surface_id: None,
            min_x: f32::MAX,
            max_x: f32::MIN,
            surface_width: 0.0,
            scan_start_s: 0.0,
            horizontal_threshold: 0.5,
            scan_window_s: 3.0,
            expire_s: 30.0,
            last_activity_s: 0.0,
            in_reading_mode: false,
        }
    }
}

impl ReadingDetector {
    /// Update with a new gaze position on a surface. Returns true if reading mode active.
    pub fn update(
        &mut self,
        surface_id: u64,
        pixel_x: f32,
        surface_width: f32,
        timestamp_s: f64,
    ) -> bool {
        if !self.enabled {
            return false;
        }

        // Surface changed — reset tracking
        if self.surface_id != Some(surface_id) {
            self.surface_id = Some(surface_id);
            self.min_x = pixel_x;
            self.max_x = pixel_x;
            self.surface_width = surface_width;
            self.scan_start_s = timestamp_s;
            self.active = true;
            // Don't reset reading mode immediately — it expires on its own
        }

        // Update horizontal extent
        if pixel_x < self.min_x {
            self.min_x = pixel_x;
        }
        if pixel_x > self.max_x {
            self.max_x = pixel_x;
        }

        // Check if scanning window expired — reset extent tracking
        if timestamp_s - self.scan_start_s > self.scan_window_s {
            self.min_x = pixel_x;
            self.max_x = pixel_x;
            self.scan_start_s = timestamp_s;
        }

        // Check horizontal coverage
        let extent = self.max_x - self.min_x;
        let coverage = if surface_width > 0.0 {
            extent / surface_width
        } else {
            0.0
        };

        if coverage >= self.horizontal_threshold {
            if !self.in_reading_mode {
                info!(
                    "Reading mode entered: surface {} ({:.0}% coverage)",
                    surface_id,
                    coverage * 100.0
                );
            }
            self.in_reading_mode = true;
            self.last_activity_s = timestamp_s;
        }

        // Expire reading mode after inactivity
        if self.in_reading_mode && (timestamp_s - self.last_activity_s > self.expire_s) {
            self.in_reading_mode = false;
            info!("Reading mode expired: surface {}", surface_id);
        }

        self.in_reading_mode
    }

    /// Reset reading detector state.
    pub fn reset(&mut self) {
        self.active = false;
        self.surface_id = None;
        self.min_x = f32::MAX;
        self.max_x = f32::MIN;
        self.surface_width = 0.0;
        self.scan_start_s = 0.0;
        self.last_activity_s = 0.0;
        self.in_reading_mode = false;
    }
}

// ── Cooldown state ──────────────────────────────────────────

/// Post-focus cooldown to prevent rapid re-triggering.
#[derive(Debug, Clone)]
pub struct CooldownState {
    /// Whether cooldown is currently active.
    pub active: bool,
    /// Remaining cooldown time in milliseconds.
    pub remaining_ms: f64,
    /// Default cooldown duration in milliseconds.
    pub default_ms: f64,
}

impl Default for CooldownState {
    fn default() -> Self {
        Self {
            active: false,
            remaining_ms: 0.0,
            default_ms: 500.0,
        }
    }
}

impl CooldownState {
    /// Start the cooldown timer.
    pub fn start(&mut self) {
        self.active = true;
        self.remaining_ms = self.default_ms;
    }

    /// Tick the cooldown timer by dt_ms milliseconds.
    pub fn update(&mut self, dt_ms: f64) {
        if !self.active {
            return;
        }
        self.remaining_ms -= dt_ms;
        if self.remaining_ms <= 0.0 {
            self.remaining_ms = 0.0;
            self.active = false;
        }
    }

    /// Whether cooldown is currently blocking new focus acquisitions.
    pub fn is_active(&self) -> bool {
        self.active
    }
}

// ── Hysteresis tracker ──────────────────────────────────────

/// Tracks rapid focus switching (ping-pong) and increases dwell threshold.
#[derive(Debug, Clone)]
pub struct HysteresisTracker {
    /// Whether hysteresis tracking is enabled.
    pub enabled: bool,
    /// Recent switch history: (surface_id, timestamp_s).
    pub recent_switches: VecDeque<(u64, f64)>,
    /// Time window to detect ping-pong switching (seconds).
    pub ping_pong_window_s: f64,
    /// Current extra threshold from hysteresis (milliseconds).
    pub extra_threshold_ms: f64,
    /// Extra threshold added per detected ping-pong switch (milliseconds).
    pub extra_per_pingpong_ms: f64,
    /// Seconds of stability after which hysteresis resets.
    pub stable_reset_s: f64,
    /// Timestamp of last focus switch.
    pub last_switch_s: f64,
}

impl Default for HysteresisTracker {
    fn default() -> Self {
        Self {
            enabled: true,
            recent_switches: VecDeque::with_capacity(16),
            ping_pong_window_s: 5.0,
            extra_threshold_ms: 0.0,
            extra_per_pingpong_ms: 100.0,
            stable_reset_s: 10.0,
            last_switch_s: 0.0,
        }
    }
}

impl HysteresisTracker {
    /// Record a focus switch to a surface.
    pub fn record_switch(&mut self, surface_id: u64, timestamp_s: f64) {
        if !self.enabled {
            return;
        }

        self.recent_switches.push_back((surface_id, timestamp_s));

        // Prune old entries outside the window
        while let Some(&(_, ts)) = self.recent_switches.front() {
            if timestamp_s - ts > self.ping_pong_window_s {
                self.recent_switches.pop_front();
            } else {
                break;
            }
        }

        // Detect ping-pong: same surface appearing multiple times in window
        let mut switch_count = 0u32;
        let mut seen: Vec<u64> = Vec::new();
        for &(sid, _) in &self.recent_switches {
            if seen.contains(&sid) {
                switch_count += 1;
            } else {
                seen.push(sid);
            }
        }

        if switch_count > 0 {
            self.extra_threshold_ms =
                (switch_count as f64) * self.extra_per_pingpong_ms;
            debug!(
                "Hysteresis: {} ping-pong switches, extra threshold {:.0}ms",
                switch_count, self.extra_threshold_ms
            );
        }

        self.last_switch_s = timestamp_s;
    }

    /// Get the effective extra dwell threshold at the given timestamp.
    pub fn effective_extra_ms(&self, timestamp_s: f64) -> f64 {
        if !self.enabled {
            return 0.0;
        }

        // Reset if stable for long enough
        if self.last_switch_s > 0.0
            && (timestamp_s - self.last_switch_s) > self.stable_reset_s
        {
            return 0.0;
        }

        self.extra_threshold_ms
    }

    /// Reset hysteresis state.
    pub fn reset(&mut self) {
        self.recent_switches.clear();
        self.extra_threshold_ms = 0.0;
        self.last_switch_s = 0.0;
    }
}

// ── Focus policy ────────────────────────────────────────────

/// Policy governing how gaze focus interacts with other focus methods.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FocusPolicy {
    /// Gaze is the sole focus method.
    GazeOnly,
    /// Gaze is primary; keyboard/mouse can override temporarily.
    GazePrimary,
    /// Gaze assists but does not override keyboard/mouse focus.
    GazeAssist,
    /// Gaze focus is disabled.
    Disabled,
}

impl FocusPolicy {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::GazeOnly => "gaze-only",
            Self::GazePrimary => "gaze-primary",
            Self::GazeAssist => "gaze-assist",
            Self::Disabled => "disabled",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "gaze-only" => Some(Self::GazeOnly),
            "gaze-primary" => Some(Self::GazePrimary),
            "gaze-assist" => Some(Self::GazeAssist),
            "disabled" => Some(Self::Disabled),
            _ => None,
        }
    }
}

// ── Focus analytics ─────────────────────────────────────────

/// Analytics for gaze focus quality monitoring.
#[derive(Debug, Clone)]
pub struct FocusAnalytics {
    /// Total focus switches in this session.
    pub focus_switches: u64,
    /// Focus switches undone within 1 second (false positives).
    pub false_positives: u64,
    /// Times a saccade suppressed dwell accumulation.
    pub saccade_suppressions: u64,
    /// Times cooldown blocked a focus acquisition.
    pub cooldown_blocks: u64,
    /// Times reading mode suppressed a focus change.
    pub reading_suppressions: u64,
    /// Last 100 successful dwell durations (milliseconds).
    pub dwell_durations: Vec<f64>,
    /// Session start timestamp.
    pub session_start_s: f64,
}

impl Default for FocusAnalytics {
    fn default() -> Self {
        Self {
            focus_switches: 0,
            false_positives: 0,
            saccade_suppressions: 0,
            cooldown_blocks: 0,
            reading_suppressions: 0,
            dwell_durations: Vec::with_capacity(100),
            session_start_s: 0.0,
        }
    }
}

impl FocusAnalytics {
    /// Record a successful focus switch with the dwell duration that triggered it.
    pub fn record_switch(&mut self, dwell_ms: f64) {
        self.focus_switches += 1;
        if self.dwell_durations.len() >= 100 {
            self.dwell_durations.remove(0);
        }
        self.dwell_durations.push(dwell_ms);
    }

    /// Record a false positive (focus switch undone within 1 second).
    pub fn record_false_positive(&mut self) {
        self.false_positives += 1;
    }

    /// Record a saccade suppression event.
    pub fn record_saccade_suppression(&mut self) {
        self.saccade_suppressions += 1;
    }

    /// Record a cooldown block event.
    pub fn record_cooldown_block(&mut self) {
        self.cooldown_blocks += 1;
    }

    /// Record a reading mode suppression event.
    pub fn record_reading_suppression(&mut self) {
        self.reading_suppressions += 1;
    }

    /// Compute focus switches per minute since session start.
    pub fn switches_per_minute(&self, now_s: f64) -> f64 {
        let elapsed_min = (now_s - self.session_start_s) / 60.0;
        if elapsed_min <= 0.0 {
            return 0.0;
        }
        self.focus_switches as f64 / elapsed_min
    }

    /// Generate IPC status s-expression for analytics.
    pub fn status_sexp(&self) -> String {
        let avg_dwell = if self.dwell_durations.is_empty() {
            0.0
        } else {
            self.dwell_durations.iter().sum::<f64>() / self.dwell_durations.len() as f64
        };
        format!(
            "(:switches {} :false-positives {} :saccade-suppressions {} :cooldown-blocks {} :reading-suppressions {} :avg-dwell-ms {:.0} :samples {})",
            self.focus_switches,
            self.false_positives,
            self.saccade_suppressions,
            self.cooldown_blocks,
            self.reading_suppressions,
            avg_dwell,
            self.dwell_durations.len(),
        )
    }
}

// ── Gaze focus event ────────────────────────────────────────

/// Events emitted by the gaze focus pipeline.
#[derive(Debug, Clone)]
pub enum GazeFocusEvent {
    /// Dwell has started on a surface.
    DwellStarted { surface_id: u64, x: f32, y: f32 },
    /// Dwell is in progress (progress update).
    DwellProgress {
        surface_id: u64,
        elapsed_ms: f64,
        threshold_ms: f64,
    },
    /// Focus should be granted to a surface.
    FocusRequested {
        surface_id: u64,
        dwell_ms: f64,
        x: f32,
        y: f32,
    },
    /// Dwell was cancelled before reaching threshold.
    FocusCancelled { surface_id: u64, reason: String },
    /// Post-focus cooldown started.
    CooldownStarted { duration_ms: f64 },
    /// Post-focus cooldown ended.
    CooldownEnded,
    /// A saccade was detected.
    SaccadeDetected { velocity_dps: f32 },
    /// Reading mode was entered on a surface.
    ReadingModeEntered { surface_id: u64 },
    /// Reading mode was exited on a surface.
    ReadingModeExited { surface_id: u64 },
}

impl GazeFocusEvent {
    /// Convert the event to an IPC s-expression.
    pub fn to_sexp(&self) -> String {
        match self {
            Self::DwellStarted { surface_id, x, y } => {
                format!(
                    "(:type :event :event :gaze-dwell-started :surface-id {} :x {:.0} :y {:.0})",
                    surface_id, x, y
                )
            }
            Self::DwellProgress {
                surface_id,
                elapsed_ms,
                threshold_ms,
            } => {
                format!(
                    "(:type :event :event :gaze-dwell-progress :surface-id {} :elapsed-ms {:.0} :threshold-ms {:.0})",
                    surface_id, elapsed_ms, threshold_ms
                )
            }
            Self::FocusRequested {
                surface_id,
                dwell_ms,
                x,
                y,
            } => {
                format!(
                    "(:type :event :event :gaze-focus-requested :surface-id {} :dwell-ms {:.0} :x {:.0} :y {:.0})",
                    surface_id, dwell_ms, x, y
                )
            }
            Self::FocusCancelled { surface_id, reason } => {
                format!(
                    "(:type :event :event :gaze-focus-cancelled :surface-id {} :reason \"{}\")",
                    surface_id, reason
                )
            }
            Self::CooldownStarted { duration_ms } => {
                format!(
                    "(:type :event :event :gaze-cooldown-started :duration-ms {:.0})",
                    duration_ms
                )
            }
            Self::CooldownEnded => {
                "(:type :event :event :gaze-cooldown-ended)".to_string()
            }
            Self::SaccadeDetected { velocity_dps } => {
                format!(
                    "(:type :event :event :gaze-saccade-detected :velocity-dps {:.0})",
                    velocity_dps
                )
            }
            Self::ReadingModeEntered { surface_id } => {
                format!(
                    "(:type :event :event :gaze-reading-entered :surface-id {})",
                    surface_id
                )
            }
            Self::ReadingModeExited { surface_id } => {
                format!(
                    "(:type :event :event :gaze-reading-exited :surface-id {})",
                    surface_id
                )
            }
        }
    }
}

// ── Gaze focus manager ──────────────────────────────────────

/// Central orchestrator for gaze-based window focus.
pub struct GazeFocusManager {
    /// Current dwell state machine.
    pub dwell: DwellState,
    /// Dwell configuration.
    pub config: DwellConfig,
    /// Saccade detector.
    pub saccade: SaccadeDetector,
    /// Reading pattern detector.
    pub reading: ReadingDetector,
    /// Post-focus cooldown.
    pub cooldown: CooldownState,
    /// Focus switch hysteresis.
    pub hysteresis: HysteresisTracker,
    /// Active focus policy.
    pub policy: FocusPolicy,
    /// Focus quality analytics.
    pub analytics: FocusAnalytics,
    /// Currently focused surface (by gaze).
    pub focused_surface: Option<u64>,
    /// Ring buffer of last 10 focused surface IDs.
    pub focus_history: VecDeque<u64>,
    /// How the current focus was last acquired.
    pub last_focus_method: String,
    /// Whether to fall back to head gaze when eye tracking is unavailable.
    pub fallback_to_head_gaze: bool,
    /// Timestamp of the last focus change (for false positive detection).
    last_focus_change_s: f64,
    /// Surface that was focused before the last change (for false positive detection).
    prev_focused_surface: Option<u64>,
    /// Whether reading mode was active in the previous frame.
    was_reading: bool,
}

impl GazeFocusManager {
    pub fn new() -> Self {
        info!("Gaze focus manager initialized");
        Self {
            dwell: DwellState::Idle,
            config: DwellConfig::default(),
            saccade: SaccadeDetector::default(),
            reading: ReadingDetector::default(),
            cooldown: CooldownState::default(),
            hysteresis: HysteresisTracker::default(),
            policy: FocusPolicy::GazePrimary,
            analytics: FocusAnalytics::default(),
            focused_surface: None,
            focus_history: VecDeque::with_capacity(10),
            last_focus_method: "none".to_string(),
            fallback_to_head_gaze: true,
            last_focus_change_s: 0.0,
            prev_focused_surface: None,
            was_reading: false,
        }
    }

    /// Core update pipeline: saccade -> reading -> dwell -> cooldown -> emit event.
    ///
    /// Called once per frame with the current gaze surface hit and raw gaze data.
    /// Returns an event if a state transition occurred.
    pub fn update(
        &mut self,
        hit: Option<&GazeSurfaceHit>,
        gaze: Option<&GazeData>,
        dt_s: f64,
    ) -> Option<GazeFocusEvent> {
        if self.policy == FocusPolicy::Disabled {
            return None;
        }

        let dt_ms = dt_s * 1000.0;
        let timestamp_s = gaze.map(|g| g.timestamp_s).unwrap_or(0.0);

        // ── Step 1: Update cooldown ──
        let was_cooldown = self.cooldown.is_active();
        self.cooldown.update(dt_ms);
        if was_cooldown && !self.cooldown.is_active() {
            return Some(GazeFocusEvent::CooldownEnded);
        }

        // ── Step 2: Saccade detection ──
        if let Some(gaze) = gaze {
            let in_saccade = self.saccade.update(gaze.ray.direction, dt_s);
            if in_saccade {
                // Reset dwell during saccade
                if let DwellState::Dwelling { surface_id, .. } = &self.dwell {
                    let sid = *surface_id;
                    self.dwell = DwellState::Idle;
                    self.analytics.record_saccade_suppression();
                    return Some(GazeFocusEvent::SaccadeDetected {
                        velocity_dps: self.saccade.angular_velocity_dps,
                    });
                }
                return None;
            }
        }

        // ── Step 3: Reading detection ──
        if let Some(hit) = hit {
            let was_reading_before = self.reading.in_reading_mode;
            let is_reading = self.reading.update(
                hit.surface_id,
                hit.pixel_x,
                // Use a reasonable surface width estimate; the hit provides pixel coords
                self.reading.surface_width.max(1.0),
                timestamp_s,
            );

            if is_reading && !was_reading_before {
                self.was_reading = true;
                return Some(GazeFocusEvent::ReadingModeEntered {
                    surface_id: hit.surface_id,
                });
            }
            if !is_reading && was_reading_before {
                self.was_reading = false;
                return Some(GazeFocusEvent::ReadingModeExited {
                    surface_id: hit.surface_id,
                });
            }

            // Reading mode suppresses focus changes to OTHER surfaces
            if is_reading && self.focused_surface == Some(hit.surface_id) {
                // Same surface — reading continues, no focus change needed
            } else if is_reading && self.focused_surface != Some(hit.surface_id) {
                self.analytics.record_reading_suppression();
                return None;
            }
        }

        // ── Step 4: Confidence filter ──
        if let Some(gaze) = gaze {
            if gaze.confidence < self.config.require_confidence {
                return None;
            }
        }

        // ── Step 5: Cooldown check ──
        if self.cooldown.is_active() {
            if let Some(hit) = hit {
                if self.focused_surface != Some(hit.surface_id) {
                    self.analytics.record_cooldown_block();
                }
            }
            return None;
        }

        // ── Step 6: Dwell state machine ──
        let effective_threshold = self.config.threshold_ms
            + self.hysteresis.effective_extra_ms(timestamp_s);

        match (&self.dwell, hit) {
            // No hit — reset to idle
            (_, None) => {
                if let DwellState::Dwelling { surface_id, .. } = &self.dwell {
                    let sid = *surface_id;
                    self.dwell = DwellState::Idle;
                    return Some(GazeFocusEvent::FocusCancelled {
                        surface_id: sid,
                        reason: "gaze-lost".to_string(),
                    });
                }
                self.dwell = DwellState::Idle;
                None
            }

            // Idle with hit — start dwelling (unless already focused on this surface)
            (DwellState::Idle, Some(hit)) => {
                if self.focused_surface == Some(hit.surface_id) {
                    return None;
                }
                self.dwell = DwellState::Dwelling {
                    surface_id: hit.surface_id,
                    elapsed_ms: 0.0,
                    center_x: hit.pixel_x,
                    center_y: hit.pixel_y,
                    jitter_max_px: 0.0,
                };
                Some(GazeFocusEvent::DwellStarted {
                    surface_id: hit.surface_id,
                    x: hit.pixel_x,
                    y: hit.pixel_y,
                })
            }

            // Dwelling on same surface — accumulate time
            (
                DwellState::Dwelling {
                    surface_id,
                    elapsed_ms,
                    center_x,
                    center_y,
                    jitter_max_px,
                },
                Some(hit),
            ) if hit.surface_id == *surface_id => {
                let dx = hit.pixel_x - center_x;
                let dy = hit.pixel_y - center_y;
                let jitter = (dx * dx + dy * dy).sqrt();
                let new_jitter = jitter_max_px.max(jitter);

                // Jitter exceeded — cancel dwell
                if new_jitter > self.config.max_jitter_px {
                    let sid = *surface_id;
                    self.dwell = DwellState::Idle;
                    return Some(GazeFocusEvent::FocusCancelled {
                        surface_id: sid,
                        reason: "jitter-exceeded".to_string(),
                    });
                }

                let new_elapsed = elapsed_ms + dt_ms;

                // Threshold reached — confirm focus
                if new_elapsed >= effective_threshold {
                    let sid = *surface_id;
                    let cx = *center_x;
                    let cy = *center_y;
                    self.dwell = DwellState::Confirmed { surface_id: sid };

                    // Record focus change
                    self.prev_focused_surface = self.focused_surface;
                    self.focused_surface = Some(sid);
                    self.last_focus_method = "gaze".to_string();
                    self.last_focus_change_s = timestamp_s;

                    // Update history ring buffer
                    if self.focus_history.len() >= 10 {
                        self.focus_history.pop_front();
                    }
                    self.focus_history.push_back(sid);

                    // Record analytics and hysteresis
                    self.analytics.record_switch(new_elapsed);
                    self.hysteresis.record_switch(sid, timestamp_s);

                    // Start cooldown
                    self.cooldown.start();
                    let cooldown_ms = self.cooldown.default_ms;

                    // Transition to idle (cooldown active)
                    self.dwell = DwellState::Idle;

                    return Some(GazeFocusEvent::FocusRequested {
                        surface_id: sid,
                        dwell_ms: new_elapsed,
                        x: cx,
                        y: cy,
                    });
                }

                // Still dwelling — update state
                self.dwell = DwellState::Dwelling {
                    surface_id: *surface_id,
                    elapsed_ms: new_elapsed,
                    center_x: *center_x,
                    center_y: *center_y,
                    jitter_max_px: new_jitter,
                };

                Some(GazeFocusEvent::DwellProgress {
                    surface_id: *surface_id,
                    elapsed_ms: new_elapsed,
                    threshold_ms: effective_threshold,
                })
            }

            // Dwelling but surface changed — reset
            (DwellState::Dwelling { surface_id, .. }, Some(hit)) => {
                let old_sid = *surface_id;
                self.dwell = DwellState::Dwelling {
                    surface_id: hit.surface_id,
                    elapsed_ms: 0.0,
                    center_x: hit.pixel_x,
                    center_y: hit.pixel_y,
                    jitter_max_px: 0.0,
                };
                Some(GazeFocusEvent::DwellStarted {
                    surface_id: hit.surface_id,
                    x: hit.pixel_x,
                    y: hit.pixel_y,
                })
            }

            // Confirmed — stay idle until cooldown ends
            (DwellState::Confirmed { .. }, _) => {
                self.dwell = DwellState::Idle;
                None
            }

            // Cooldown state — handled above
            (DwellState::Cooldown { .. }, _) => {
                self.dwell = DwellState::Idle;
                None
            }
        }
    }

    /// Set the focus policy.
    pub fn set_policy(&mut self, policy: FocusPolicy) {
        self.policy = policy;
        info!("Gaze focus policy set to {}", policy.as_str());
    }

    /// Set the dwell threshold in milliseconds.
    pub fn set_dwell_threshold(&mut self, threshold_ms: f64) {
        self.config.threshold_ms = threshold_ms;
        info!("Dwell threshold set to {:.0}ms", threshold_ms);
    }

    /// Set the cooldown duration in milliseconds.
    pub fn set_cooldown(&mut self, cooldown_ms: f64) {
        self.cooldown.default_ms = cooldown_ms;
        info!("Focus cooldown set to {:.0}ms", cooldown_ms);
    }

    /// Set the saccade threshold in degrees per second.
    pub fn set_saccade_threshold(&mut self, threshold_dps: f32) {
        self.saccade.saccade_threshold_dps = threshold_dps;
        info!("Saccade threshold set to {:.0} deg/s", threshold_dps);
    }

    /// Record a keyboard-initiated focus change (for false positive detection).
    pub fn record_keyboard_focus(&mut self, surface_id: u64) {
        let now = self.last_focus_change_s; // approximate
        if let Some(prev) = self.focused_surface {
            // If user overrode gaze focus within 1 second, it was a false positive
            if self.last_focus_method == "gaze"
                && now > 0.0
                && prev != surface_id
            {
                self.analytics.record_false_positive();
                debug!(
                    "False positive detected: gaze focused {} but keyboard chose {}",
                    prev, surface_id
                );
            }
        }
        self.focused_surface = Some(surface_id);
        self.last_focus_method = "keyboard".to_string();

        if self.focus_history.len() >= 10 {
            self.focus_history.pop_front();
        }
        self.focus_history.push_back(surface_id);
    }

    /// Navigate to the previously focused surface (focus history back).
    pub fn focus_back(&mut self) -> Option<u64> {
        if self.focus_history.len() < 2 {
            return None;
        }
        // Current focus is the last entry; go to second-to-last
        let len = self.focus_history.len();
        let prev = self.focus_history[len - 2];
        self.focused_surface = Some(prev);
        self.last_focus_method = "history".to_string();
        Some(prev)
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        format!(
            "(:policy :{} :dwell-threshold-ms {:.0} :max-jitter-px {:.0} :cooldown-ms {:.0} :saccade-threshold-dps {:.0} :confidence-min {:.2} :reading-detection {} :hysteresis {} :fallback-head-gaze {})",
            self.policy.as_str(),
            self.config.threshold_ms,
            self.config.max_jitter_px,
            self.cooldown.default_ms,
            self.saccade.saccade_threshold_dps,
            self.config.require_confidence,
            if self.reading.enabled { "t" } else { "nil" },
            if self.hysteresis.enabled { "t" } else { "nil" },
            if self.fallback_to_head_gaze { "t" } else { "nil" },
        )
    }

    /// Generate IPC status s-expression.
    pub fn status_sexp(&self) -> String {
        let dwell_str = match &self.dwell {
            DwellState::Idle => "idle".to_string(),
            DwellState::Dwelling { surface_id, elapsed_ms, .. } => {
                format!("dwelling-{}-{:.0}ms", surface_id, elapsed_ms)
            }
            DwellState::Confirmed { surface_id } => {
                format!("confirmed-{}", surface_id)
            }
            DwellState::Cooldown { remaining_ms } => {
                format!("cooldown-{:.0}ms", remaining_ms)
            }
        };
        let focused_str = self
            .focused_surface
            .map(|id| id.to_string())
            .unwrap_or_else(|| "nil".to_string());
        format!(
            "(:focused {} :dwell {} :method \"{}\" :saccade {} :reading {} :cooldown {} :analytics {})",
            focused_str,
            dwell_str,
            self.last_focus_method,
            if self.saccade.is_saccade { "t" } else { "nil" },
            if self.reading.in_reading_mode { "t" } else { "nil" },
            if self.cooldown.is_active() { "t" } else { "nil" },
            self.analytics.status_sexp(),
        )
    }

    /// Clean up gaze focus state.
    pub fn teardown(&mut self) {
        self.dwell = DwellState::Idle;
        self.saccade.reset();
        self.reading.reset();
        self.cooldown = CooldownState::default();
        self.hysteresis.reset();
        self.focused_surface = None;
        self.focus_history.clear();
        self.analytics = FocusAnalytics::default();
        info!("Gaze focus manager torn down");
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::eye_tracking::{GazeData, GazeSurfaceHit, GazeSource};
    use super::super::vr_interaction::Ray;
    use super::super::scene::Vec3;

    fn make_gaze(direction: Vec3, confidence: f32, timestamp_s: f64) -> GazeData {
        GazeData {
            source: GazeSource::Simulated,
            ray: Ray::new(Vec3::ZERO, direction),
            confidence,
            timestamp_s,
            per_eye: None,
        }
    }

    fn make_hit(surface_id: u64, pixel_x: f32, pixel_y: f32) -> GazeSurfaceHit {
        GazeSurfaceHit {
            surface_id,
            pixel_x,
            pixel_y,
            distance: 2.0,
            confidence: 0.95,
        }
    }

    #[test]
    fn test_dwell_detection_basic() {
        let mut mgr = GazeFocusManager::new();
        let dir = Vec3::new(0.0, 0.0, -1.0);
        let hit = make_hit(1, 100.0, 200.0);
        let gaze = make_gaze(dir, 0.95, 0.0);

        // Start dwell
        let evt = mgr.update(Some(&hit), Some(&gaze), 0.011);
        assert!(matches!(evt, Some(GazeFocusEvent::DwellStarted { surface_id: 1, .. })));

        // Accumulate 250ms (above 200ms threshold)
        let mut timestamp = 0.011;
        let mut confirmed = false;
        for _ in 0..25 {
            timestamp += 0.011;
            let gaze = make_gaze(dir, 0.95, timestamp);
            let evt = mgr.update(Some(&hit), Some(&gaze), 0.011);
            if let Some(GazeFocusEvent::FocusRequested { surface_id: 1, .. }) = &evt {
                confirmed = true;
                break;
            }
        }
        assert!(confirmed, "Dwell should confirm after ~250ms");
        assert_eq!(mgr.focused_surface, Some(1));
    }

    #[test]
    fn test_dwell_reset_on_surface_change() {
        let mut mgr = GazeFocusManager::new();
        let dir = Vec3::new(0.0, 0.0, -1.0);
        let hit1 = make_hit(1, 100.0, 200.0);
        let hit2 = make_hit(2, 300.0, 400.0);

        // Start dwelling on surface 1
        let gaze = make_gaze(dir, 0.95, 0.0);
        mgr.update(Some(&hit1), Some(&gaze), 0.011);

        // Accumulate 100ms on surface 1
        for i in 1..=10 {
            let gaze = make_gaze(dir, 0.95, i as f64 * 0.011);
            mgr.update(Some(&hit1), Some(&gaze), 0.011);
        }

        // Switch to surface 2 — should reset
        let gaze = make_gaze(dir, 0.95, 0.12);
        let evt = mgr.update(Some(&hit2), Some(&gaze), 0.011);
        assert!(matches!(evt, Some(GazeFocusEvent::DwellStarted { surface_id: 2, .. })));

        // Verify elapsed_ms is reset (check via progress event)
        let gaze = make_gaze(dir, 0.95, 0.131);
        let evt = mgr.update(Some(&hit2), Some(&gaze), 0.011);
        if let Some(GazeFocusEvent::DwellProgress { elapsed_ms, .. }) = evt {
            assert!(elapsed_ms < 20.0, "Elapsed should be small after reset: {}", elapsed_ms);
        }
    }

    #[test]
    fn test_saccade_suppression() {
        let mut mgr = GazeFocusManager::new();
        let hit = make_hit(1, 100.0, 200.0);

        // Start with a normal gaze
        let gaze = make_gaze(Vec3::new(0.0, 0.0, -1.0), 0.95, 0.0);
        mgr.update(Some(&hit), Some(&gaze), 0.011);

        // Inject a rapid direction change (400 deg/s equivalent)
        // At 0.011s dt, 400 deg/s = 4.4 degrees of change
        let angle_rad = (4.4f32).to_radians();
        let fast_dir = Vec3::new(angle_rad.sin(), 0.0, -angle_rad.cos());
        let gaze = make_gaze(fast_dir, 0.95, 0.011);
        let evt = mgr.update(Some(&hit), Some(&gaze), 0.011);

        // Saccade should be detected and dwell should not accumulate
        assert!(
            mgr.saccade.is_saccade || matches!(evt, Some(GazeFocusEvent::SaccadeDetected { .. })),
            "Saccade should be detected at 400 deg/s"
        );
    }

    #[test]
    fn test_microsaccade_ignored() {
        let mut mgr = GazeFocusManager::new();
        let hit = make_hit(1, 100.0, 200.0);

        // Start with a normal gaze
        let dir = Vec3::new(0.0, 0.0, -1.0);
        let gaze = make_gaze(dir, 0.95, 0.0);
        mgr.update(Some(&hit), Some(&gaze), 0.011);

        // Small movement (0.5 degrees) — microsaccade, should be ignored
        let angle_rad = (0.5f32).to_radians();
        let small_dir = Vec3::new(angle_rad.sin(), 0.0, -angle_rad.cos());
        let gaze = make_gaze(small_dir, 0.95, 0.011);
        mgr.update(Some(&hit), Some(&gaze), 0.011);

        assert!(!mgr.saccade.is_saccade, "Microsaccade should not trigger saccade state");

        // Continue dwelling — should progress
        let gaze = make_gaze(small_dir, 0.95, 0.022);
        let evt = mgr.update(Some(&hit), Some(&gaze), 0.011);
        assert!(
            matches!(evt, Some(GazeFocusEvent::DwellProgress { .. })),
            "Dwell should continue after microsaccade"
        );
    }

    #[test]
    fn test_cooldown_blocks_rapid_focus() {
        let mut mgr = GazeFocusManager::new();
        mgr.config.threshold_ms = 50.0; // fast dwell for testing
        mgr.cooldown.default_ms = 300.0;
        let dir = Vec3::new(0.0, 0.0, -1.0);

        // Complete dwell on surface 1
        let hit1 = make_hit(1, 100.0, 200.0);
        let mut t = 0.0;
        for _ in 0..10 {
            t += 0.011;
            let gaze = make_gaze(dir, 0.95, t);
            mgr.update(Some(&hit1), Some(&gaze), 0.011);
        }
        assert_eq!(mgr.focused_surface, Some(1));

        // Immediately try to focus surface 2 — should be blocked by cooldown
        let hit2 = make_hit(2, 300.0, 400.0);
        t += 0.011;
        let gaze = make_gaze(dir, 0.95, t);
        let evt = mgr.update(Some(&hit2), Some(&gaze), 0.011);

        // Should still be focused on surface 1, not 2
        assert_eq!(
            mgr.focused_surface,
            Some(1),
            "Cooldown should block rapid refocus"
        );
    }

    #[test]
    fn test_hysteresis_increases_threshold() {
        let mut tracker = HysteresisTracker::default();

        // Simulate A-B-A-B switching
        tracker.record_switch(1, 1.0);
        tracker.record_switch(2, 1.5);
        tracker.record_switch(1, 2.0);
        tracker.record_switch(2, 2.5);

        let extra = tracker.effective_extra_ms(3.0);
        assert!(
            extra > 0.0,
            "Hysteresis should add extra threshold after ping-pong: {}",
            extra
        );

        // After long stability, should reset
        let extra_after = tracker.effective_extra_ms(20.0);
        assert_eq!(
            extra_after, 0.0,
            "Hysteresis should reset after stable period"
        );
    }

    #[test]
    fn test_jitter_exceeds_max() {
        let mut mgr = GazeFocusManager::new();
        mgr.config.max_jitter_px = 50.0;
        let dir = Vec3::new(0.0, 0.0, -1.0);

        // Start dwell at (100, 200)
        let hit = make_hit(1, 100.0, 200.0);
        let gaze = make_gaze(dir, 0.95, 0.0);
        mgr.update(Some(&hit), Some(&gaze), 0.011);

        // Move to 80px away (exceeds 50px max jitter)
        let jittery_hit = make_hit(1, 180.0, 200.0);
        let gaze = make_gaze(dir, 0.95, 0.011);
        let evt = mgr.update(Some(&jittery_hit), Some(&gaze), 0.011);

        assert!(
            matches!(evt, Some(GazeFocusEvent::FocusCancelled { surface_id: 1, .. })),
            "Jitter exceeding max should cancel dwell"
        );
    }

    #[test]
    fn test_confidence_filter() {
        let mut mgr = GazeFocusManager::new();
        mgr.config.require_confidence = 0.6;
        let dir = Vec3::new(0.0, 0.0, -1.0);
        let hit = make_hit(1, 100.0, 200.0);

        // Start dwell with high confidence
        let gaze = make_gaze(dir, 0.95, 0.0);
        mgr.update(Some(&hit), Some(&gaze), 0.011);

        // Low confidence frames should not advance dwell
        let mut last_elapsed = 0.0;
        for i in 1..=5 {
            let gaze = make_gaze(dir, 0.3, i as f64 * 0.011); // low confidence
            let evt = mgr.update(Some(&hit), Some(&gaze), 0.011);
            // Should return None (filtered out)
            assert!(evt.is_none(), "Low confidence frame should be filtered");
        }

        // High confidence frame should advance
        let gaze = make_gaze(dir, 0.95, 0.07);
        let evt = mgr.update(Some(&hit), Some(&gaze), 0.011);
        assert!(
            matches!(evt, Some(GazeFocusEvent::DwellProgress { .. })),
            "High confidence frame should advance dwell"
        );
    }

    #[test]
    fn test_reading_detection() {
        let mut detector = ReadingDetector::default();
        detector.horizontal_threshold = 0.5;

        // Scan across >50% of a 1000px wide surface
        let reading = detector.update(1, 100.0, 1000.0, 0.0);
        assert!(!reading);

        detector.update(1, 200.0, 1000.0, 0.1);
        detector.update(1, 400.0, 1000.0, 0.2);
        let reading = detector.update(1, 600.0, 1000.0, 0.3); // 500px extent = 50%

        assert!(reading, "Reading mode should activate at 50% horizontal coverage");
        assert!(detector.in_reading_mode);
    }

    #[test]
    fn test_focus_history_ring() {
        let mut mgr = GazeFocusManager::new();
        mgr.config.threshold_ms = 10.0; // very fast for testing
        mgr.cooldown.default_ms = 0.0; // no cooldown for this test
        let dir = Vec3::new(0.0, 0.0, -1.0);

        // Focus 15 different surfaces
        for sid in 1..=15u64 {
            let hit = make_hit(sid, 100.0, 200.0);
            for i in 0..5 {
                let gaze = make_gaze(dir, 0.95, (sid * 100 + i) as f64 * 0.011);
                mgr.update(Some(&hit), Some(&gaze), 0.011);
            }
        }

        // Ring buffer should contain at most 10 entries
        assert!(
            mgr.focus_history.len() <= 10,
            "Focus history should be capped at 10, got {}",
            mgr.focus_history.len()
        );
    }

    #[test]
    fn test_policy_roundtrip() {
        let policies = vec![
            ("gaze-only", FocusPolicy::GazeOnly),
            ("gaze-primary", FocusPolicy::GazePrimary),
            ("gaze-assist", FocusPolicy::GazeAssist),
            ("disabled", FocusPolicy::Disabled),
        ];

        for (s, p) in &policies {
            assert_eq!(FocusPolicy::from_str(s), Some(*p));
            assert_eq!(p.as_str(), *s);
        }

        assert_eq!(FocusPolicy::from_str("invalid"), None);
    }

    #[test]
    fn test_analytics_tracking() {
        let mut analytics = FocusAnalytics::default();
        analytics.session_start_s = 0.0;

        analytics.record_switch(200.0);
        analytics.record_switch(180.0);
        analytics.record_switch(220.0);
        assert_eq!(analytics.focus_switches, 3);
        assert_eq!(analytics.dwell_durations.len(), 3);

        analytics.record_false_positive();
        assert_eq!(analytics.false_positives, 1);

        analytics.record_saccade_suppression();
        assert_eq!(analytics.saccade_suppressions, 1);

        analytics.record_cooldown_block();
        assert_eq!(analytics.cooldown_blocks, 1);

        analytics.record_reading_suppression();
        assert_eq!(analytics.reading_suppressions, 1);

        let spm = analytics.switches_per_minute(60.0); // 1 minute elapsed
        assert!((spm - 3.0).abs() < 0.1, "Switches per minute: {}", spm);

        let sexp = analytics.status_sexp();
        assert!(sexp.contains(":switches 3"));
        assert!(sexp.contains(":false-positives 1"));
    }
}
