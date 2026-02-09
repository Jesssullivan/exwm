//! Blink detection and wink discrimination — classify single-eye closures
//! as intentional winks vs. natural blinks, with calibration support.
//!
//! Provides BlinkDetector for raw blink event tracking, WinkClassifier for
//! distinguishing left/right winks from natural blinks, and WinkCalibration
//! for per-user threshold tuning.  No openxrs dependency.

use std::collections::VecDeque;
use tracing::{debug, info, warn};

// ── Blink types ─────────────────────────────────────────────

/// Which eye(s) were involved in a blink.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlinkEye {
    Left,
    Right,
    Both,
}

impl BlinkEye {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Left => "left",
            Self::Right => "right",
            Self::Both => "both",
        }
    }
}

/// A single detected blink event.
#[derive(Debug, Clone)]
pub struct BlinkEvent {
    /// Which eye(s) blinked.
    pub eye: BlinkEye,
    /// Duration of the closure in milliseconds.
    pub duration_ms: f64,
    /// Detection confidence (0.0–1.0).
    pub confidence: f32,
    /// Timestamp when the blink ended (seconds).
    pub timestamp_s: f64,
}

// ── Blink detector ──────────────────────────────────────────

/// Tracks per-eye open/close transitions and emits BlinkEvents.
#[derive(Debug)]
pub struct BlinkDetector {
    /// Ring buffer of the most recent blinks (max 50).
    pub history: VecDeque<BlinkEvent>,
    /// Minimum confidence to accept a blink.
    pub confidence_threshold: f32,
    /// Whether the left eye is currently closed.
    pub left_closed: bool,
    /// Whether the right eye is currently closed.
    pub right_closed: bool,
    /// Timestamp when the left eye closed (seconds), or None if open.
    pub left_close_time: Option<f64>,
    /// Timestamp when the right eye closed (seconds), or None if open.
    pub right_close_time: Option<f64>,
    /// Validity-based fallback: left eye was invalid in previous frame.
    pub left_was_invalid: bool,
    /// Validity-based fallback: right eye was invalid in previous frame.
    pub right_was_invalid: bool,
    /// Timestamp when left validity was lost (for fallback detection).
    pub left_invalid_since: Option<f64>,
    /// Timestamp when right validity was lost (for fallback detection).
    pub right_invalid_since: Option<f64>,
    /// Minimum invalid duration to consider a blink onset (milliseconds).
    pub validity_blink_onset_ms: f64,
}

impl Default for BlinkDetector {
    fn default() -> Self {
        Self {
            history: VecDeque::with_capacity(50),
            confidence_threshold: 0.7,
            left_closed: false,
            right_closed: false,
            left_close_time: None,
            right_close_time: None,
            left_was_invalid: false,
            right_was_invalid: false,
            left_invalid_since: None,
            right_invalid_since: None,
            validity_blink_onset_ms: 80.0,
        }
    }
}

impl BlinkDetector {
    /// Update with per-eye open/close state.  Returns blink events for any
    /// closures that just ended (transition from closed to open).
    pub fn update_eye_state(
        &mut self,
        left_open: bool,
        right_open: bool,
        timestamp_s: f64,
    ) -> Vec<BlinkEvent> {
        let mut events = Vec::new();

        // ── Left eye transitions ──
        if !left_open && !self.left_closed {
            // Left eye just closed
            self.left_closed = true;
            self.left_close_time = Some(timestamp_s);
            debug!("Left eye closed at {:.3}s", timestamp_s);
        } else if left_open && self.left_closed {
            // Left eye just opened — emit blink
            self.left_closed = false;
            if let Some(close_time) = self.left_close_time.take() {
                let duration_ms = (timestamp_s - close_time) * 1000.0;
                let evt = BlinkEvent {
                    eye: BlinkEye::Left,
                    duration_ms,
                    confidence: 1.0,
                    timestamp_s,
                };
                debug!("Left blink: {:.0}ms", duration_ms);
                self.push_event(evt.clone());
                events.push(evt);
            }
        }

        // ── Right eye transitions ──
        if !right_open && !self.right_closed {
            // Right eye just closed
            self.right_closed = true;
            self.right_close_time = Some(timestamp_s);
            debug!("Right eye closed at {:.3}s", timestamp_s);
        } else if right_open && self.right_closed {
            // Right eye just opened — emit blink
            self.right_closed = false;
            if let Some(close_time) = self.right_close_time.take() {
                let duration_ms = (timestamp_s - close_time) * 1000.0;
                let evt = BlinkEvent {
                    eye: BlinkEye::Right,
                    duration_ms,
                    confidence: 1.0,
                    timestamp_s,
                };
                debug!("Right blink: {:.0}ms", duration_ms);
                self.push_event(evt.clone());
                events.push(evt);
            }
        }

        events
    }

    /// OpenXR fallback: update with per-eye validity flags.
    /// When isValid goes false for longer than `validity_blink_onset_ms`,
    /// treat it as a blink onset.  When validity returns, emit the blink.
    pub fn update_validity(
        &mut self,
        left_valid: bool,
        right_valid: bool,
        timestamp_s: f64,
    ) -> Vec<BlinkEvent> {
        let mut events = Vec::new();

        // ── Left validity tracking ──
        if !left_valid && !self.left_was_invalid {
            self.left_was_invalid = true;
            self.left_invalid_since = Some(timestamp_s);
        } else if left_valid && self.left_was_invalid {
            self.left_was_invalid = false;
            if let Some(since) = self.left_invalid_since.take() {
                let duration_ms = (timestamp_s - since) * 1000.0;
                if duration_ms >= self.validity_blink_onset_ms {
                    let evt = BlinkEvent {
                        eye: BlinkEye::Left,
                        duration_ms,
                        confidence: 0.8,
                        timestamp_s,
                    };
                    debug!("Left blink (validity fallback): {:.0}ms", duration_ms);
                    self.push_event(evt.clone());
                    events.push(evt);
                }
            }
        }

        // ── Right validity tracking ──
        if !right_valid && !self.right_was_invalid {
            self.right_was_invalid = true;
            self.right_invalid_since = Some(timestamp_s);
        } else if right_valid && self.right_was_invalid {
            self.right_was_invalid = false;
            if let Some(since) = self.right_invalid_since.take() {
                let duration_ms = (timestamp_s - since) * 1000.0;
                if duration_ms >= self.validity_blink_onset_ms {
                    let evt = BlinkEvent {
                        eye: BlinkEye::Right,
                        duration_ms,
                        confidence: 0.8,
                        timestamp_s,
                    };
                    debug!("Right blink (validity fallback): {:.0}ms", duration_ms);
                    self.push_event(evt.clone());
                    events.push(evt);
                }
            }
        }

        events
    }

    /// Compute blinks per minute over a rolling window of `window_s` seconds.
    /// Counts only blinks whose timestamp falls within `[now - window_s, now]`
    /// where `now` is the timestamp of the most recent blink.
    pub fn blink_rate(&self, window_s: f64) -> f64 {
        if self.history.is_empty() || window_s <= 0.0 {
            return 0.0;
        }
        let now = self.history.back().unwrap().timestamp_s;
        let cutoff = now - window_s;
        let count = self
            .history
            .iter()
            .filter(|b| b.timestamp_s >= cutoff)
            .count();
        (count as f64 / window_s) * 60.0
    }

    /// Return the last N blinks (or all if fewer than N are available).
    pub fn recent_blinks(&self, n: usize) -> Vec<&BlinkEvent> {
        let len = self.history.len();
        let start = if len > n { len - n } else { 0 };
        self.history.iter().skip(start).collect()
    }

    /// Push a blink event into the ring buffer, evicting the oldest if full.
    fn push_event(&mut self, event: BlinkEvent) {
        if self.history.len() >= 50 {
            self.history.pop_front();
        }
        self.history.push_back(event);
    }
}

// ── Wink state machine ──────────────────────────────────────

/// Internal state for wink classification.
#[derive(Debug, Clone, PartialEq)]
pub enum WinkState {
    /// No active closure being tracked.
    Idle,
    /// Left eye is closing; right eye remains open.
    LeftEyeClosing { since_s: f64 },
    /// Right eye is closing; left eye remains open.
    RightEyeClosing { since_s: f64 },
    /// Both eyes are closing (probable natural blink).
    BothEyesClosing { since_s: f64 },
    /// Left wink confirmed (terminal — resets to Idle next frame).
    LeftWinkConfirmed,
    /// Right wink confirmed (terminal — resets to Idle next frame).
    RightWinkConfirmed,
    /// Natural blink detected (terminal — resets to Idle next frame).
    NaturalBlink,
}

// ── Wink config ─────────────────────────────────────────────

/// Timing thresholds for wink vs. blink classification.
#[derive(Debug, Clone)]
pub struct WinkConfig {
    /// Minimum single-eye closure to qualify as a wink (ms).
    pub min_duration_ms: f64,
    /// Maximum single-eye closure to qualify as a wink (ms).
    pub max_duration_ms: f64,
    /// Maximum time offset between left and right closure to count as
    /// "both eyes closing simultaneously" (ms).
    pub both_eye_tolerance_ms: f64,
    /// Closures shorter than this are noise (ms).
    pub noise_floor_ms: f64,
    /// Two winks of the same eye within this window → double-wink (ms).
    pub double_wink_window_ms: f64,
}

impl Default for WinkConfig {
    fn default() -> Self {
        Self {
            min_duration_ms: 300.0,
            max_duration_ms: 600.0,
            both_eye_tolerance_ms: 50.0,
            noise_floor_ms: 100.0,
            double_wink_window_ms: 800.0,
        }
    }
}

// ── Wink event ──────────────────────────────────────────────

/// Events produced by the wink classifier.
#[derive(Debug, Clone, PartialEq)]
pub enum WinkEvent {
    LeftWink {
        duration_ms: f64,
        timestamp_s: f64,
    },
    RightWink {
        duration_ms: f64,
        timestamp_s: f64,
    },
    DoubleLeftWink {
        timestamp_s: f64,
    },
    DoubleRightWink {
        timestamp_s: f64,
    },
    NaturalBlink {
        duration_ms: f64,
        timestamp_s: f64,
    },
    ExtendedClosure {
        eye: BlinkEye,
        duration_ms: f64,
        timestamp_s: f64,
    },
}

impl WinkEvent {
    /// Serialize the event as an IPC s-expression.
    pub fn to_sexp(&self) -> String {
        match self {
            Self::LeftWink {
                duration_ms,
                timestamp_s,
            } => {
                format!(
                    "(:type :event :event :left-wink :duration-ms {:.0} :timestamp {:.3})",
                    duration_ms, timestamp_s
                )
            }
            Self::RightWink {
                duration_ms,
                timestamp_s,
            } => {
                format!(
                    "(:type :event :event :right-wink :duration-ms {:.0} :timestamp {:.3})",
                    duration_ms, timestamp_s
                )
            }
            Self::DoubleLeftWink { timestamp_s } => {
                format!(
                    "(:type :event :event :double-left-wink :timestamp {:.3})",
                    timestamp_s
                )
            }
            Self::DoubleRightWink { timestamp_s } => {
                format!(
                    "(:type :event :event :double-right-wink :timestamp {:.3})",
                    timestamp_s
                )
            }
            Self::NaturalBlink {
                duration_ms,
                timestamp_s,
            } => {
                format!(
                    "(:type :event :event :natural-blink :duration-ms {:.0} :timestamp {:.3})",
                    duration_ms, timestamp_s
                )
            }
            Self::ExtendedClosure {
                eye,
                duration_ms,
                timestamp_s,
            } => {
                format!(
                    "(:type :event :event :extended-closure :eye :{} :duration-ms {:.0} :timestamp {:.3})",
                    eye.as_str(),
                    duration_ms,
                    timestamp_s
                )
            }
        }
    }
}

// ── Wink classifier ─────────────────────────────────────────

/// State machine that classifies per-eye closures as winks, blinks, or noise.
#[derive(Debug)]
pub struct WinkClassifier {
    /// Current state of the classifier.
    pub state: WinkState,
    /// Timing thresholds.
    pub config: WinkConfig,
    /// Eye of the last confirmed wink (for double-wink detection).
    pub last_wink_eye: Option<BlinkEye>,
    /// Timestamp of the last confirmed wink (for double-wink detection).
    pub last_wink_time: Option<f64>,
    /// Tracking: is the left eye currently closed?
    left_closed: bool,
    /// Tracking: is the right eye currently closed?
    right_closed: bool,
    /// Timestamp when the left eye closed.
    left_close_time: Option<f64>,
    /// Timestamp when the right eye closed.
    right_close_time: Option<f64>,
}

impl WinkClassifier {
    pub fn new(config: WinkConfig) -> Self {
        Self {
            state: WinkState::Idle,
            config,
            last_wink_eye: None,
            last_wink_time: None,
            left_closed: false,
            right_closed: false,
            left_close_time: None,
            right_close_time: None,
        }
    }

    /// Core classification pipeline.  Call once per frame with per-eye state.
    /// Returns a WinkEvent when a classification is made.
    pub fn update(
        &mut self,
        left_open: bool,
        right_open: bool,
        timestamp_s: f64,
    ) -> Option<WinkEvent> {
        let left_closed_now = !left_open;
        let right_closed_now = !right_open;

        // ── Detect close transitions ──
        if left_closed_now && !self.left_closed {
            self.left_closed = true;
            self.left_close_time = Some(timestamp_s);
        }
        if right_closed_now && !self.right_closed {
            self.right_closed = true;
            self.right_close_time = Some(timestamp_s);
        }

        // ── Check for both-eyes-closing (natural blink detection) ──
        // If both eyes are closed and their close times are within tolerance,
        // transition to BothEyesClosing.
        if self.left_closed && self.right_closed {
            if let (Some(lt), Some(rt)) = (self.left_close_time, self.right_close_time) {
                let delta_ms = (lt - rt).abs() * 1000.0;
                if delta_ms <= self.config.both_eye_tolerance_ms {
                    let since = lt.min(rt);
                    if !matches!(self.state, WinkState::BothEyesClosing { .. }) {
                        self.state = WinkState::BothEyesClosing { since_s: since };
                    }
                }
            }
        }

        // ── Update state based on single-eye closure ──
        match self.state {
            WinkState::Idle => {
                if self.left_closed && !self.right_closed {
                    if let Some(t) = self.left_close_time {
                        self.state = WinkState::LeftEyeClosing { since_s: t };
                    }
                } else if self.right_closed && !self.left_closed {
                    if let Some(t) = self.right_close_time {
                        self.state = WinkState::RightEyeClosing { since_s: t };
                    }
                } else if self.left_closed && self.right_closed {
                    // Both closed from idle — natural blink
                    let since = self
                        .left_close_time
                        .unwrap_or(timestamp_s)
                        .min(self.right_close_time.unwrap_or(timestamp_s));
                    self.state = WinkState::BothEyesClosing { since_s: since };
                }
                None
            }

            WinkState::LeftEyeClosing { since_s } => {
                // If right eye also closed within tolerance → natural blink
                // (handled above by BothEyesClosing transition)

                // If left eye opened — classify the closure
                if left_open {
                    self.left_closed = false;
                    self.left_close_time = None;
                    let duration_ms = (timestamp_s - since_s) * 1000.0;
                    self.state = WinkState::Idle;
                    return self.classify_single_eye(
                        BlinkEye::Left,
                        duration_ms,
                        timestamp_s,
                    );
                }
                None
            }

            WinkState::RightEyeClosing { since_s } => {
                if right_open {
                    self.right_closed = false;
                    self.right_close_time = None;
                    let duration_ms = (timestamp_s - since_s) * 1000.0;
                    self.state = WinkState::Idle;
                    return self.classify_single_eye(
                        BlinkEye::Right,
                        duration_ms,
                        timestamp_s,
                    );
                }
                None
            }

            WinkState::BothEyesClosing { since_s } => {
                // Both eyes open again — natural blink
                if left_open && right_open {
                    self.left_closed = false;
                    self.right_closed = false;
                    self.left_close_time = None;
                    self.right_close_time = None;
                    let duration_ms = (timestamp_s - since_s) * 1000.0;
                    self.state = WinkState::Idle;
                    debug!("Natural blink: {:.0}ms", duration_ms);
                    return Some(WinkEvent::NaturalBlink {
                        duration_ms,
                        timestamp_s,
                    });
                }
                None
            }

            WinkState::LeftWinkConfirmed
            | WinkState::RightWinkConfirmed
            | WinkState::NaturalBlink => {
                // Terminal states — reset on next call
                self.state = WinkState::Idle;
                None
            }
        }
    }

    /// Classify a single-eye closure by its duration.
    fn classify_single_eye(
        &mut self,
        eye: BlinkEye,
        duration_ms: f64,
        timestamp_s: f64,
    ) -> Option<WinkEvent> {
        // Noise: too short
        if duration_ms < self.config.noise_floor_ms {
            debug!("Noise rejected: {:?} eye {:.0}ms", eye, duration_ms);
            return None;
        }

        // Extended closure: too long
        if duration_ms > self.config.max_duration_ms {
            warn!(
                "Extended closure: {:?} eye {:.0}ms (max {:.0}ms)",
                eye, duration_ms, self.config.max_duration_ms
            );
            return Some(WinkEvent::ExtendedClosure {
                eye,
                duration_ms,
                timestamp_s,
            });
        }

        // Valid wink range
        if duration_ms >= self.config.min_duration_ms {
            // Check for double-wink
            if let (Some(last_eye), Some(last_time)) =
                (self.last_wink_eye, self.last_wink_time)
            {
                let gap_ms = (timestamp_s - last_time) * 1000.0;
                if last_eye == eye && gap_ms <= self.config.double_wink_window_ms {
                    self.last_wink_eye = None;
                    self.last_wink_time = None;
                    let evt = match eye {
                        BlinkEye::Left => {
                            info!("Double left wink at {:.3}s", timestamp_s);
                            WinkEvent::DoubleLeftWink { timestamp_s }
                        }
                        BlinkEye::Right => {
                            info!("Double right wink at {:.3}s", timestamp_s);
                            WinkEvent::DoubleRightWink { timestamp_s }
                        }
                        BlinkEye::Both => {
                            // Should not happen for single-eye classification
                            return None;
                        }
                    };
                    return Some(evt);
                }
            }

            // Record this wink for future double-wink detection
            self.last_wink_eye = Some(eye);
            self.last_wink_time = Some(timestamp_s);

            match eye {
                BlinkEye::Left => {
                    self.state = WinkState::LeftWinkConfirmed;
                    info!("Left wink: {:.0}ms at {:.3}s", duration_ms, timestamp_s);
                    Some(WinkEvent::LeftWink {
                        duration_ms,
                        timestamp_s,
                    })
                }
                BlinkEye::Right => {
                    self.state = WinkState::RightWinkConfirmed;
                    info!("Right wink: {:.0}ms at {:.3}s", duration_ms, timestamp_s);
                    Some(WinkEvent::RightWink {
                        duration_ms,
                        timestamp_s,
                    })
                }
                BlinkEye::Both => None,
            }
        } else {
            // Between noise_floor and min_duration — ambiguous, ignore
            debug!(
                "Ambiguous closure: {:?} eye {:.0}ms (noise floor {:.0}ms, min wink {:.0}ms)",
                eye, duration_ms, self.config.noise_floor_ms, self.config.min_duration_ms
            );
            None
        }
    }

    /// Reset classifier state.
    pub fn reset(&mut self) {
        self.state = WinkState::Idle;
        self.left_closed = false;
        self.right_closed = false;
        self.left_close_time = None;
        self.right_close_time = None;
        self.last_wink_eye = None;
        self.last_wink_time = None;
    }
}

// ── Calibration trial type ──────────────────────────────────

/// Type of calibration trial.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrialType {
    LeftWink,
    RightWink,
    NaturalBlink,
}

// ── Wink calibration ────────────────────────────────────────

/// Per-user calibration data for wink thresholds.
#[derive(Debug, Clone)]
pub struct WinkCalibration {
    /// Recorded left wink durations (ms).
    pub left_durations: Vec<f64>,
    /// Recorded right wink durations (ms).
    pub right_durations: Vec<f64>,
    /// Recorded natural blink durations (ms).
    pub blink_durations: Vec<f64>,
    /// Whether calibration has been computed successfully.
    pub calibrated: bool,
    /// Computed left wink minimum threshold (ms).
    pub left_min_ms: f64,
    /// Computed left wink maximum threshold (ms).
    pub left_max_ms: f64,
    /// Computed right wink minimum threshold (ms).
    pub right_min_ms: f64,
    /// Computed right wink maximum threshold (ms).
    pub right_max_ms: f64,
}

impl Default for WinkCalibration {
    fn default() -> Self {
        Self {
            left_durations: Vec::new(),
            right_durations: Vec::new(),
            blink_durations: Vec::new(),
            calibrated: false,
            left_min_ms: 300.0,
            left_max_ms: 600.0,
            right_min_ms: 300.0,
            right_max_ms: 600.0,
        }
    }
}

impl WinkCalibration {
    /// Record a calibration trial of the given type.
    pub fn record_trial(&mut self, trial_type: TrialType, duration_ms: f64) {
        match trial_type {
            TrialType::LeftWink => self.left_durations.push(duration_ms),
            TrialType::RightWink => self.right_durations.push(duration_ms),
            TrialType::NaturalBlink => self.blink_durations.push(duration_ms),
        }
        debug!(
            "Calibration trial: {:?} {:.0}ms (total: L={} R={} B={})",
            trial_type,
            duration_ms,
            self.left_durations.len(),
            self.right_durations.len(),
            self.blink_durations.len(),
        );
    }

    /// Compute per-eye thresholds from calibration data using mean +/- 2*stddev.
    /// Returns true if thresholds were computed successfully without overlap
    /// between blink and wink ranges.
    pub fn compute_thresholds(&mut self) -> bool {
        if self.left_durations.len() < 3
            || self.right_durations.len() < 3
            || self.blink_durations.len() < 3
        {
            warn!(
                "Calibration requires at least 3 trials per type (L={} R={} B={})",
                self.left_durations.len(),
                self.right_durations.len(),
                self.blink_durations.len(),
            );
            self.calibrated = false;
            return false;
        }

        let (left_mean, left_std) = mean_stddev(&self.left_durations);
        let (right_mean, right_std) = mean_stddev(&self.right_durations);
        let (blink_mean, blink_std) = mean_stddev(&self.blink_durations);

        self.left_min_ms = (left_mean - 2.0 * left_std).max(0.0);
        self.left_max_ms = left_mean + 2.0 * left_std;
        self.right_min_ms = (right_mean - 2.0 * right_std).max(0.0);
        self.right_max_ms = right_mean + 2.0 * right_std;

        // Check for overlap: blink range (mean +/- 2*std) should not overlap wink ranges
        let blink_max = blink_mean + 2.0 * blink_std;
        let overlap_left = blink_max > self.left_min_ms && blink_mean < self.left_max_ms;
        let overlap_right = blink_max > self.right_min_ms && blink_mean < self.right_max_ms;

        if overlap_left || overlap_right {
            warn!(
                "Calibration overlap detected: blink [{:.0}–{:.0}] vs left [{:.0}–{:.0}] right [{:.0}–{:.0}]",
                blink_mean - 2.0 * blink_std,
                blink_max,
                self.left_min_ms,
                self.left_max_ms,
                self.right_min_ms,
                self.right_max_ms,
            );
            self.calibrated = false;
            return false;
        }

        self.calibrated = true;
        info!(
            "Calibration computed: left [{:.0}–{:.0}ms] right [{:.0}–{:.0}ms] blink mean {:.0}ms",
            self.left_min_ms,
            self.left_max_ms,
            self.right_min_ms,
            self.right_max_ms,
            blink_mean,
        );
        true
    }

    /// Estimate left and right wink accuracy based on separation from blink distribution.
    /// Returns (left_accuracy, right_accuracy) as values in [0.0, 1.0].
    pub fn accuracy_estimate(&self) -> (f64, f64) {
        if self.left_durations.len() < 3
            || self.right_durations.len() < 3
            || self.blink_durations.len() < 3
        {
            return (0.0, 0.0);
        }

        let (left_mean, left_std) = mean_stddev(&self.left_durations);
        let (right_mean, right_std) = mean_stddev(&self.right_durations);
        let (blink_mean, blink_std) = mean_stddev(&self.blink_durations);

        // Accuracy based on d-prime-like separation
        let combined_left_std = (left_std * left_std + blink_std * blink_std).sqrt();
        let left_acc = if combined_left_std > 0.0 {
            let separation = (left_mean - blink_mean).abs() / combined_left_std;
            (separation / 4.0).min(1.0) // normalize: 4 stddev separation = perfect
        } else {
            0.0
        };

        let combined_right_std = (right_std * right_std + blink_std * blink_std).sqrt();
        let right_acc = if combined_right_std > 0.0 {
            let separation = (right_mean - blink_mean).abs() / combined_right_std;
            (separation / 4.0).min(1.0)
        } else {
            0.0
        };

        (left_acc, right_acc)
    }

    /// Generate IPC s-expression for calibration state.
    pub fn calibration_sexp(&self) -> String {
        let (left_acc, right_acc) = self.accuracy_estimate();
        format!(
            "(:calibrated {} :left-min-ms {:.0} :left-max-ms {:.0} :right-min-ms {:.0} :right-max-ms {:.0} :left-trials {} :right-trials {} :blink-trials {} :left-accuracy {:.2} :right-accuracy {:.2})",
            if self.calibrated { "t" } else { "nil" },
            self.left_min_ms,
            self.left_max_ms,
            self.right_min_ms,
            self.right_max_ms,
            self.left_durations.len(),
            self.right_durations.len(),
            self.blink_durations.len(),
            left_acc,
            right_acc,
        )
    }

    /// Reset all calibration data.
    pub fn reset(&mut self) {
        self.left_durations.clear();
        self.right_durations.clear();
        self.blink_durations.clear();
        self.calibrated = false;
        self.left_min_ms = 300.0;
        self.left_max_ms = 600.0;
        self.right_min_ms = 300.0;
        self.right_max_ms = 600.0;
    }
}

/// Compute mean and population standard deviation for a slice of f64.
fn mean_stddev(values: &[f64]) -> (f64, f64) {
    if values.is_empty() {
        return (0.0, 0.0);
    }
    let n = values.len() as f64;
    let mean = values.iter().sum::<f64>() / n;
    let variance = values.iter().map(|v| (v - mean) * (v - mean)).sum::<f64>() / n;
    (mean, variance.sqrt())
}

// ── Top-level manager ───────────────────────────────────────

/// Combines blink detection, wink classification, and calibration into
/// a single update pipeline.
pub struct BlinkWinkManager {
    /// Raw blink event detector.
    pub blink_detector: BlinkDetector,
    /// Wink vs. blink classifier.
    pub wink_classifier: WinkClassifier,
    /// Per-user calibration data.
    pub calibration: WinkCalibration,
    /// Whether the blink/wink subsystem is enabled.
    pub enabled: bool,
}

impl BlinkWinkManager {
    pub fn new() -> Self {
        info!("Blink/wink manager initialized");
        Self {
            blink_detector: BlinkDetector::default(),
            wink_classifier: WinkClassifier::new(WinkConfig::default()),
            calibration: WinkCalibration::default(),
            enabled: true,
        }
    }

    /// Update with per-eye open/close state.  Feeds both the blink detector
    /// and wink classifier, returning any wink events produced.
    pub fn update(
        &mut self,
        left_open: bool,
        right_open: bool,
        timestamp_s: f64,
    ) -> Vec<WinkEvent> {
        if !self.enabled {
            return Vec::new();
        }

        // Feed blink detector for rate tracking
        self.blink_detector
            .update_eye_state(left_open, right_open, timestamp_s);

        // Feed wink classifier for event classification
        let mut events = Vec::new();
        if let Some(evt) = self.wink_classifier.update(left_open, right_open, timestamp_s) {
            events.push(evt);
        }

        events
    }

    /// Generate IPC status s-expression.
    pub fn status_sexp(&self) -> String {
        let state_str = match &self.wink_classifier.state {
            WinkState::Idle => "idle".to_string(),
            WinkState::LeftEyeClosing { since_s } => {
                format!("left-closing-{:.3}", since_s)
            }
            WinkState::RightEyeClosing { since_s } => {
                format!("right-closing-{:.3}", since_s)
            }
            WinkState::BothEyesClosing { since_s } => {
                format!("both-closing-{:.3}", since_s)
            }
            WinkState::LeftWinkConfirmed => "left-wink-confirmed".to_string(),
            WinkState::RightWinkConfirmed => "right-wink-confirmed".to_string(),
            WinkState::NaturalBlink => "natural-blink".to_string(),
        };
        let blink_rate = self.blink_detector.blink_rate(60.0);
        format!(
            "(:enabled {} :state \"{}\" :blink-rate {:.1} :blinks {} :calibrated {})",
            if self.enabled { "t" } else { "nil" },
            state_str,
            blink_rate,
            self.blink_detector.history.len(),
            if self.calibration.calibrated { "t" } else { "nil" },
        )
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        let c = &self.wink_classifier.config;
        format!(
            "(:enabled {} :min-duration-ms {:.0} :max-duration-ms {:.0} :both-eye-tolerance-ms {:.0} :noise-floor-ms {:.0} :double-wink-window-ms {:.0} :confidence-threshold {:.2})",
            if self.enabled { "t" } else { "nil" },
            c.min_duration_ms,
            c.max_duration_ms,
            c.both_eye_tolerance_ms,
            c.noise_floor_ms,
            c.double_wink_window_ms,
            self.blink_detector.confidence_threshold,
        )
    }

    /// Tear down blink/wink state.
    pub fn teardown(&mut self) {
        self.blink_detector = BlinkDetector::default();
        self.wink_classifier.reset();
        self.calibration.reset();
        self.enabled = false;
        info!("Blink/wink manager torn down");
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_natural_blink_ignored() {
        // Both eyes close within 50ms → NaturalBlink (not a wink)
        let mut classifier = WinkClassifier::new(WinkConfig::default());

        // Both eyes close at t=1.0
        let evt = classifier.update(false, false, 1.0);
        assert!(evt.is_none());

        // Both eyes open at t=1.2 (200ms later)
        let evt = classifier.update(true, true, 1.2);
        assert!(
            matches!(evt, Some(WinkEvent::NaturalBlink { .. })),
            "Both eyes closing simultaneously should produce NaturalBlink, got {:?}",
            evt
        );
    }

    #[test]
    fn test_left_wink_detection() {
        // Only left eye closes for 400ms → LeftWink
        let mut classifier = WinkClassifier::new(WinkConfig::default());

        // Left eye closes, right stays open at t=1.0
        let evt = classifier.update(false, true, 1.0);
        assert!(evt.is_none());

        // Left eye opens at t=1.4 (400ms later), right still open
        let evt = classifier.update(true, true, 1.4);
        assert!(
            matches!(
                evt,
                Some(WinkEvent::LeftWink { duration_ms, .. }) if (duration_ms - 400.0).abs() < 1.0
            ),
            "Left eye closure for 400ms should produce LeftWink, got {:?}",
            evt
        );
    }

    #[test]
    fn test_right_wink_detection() {
        // Only right eye closes for 400ms → RightWink
        let mut classifier = WinkClassifier::new(WinkConfig::default());

        // Right eye closes, left stays open at t=2.0
        let evt = classifier.update(true, false, 2.0);
        assert!(evt.is_none());

        // Right eye opens at t=2.4 (400ms later), left still open
        let evt = classifier.update(true, true, 2.4);
        assert!(
            matches!(
                evt,
                Some(WinkEvent::RightWink { duration_ms, .. }) if (duration_ms - 400.0).abs() < 1.0
            ),
            "Right eye closure for 400ms should produce RightWink, got {:?}",
            evt
        );
    }

    #[test]
    fn test_noise_rejected() {
        // Single eye closure < 100ms → no event
        let mut classifier = WinkClassifier::new(WinkConfig::default());

        // Left eye closes at t=1.0
        let evt = classifier.update(false, true, 1.0);
        assert!(evt.is_none());

        // Left eye opens at t=1.05 (50ms later) — below noise floor
        let evt = classifier.update(true, true, 1.05);
        assert!(
            evt.is_none(),
            "Closure under noise floor should produce no event, got {:?}",
            evt
        );
    }

    #[test]
    fn test_extended_closure_warning() {
        // Single eye > 600ms → ExtendedClosure
        let mut classifier = WinkClassifier::new(WinkConfig::default());

        // Left eye closes at t=1.0
        let evt = classifier.update(false, true, 1.0);
        assert!(evt.is_none());

        // Left eye opens at t=1.8 (800ms later) — extended
        let evt = classifier.update(true, true, 1.8);
        assert!(
            matches!(
                evt,
                Some(WinkEvent::ExtendedClosure { eye: BlinkEye::Left, duration_ms, .. })
                    if (duration_ms - 800.0).abs() < 1.0
            ),
            "Closure over max duration should produce ExtendedClosure, got {:?}",
            evt
        );
    }

    #[test]
    fn test_double_wink_detection() {
        // Two left winks within 800ms → DoubleLeftWink
        let mut classifier = WinkClassifier::new(WinkConfig::default());

        // First left wink: close at t=1.0, open at t=1.4 (400ms)
        classifier.update(false, true, 1.0);
        let evt = classifier.update(true, true, 1.4);
        assert!(
            matches!(evt, Some(WinkEvent::LeftWink { .. })),
            "First wink should be LeftWink, got {:?}",
            evt
        );

        // Second left wink: close at t=1.5, open at t=1.9 (400ms)
        // Gap from first wink end (1.4) to second wink end (1.9) = 500ms < 800ms
        classifier.update(false, true, 1.5);
        let evt = classifier.update(true, true, 1.9);
        assert!(
            matches!(evt, Some(WinkEvent::DoubleLeftWink { .. })),
            "Two left winks within 800ms should produce DoubleLeftWink, got {:?}",
            evt
        );
    }

    #[test]
    fn test_blink_rate_computation() {
        let mut detector = BlinkDetector::default();

        // Add 6 blinks over 30 seconds → 12 blinks/min
        for i in 0..6 {
            let t = i as f64 * 5.0;
            detector.push_event(BlinkEvent {
                eye: BlinkEye::Both,
                duration_ms: 150.0,
                confidence: 1.0,
                timestamp_s: t,
            });
        }

        // Rate over 30-second window: 6 blinks / 0.5 min = 12 bpm
        let rate = detector.blink_rate(30.0);
        assert!(
            (rate - 12.0).abs() < 0.5,
            "Expected ~12 blinks/min over 30s window, got {:.1}",
            rate
        );

        // Rate over 10-second window: only blinks at t=20 and t=25 are within [15, 25]
        let rate_10 = detector.blink_rate(10.0);
        assert!(
            rate_10 > 0.0,
            "Should have some blinks in the 10s window, got {:.1}",
            rate_10
        );
    }

    #[test]
    fn test_wink_config_defaults() {
        let config = WinkConfig::default();
        assert!((config.min_duration_ms - 300.0).abs() < f64::EPSILON);
        assert!((config.max_duration_ms - 600.0).abs() < f64::EPSILON);
        assert!((config.both_eye_tolerance_ms - 50.0).abs() < f64::EPSILON);
        assert!((config.noise_floor_ms - 100.0).abs() < f64::EPSILON);
        assert!((config.double_wink_window_ms - 800.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_calibration_compute() {
        let mut cal = WinkCalibration::default();

        // Add left wink trials (mean ~400, stddev ~20)
        for &d in &[380.0, 400.0, 420.0, 390.0, 410.0] {
            cal.record_trial(TrialType::LeftWink, d);
        }

        // Add right wink trials (mean ~450, stddev ~25)
        for &d in &[425.0, 450.0, 475.0, 440.0, 460.0] {
            cal.record_trial(TrialType::RightWink, d);
        }

        // Add natural blink trials (mean ~150, stddev ~20)
        for &d in &[130.0, 150.0, 170.0, 140.0, 160.0] {
            cal.record_trial(TrialType::NaturalBlink, d);
        }

        let success = cal.compute_thresholds();
        assert!(success, "Calibration should succeed with well-separated data");
        assert!(cal.calibrated);

        // Left range should be around 400 +/- 2*~14 ≈ [372, 428]
        assert!(cal.left_min_ms > 300.0 && cal.left_min_ms < 400.0);
        assert!(cal.left_max_ms > 400.0 && cal.left_max_ms < 500.0);

        // Right range should be around 450 +/- 2*~18 ≈ [414, 486]
        assert!(cal.right_min_ms > 350.0 && cal.right_min_ms < 450.0);
        assert!(cal.right_max_ms > 450.0 && cal.right_max_ms < 550.0);

        // Accuracy should be high for well-separated distributions
        let (left_acc, right_acc) = cal.accuracy_estimate();
        assert!(
            left_acc > 0.5,
            "Left accuracy should be high, got {:.2}",
            left_acc
        );
        assert!(
            right_acc > 0.5,
            "Right accuracy should be high, got {:.2}",
            right_acc
        );
    }

    #[test]
    fn test_wink_event_sexp() {
        let events = vec![
            WinkEvent::LeftWink {
                duration_ms: 400.0,
                timestamp_s: 1.234,
            },
            WinkEvent::RightWink {
                duration_ms: 350.0,
                timestamp_s: 2.567,
            },
            WinkEvent::DoubleLeftWink {
                timestamp_s: 3.000,
            },
            WinkEvent::DoubleRightWink {
                timestamp_s: 4.000,
            },
            WinkEvent::NaturalBlink {
                duration_ms: 150.0,
                timestamp_s: 5.000,
            },
            WinkEvent::ExtendedClosure {
                eye: BlinkEye::Left,
                duration_ms: 900.0,
                timestamp_s: 6.000,
            },
        ];

        let sexps: Vec<String> = events.iter().map(|e| e.to_sexp()).collect();

        assert!(sexps[0].contains(":left-wink"));
        assert!(sexps[0].contains(":duration-ms 400"));
        assert!(sexps[0].contains(":timestamp 1.234"));

        assert!(sexps[1].contains(":right-wink"));
        assert!(sexps[1].contains(":duration-ms 350"));

        assert!(sexps[2].contains(":double-left-wink"));
        assert!(sexps[2].contains(":timestamp 3.000"));

        assert!(sexps[3].contains(":double-right-wink"));

        assert!(sexps[4].contains(":natural-blink"));
        assert!(sexps[4].contains(":duration-ms 150"));

        assert!(sexps[5].contains(":extended-closure"));
        assert!(sexps[5].contains(":eye :left"));
        assert!(sexps[5].contains(":duration-ms 900"));
    }
}
