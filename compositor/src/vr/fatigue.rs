//! Eye fatigue monitoring — blink rate, PERCLOS, saccade jitter, and
//! session duration tracking for VR eye strain prevention.
//!
//! Uses rolling windows over blink timestamps, eye closure samples, and
//! gaze velocities to classify fatigue level and emit alerts.
//! No openxrs dependency; compiles unconditionally.

use std::collections::VecDeque;
use tracing::{debug, info, warn};

// ── FatigueLevel ────────────────────────────────────────────

/// Classification of eye fatigue severity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FatigueLevel {
    /// Normal operation (< 20 blinks/min).
    Normal,
    /// Mild fatigue (20-25 blinks/min).
    Mild,
    /// Significant fatigue (> 25 blinks/min).
    Significant,
    /// Critical fatigue (PERCLOS > 15% OR > 30 blinks/min).
    Critical,
}

impl FatigueLevel {
    /// String representation for IPC and logging.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Normal => "normal",
            Self::Mild => "mild",
            Self::Significant => "significant",
            Self::Critical => "critical",
        }
    }

    /// Parse a fatigue level from its string representation.
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "normal" => Some(Self::Normal),
            "mild" => Some(Self::Mild),
            "significant" => Some(Self::Significant),
            "critical" => Some(Self::Critical),
            _ => None,
        }
    }
}

// ── FatigueMetrics ──────────────────────────────────────────

/// Current computed fatigue metrics from rolling windows.
#[derive(Debug, Clone)]
pub struct FatigueMetrics {
    /// Blinks per minute computed over a rolling window.
    pub blink_rate: f64,
    /// Standard deviation of gaze velocity (degrees/second).
    pub saccade_jitter: f64,
    /// Percentage of eye closure (0.0-1.0) over the rolling window.
    pub perclos: f64,
    /// Minutes since session start.
    pub session_duration_min: f64,
}

impl Default for FatigueMetrics {
    fn default() -> Self {
        Self {
            blink_rate: 0.0,
            saccade_jitter: 0.0,
            perclos: 0.0,
            session_duration_min: 0.0,
        }
    }
}

// ── FatigueConfig ───────────────────────────────────────────

/// Configuration for fatigue detection thresholds.
#[derive(Debug, Clone)]
pub struct FatigueConfig {
    /// Blink rate threshold for mild fatigue (blinks/min).
    pub mild_threshold: f64,
    /// Blink rate threshold for significant fatigue (blinks/min).
    pub significant_threshold: f64,
    /// Blink rate threshold for critical fatigue (blinks/min).
    pub critical_threshold: f64,
    /// PERCLOS threshold for critical fatigue (0.0-1.0).
    pub perclos_threshold: f64,
    /// Gaze velocity jitter threshold (deg/s).
    pub jitter_threshold: f64,
    /// Rolling window duration (seconds).
    pub window_s: f64,
    /// Minimum seconds between fatigue checks.
    pub check_interval_s: f64,
    /// Whether fatigue monitoring is enabled.
    pub enabled: bool,
    /// Whether to log fatigue events.
    pub log_enabled: bool,
}

impl Default for FatigueConfig {
    fn default() -> Self {
        Self {
            mild_threshold: 20.0,
            significant_threshold: 25.0,
            critical_threshold: 30.0,
            perclos_threshold: 0.15,
            jitter_threshold: 2.0,
            window_s: 60.0,
            check_interval_s: 60.0,
            enabled: true,
            log_enabled: true,
        }
    }
}

// ── FatigueEvent ────────────────────────────────────────────

/// Events emitted by the fatigue monitoring pipeline.
#[derive(Debug, Clone)]
pub enum FatigueEvent {
    /// Fatigue level changed (general transition).
    LevelChanged {
        from: FatigueLevel,
        to: FatigueLevel,
        metrics: FatigueMetrics,
    },
    /// Mild fatigue alert.
    AlertMild {
        blink_rate: f64,
        session_min: f64,
    },
    /// Significant fatigue alert.
    AlertSignificant {
        blink_rate: f64,
        perclos: f64,
        session_min: f64,
    },
    /// Critical fatigue alert.
    AlertCritical {
        blink_rate: f64,
        perclos: f64,
        jitter: f64,
        session_min: f64,
    },
}

impl FatigueEvent {
    /// Convert the event to an IPC s-expression.
    pub fn to_sexp(&self) -> String {
        match self {
            Self::LevelChanged { from, to, metrics } => {
                format!(
                    "(:type :event :event :fatigue-level-changed :from :{} :to :{} :blink-rate {:.1} :perclos {:.3} :jitter {:.2} :session-min {:.1})",
                    from.as_str(),
                    to.as_str(),
                    metrics.blink_rate,
                    metrics.perclos,
                    metrics.saccade_jitter,
                    metrics.session_duration_min,
                )
            }
            Self::AlertMild {
                blink_rate,
                session_min,
            } => {
                format!(
                    "(:type :event :event :fatigue-alert-mild :blink-rate {:.1} :session-min {:.1})",
                    blink_rate, session_min,
                )
            }
            Self::AlertSignificant {
                blink_rate,
                perclos,
                session_min,
            } => {
                format!(
                    "(:type :event :event :fatigue-alert-significant :blink-rate {:.1} :perclos {:.3} :session-min {:.1})",
                    blink_rate, perclos, session_min,
                )
            }
            Self::AlertCritical {
                blink_rate,
                perclos,
                jitter,
                session_min,
            } => {
                format!(
                    "(:type :event :event :fatigue-alert-critical :blink-rate {:.1} :perclos {:.3} :jitter {:.2} :session-min {:.1})",
                    blink_rate, perclos, jitter, session_min,
                )
            }
        }
    }
}

// ── FatigueMonitor ──────────────────────────────────────────

/// Central fatigue monitoring system with rolling-window metrics.
pub struct FatigueMonitor {
    /// Fatigue detection configuration.
    pub config: FatigueConfig,
    /// Current computed fatigue level.
    pub current_level: FatigueLevel,
    /// Current computed metrics.
    pub metrics: FatigueMetrics,
    /// Timestamps of recent blinks (seconds).
    pub blink_timestamps: VecDeque<f64>,
    /// Eye closure samples: (timestamp_s, is_closed).
    pub eye_closure_samples: VecDeque<(f64, bool)>,
    /// Recent gaze velocities for jitter computation (deg/s).
    pub gaze_velocities: VecDeque<f64>,
    /// Session start timestamp (seconds).
    pub session_start_s: f64,
    /// Timestamp of the last fatigue check (seconds).
    pub last_check_s: f64,
    /// The fatigue level at the time of the last alert.
    pub last_alert_level: FatigueLevel,
    /// Whether fatigue monitoring is enabled.
    pub enabled: bool,
}

impl FatigueMonitor {
    pub fn new() -> Self {
        info!("Fatigue monitor initialized");
        Self {
            config: FatigueConfig::default(),
            current_level: FatigueLevel::Normal,
            metrics: FatigueMetrics::default(),
            blink_timestamps: VecDeque::with_capacity(256),
            eye_closure_samples: VecDeque::with_capacity(1024),
            gaze_velocities: VecDeque::with_capacity(512),
            session_start_s: 0.0,
            last_check_s: 0.0,
            last_alert_level: FatigueLevel::Normal,
            enabled: true,
        }
    }

    /// Record a blink event at the given timestamp.
    pub fn record_blink(&mut self, timestamp_s: f64) {
        self.blink_timestamps.push_back(timestamp_s);
        self.prune_blinks(timestamp_s);
        debug!("Blink recorded at {:.2}s, {} in window", timestamp_s, self.blink_timestamps.len());
    }

    /// Record an eye open/closed state sample.
    pub fn record_eye_state(&mut self, timestamp_s: f64, is_closed: bool) {
        self.eye_closure_samples.push_back((timestamp_s, is_closed));
        self.prune_eye_closure(timestamp_s);
    }

    /// Record a gaze velocity sample for jitter computation.
    pub fn record_gaze_velocity(&mut self, velocity_dps: f32) {
        self.gaze_velocities.push_back(velocity_dps as f64);
        // Keep a bounded buffer
        while self.gaze_velocities.len() > 512 {
            self.gaze_velocities.pop_front();
        }
    }

    /// Recompute metrics and classify fatigue level.
    ///
    /// Returns a `FatigueEvent` if the fatigue level changed or an alert
    /// should be emitted.
    pub fn update(&mut self, timestamp_s: f64) -> Option<FatigueEvent> {
        if !self.enabled || !self.config.enabled {
            return None;
        }

        // Initialize session start on first update
        if self.session_start_s == 0.0 {
            self.session_start_s = timestamp_s;
            self.last_check_s = timestamp_s;
        }

        // Rate-limit checks
        if timestamp_s - self.last_check_s < self.config.check_interval_s {
            return None;
        }
        self.last_check_s = timestamp_s;

        // Prune old data
        self.prune_blinks(timestamp_s);
        self.prune_eye_closure(timestamp_s);

        // Compute blink rate (blinks per minute)
        let window_s = self.config.window_s;
        let blink_count = self.blink_timestamps.len() as f64;
        let blink_rate = if window_s > 0.0 {
            blink_count * (60.0 / window_s)
        } else {
            0.0
        };

        // Compute PERCLOS
        let perclos = self.compute_perclos();

        // Compute saccade jitter (stddev of gaze velocities)
        let jitter = self.compute_jitter();

        // Session duration
        let session_min = (timestamp_s - self.session_start_s) / 60.0;

        // Update metrics
        self.metrics = FatigueMetrics {
            blink_rate,
            saccade_jitter: jitter,
            perclos,
            session_duration_min: session_min,
        };

        // Classify level
        let new_level = self.classify_level(blink_rate, perclos);
        let prev_level = self.current_level;
        self.current_level = new_level;

        // Emit event if level changed
        if new_level != prev_level {
            if self.config.log_enabled {
                match new_level {
                    FatigueLevel::Normal => {
                        info!("Fatigue level: normal (blink rate {:.1}/min)", blink_rate);
                    }
                    FatigueLevel::Mild => {
                        warn!("Fatigue level: mild (blink rate {:.1}/min)", blink_rate);
                    }
                    FatigueLevel::Significant => {
                        warn!(
                            "Fatigue level: significant (blink rate {:.1}/min, PERCLOS {:.1}%)",
                            blink_rate,
                            perclos * 100.0,
                        );
                    }
                    FatigueLevel::Critical => {
                        warn!(
                            "Fatigue level: CRITICAL (blink rate {:.1}/min, PERCLOS {:.1}%, jitter {:.2} deg/s)",
                            blink_rate,
                            perclos * 100.0,
                            jitter,
                        );
                    }
                }
            }

            // Emit specific alert based on new level
            let alert_event = match new_level {
                FatigueLevel::Mild => Some(FatigueEvent::AlertMild {
                    blink_rate,
                    session_min,
                }),
                FatigueLevel::Significant => Some(FatigueEvent::AlertSignificant {
                    blink_rate,
                    perclos,
                    session_min,
                }),
                FatigueLevel::Critical => Some(FatigueEvent::AlertCritical {
                    blink_rate,
                    perclos,
                    jitter,
                    session_min,
                }),
                FatigueLevel::Normal => None,
            };

            self.last_alert_level = new_level;

            // Return the level change event (alerts are secondary)
            // For simplicity, emit the LevelChanged event; callers can inspect level.
            return Some(FatigueEvent::LevelChanged {
                from: prev_level,
                to: new_level,
                metrics: self.metrics.clone(),
            });
        }

        None
    }

    /// Generate IPC status s-expression.
    pub fn status_sexp(&self) -> String {
        format!(
            "(:enabled {} :level :{} :blink-rate {:.1} :perclos {:.3} :jitter {:.2} :session-min {:.1})",
            if self.enabled { "t" } else { "nil" },
            self.current_level.as_str(),
            self.metrics.blink_rate,
            self.metrics.perclos,
            self.metrics.saccade_jitter,
            self.metrics.session_duration_min,
        )
    }

    /// Generate IPC config s-expression.
    pub fn config_sexp(&self) -> String {
        format!(
            "(:mild-threshold {:.0} :significant-threshold {:.0} :critical-threshold {:.0} :perclos-threshold {:.2} :jitter-threshold {:.1} :window-s {:.0} :check-interval-s {:.0} :enabled {} :log-enabled {})",
            self.config.mild_threshold,
            self.config.significant_threshold,
            self.config.critical_threshold,
            self.config.perclos_threshold,
            self.config.jitter_threshold,
            self.config.window_s,
            self.config.check_interval_s,
            if self.config.enabled { "t" } else { "nil" },
            if self.config.log_enabled { "t" } else { "nil" },
        )
    }

    /// Generate IPC metrics s-expression.
    pub fn metrics_sexp(&self) -> String {
        format!(
            "(:blink-rate {:.1} :perclos {:.3} :jitter {:.2} :session-min {:.1} :blinks-in-window {} :closure-samples {} :velocity-samples {})",
            self.metrics.blink_rate,
            self.metrics.perclos,
            self.metrics.saccade_jitter,
            self.metrics.session_duration_min,
            self.blink_timestamps.len(),
            self.eye_closure_samples.len(),
            self.gaze_velocities.len(),
        )
    }

    /// Clean up fatigue monitor state.
    pub fn teardown(&mut self) {
        self.current_level = FatigueLevel::Normal;
        self.metrics = FatigueMetrics::default();
        self.blink_timestamps.clear();
        self.eye_closure_samples.clear();
        self.gaze_velocities.clear();
        self.session_start_s = 0.0;
        self.last_check_s = 0.0;
        self.last_alert_level = FatigueLevel::Normal;
        info!("Fatigue monitor torn down");
    }

    // ── Internal helpers ────────────────────────────────────

    /// Remove blink timestamps outside the rolling window.
    fn prune_blinks(&mut self, now_s: f64) {
        let cutoff = now_s - self.config.window_s;
        while let Some(&ts) = self.blink_timestamps.front() {
            if ts < cutoff {
                self.blink_timestamps.pop_front();
            } else {
                break;
            }
        }
    }

    /// Remove eye closure samples outside the rolling window.
    fn prune_eye_closure(&mut self, now_s: f64) {
        let cutoff = now_s - self.config.window_s;
        while let Some(&(ts, _)) = self.eye_closure_samples.front() {
            if ts < cutoff {
                self.eye_closure_samples.pop_front();
            } else {
                break;
            }
        }
    }

    /// Compute PERCLOS (percentage of eye closure) from recent samples.
    fn compute_perclos(&self) -> f64 {
        if self.eye_closure_samples.is_empty() {
            return 0.0;
        }
        let closed_count = self
            .eye_closure_samples
            .iter()
            .filter(|(_, closed)| *closed)
            .count() as f64;
        closed_count / self.eye_closure_samples.len() as f64
    }

    /// Compute standard deviation of gaze velocities (saccade jitter).
    fn compute_jitter(&self) -> f64 {
        if self.gaze_velocities.len() < 2 {
            return 0.0;
        }
        let n = self.gaze_velocities.len() as f64;
        let mean = self.gaze_velocities.iter().sum::<f64>() / n;
        let variance = self
            .gaze_velocities
            .iter()
            .map(|v| {
                let diff = v - mean;
                diff * diff
            })
            .sum::<f64>()
            / (n - 1.0);
        variance.sqrt()
    }

    /// Classify fatigue level from blink rate and PERCLOS.
    fn classify_level(&self, blink_rate: f64, perclos: f64) -> FatigueLevel {
        // Critical: PERCLOS above threshold OR blink rate above critical
        if perclos > self.config.perclos_threshold
            || blink_rate > self.config.critical_threshold
        {
            return FatigueLevel::Critical;
        }
        // Significant: blink rate above significant threshold
        if blink_rate > self.config.significant_threshold {
            return FatigueLevel::Significant;
        }
        // Mild: blink rate above mild threshold
        if blink_rate >= self.config.mild_threshold {
            return FatigueLevel::Mild;
        }
        FatigueLevel::Normal
    }
}

// ── Tests ───────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_blink_rate_computation() {
        let mut monitor = FatigueMonitor::new();
        monitor.config.window_s = 60.0;
        monitor.config.check_interval_s = 0.0; // no rate limiting in tests
        monitor.session_start_s = 0.0;

        // Add 15 blinks over 60 seconds — should be 15 blinks/min
        for i in 0..15 {
            monitor.record_blink(i as f64 * 4.0); // every 4 seconds
        }

        // Trigger update at t=60s
        monitor.update(60.0);

        assert!(
            (monitor.metrics.blink_rate - 15.0).abs() < 0.5,
            "Blink rate should be ~15/min, got {:.1}",
            monitor.metrics.blink_rate,
        );
    }

    #[test]
    fn test_fatigue_level_classification() {
        let monitor = FatigueMonitor::new();

        // Normal: < 20 blinks/min, low PERCLOS
        assert_eq!(monitor.classify_level(10.0, 0.05), FatigueLevel::Normal);
        assert_eq!(monitor.classify_level(19.9, 0.10), FatigueLevel::Normal);

        // Mild: 20-25 blinks/min
        assert_eq!(monitor.classify_level(20.0, 0.10), FatigueLevel::Mild);
        assert_eq!(monitor.classify_level(24.0, 0.10), FatigueLevel::Mild);

        // Significant: > 25 blinks/min (but < 30)
        assert_eq!(monitor.classify_level(26.0, 0.10), FatigueLevel::Significant);
        assert_eq!(monitor.classify_level(29.0, 0.10), FatigueLevel::Significant);

        // Critical: > 30 blinks/min
        assert_eq!(monitor.classify_level(31.0, 0.10), FatigueLevel::Critical);

        // Critical: PERCLOS > 15% (even with normal blink rate)
        assert_eq!(monitor.classify_level(10.0, 0.20), FatigueLevel::Critical);
    }

    #[test]
    fn test_perclos_computation() {
        let mut monitor = FatigueMonitor::new();

        // Simulate 100 samples: 20 closed, 80 open → PERCLOS = 0.20
        for i in 0..100 {
            let is_closed = i < 20;
            monitor.record_eye_state(i as f64 * 0.5, is_closed);
        }

        let perclos = monitor.compute_perclos();
        assert!(
            (perclos - 0.20).abs() < 0.01,
            "PERCLOS should be ~0.20, got {:.3}",
            perclos,
        );

        // Simulate all open
        monitor.eye_closure_samples.clear();
        for i in 0..50 {
            monitor.record_eye_state(i as f64 * 0.5, false);
        }
        let perclos = monitor.compute_perclos();
        assert!(
            perclos < 0.01,
            "PERCLOS should be ~0.0 when eyes always open, got {:.3}",
            perclos,
        );
    }

    #[test]
    fn test_fatigue_event_sexp() {
        let evt = FatigueEvent::LevelChanged {
            from: FatigueLevel::Normal,
            to: FatigueLevel::Mild,
            metrics: FatigueMetrics {
                blink_rate: 22.0,
                saccade_jitter: 1.5,
                perclos: 0.08,
                session_duration_min: 45.0,
            },
        };
        let sexp = evt.to_sexp();
        assert!(sexp.contains(":event :fatigue-level-changed"));
        assert!(sexp.contains(":from :normal"));
        assert!(sexp.contains(":to :mild"));
        assert!(sexp.contains(":blink-rate 22.0"));

        let evt = FatigueEvent::AlertCritical {
            blink_rate: 35.0,
            perclos: 0.18,
            jitter: 3.5,
            session_min: 120.0,
        };
        let sexp = evt.to_sexp();
        assert!(sexp.contains(":event :fatigue-alert-critical"));
        assert!(sexp.contains(":blink-rate 35.0"));
        assert!(sexp.contains(":perclos 0.180"));
        assert!(sexp.contains(":jitter 3.50"));

        let evt = FatigueEvent::AlertMild {
            blink_rate: 21.0,
            session_min: 30.0,
        };
        let sexp = evt.to_sexp();
        assert!(sexp.contains(":event :fatigue-alert-mild"));
        assert!(sexp.contains(":session-min 30.0"));
    }

    #[test]
    fn test_fatigue_config_defaults() {
        let config = FatigueConfig::default();
        assert!((config.mild_threshold - 20.0).abs() < f64::EPSILON);
        assert!((config.significant_threshold - 25.0).abs() < f64::EPSILON);
        assert!((config.critical_threshold - 30.0).abs() < f64::EPSILON);
        assert!((config.perclos_threshold - 0.15).abs() < f64::EPSILON);
        assert!((config.jitter_threshold - 2.0).abs() < f64::EPSILON);
        assert!((config.window_s - 60.0).abs() < f64::EPSILON);
        assert!((config.check_interval_s - 60.0).abs() < f64::EPSILON);
        assert!(config.enabled);
        assert!(config.log_enabled);
    }

    #[test]
    fn test_fatigue_level_roundtrip() {
        let levels = vec![
            ("normal", FatigueLevel::Normal),
            ("mild", FatigueLevel::Mild),
            ("significant", FatigueLevel::Significant),
            ("critical", FatigueLevel::Critical),
        ];
        for (s, l) in &levels {
            assert_eq!(FatigueLevel::from_str(s), Some(*l));
            assert_eq!(l.as_str(), *s);
        }
        assert_eq!(FatigueLevel::from_str("unknown"), None);
    }

    #[test]
    fn test_jitter_computation() {
        let mut monitor = FatigueMonitor::new();

        // All same velocity → jitter should be 0
        for _ in 0..10 {
            monitor.record_gaze_velocity(5.0);
        }
        let jitter = monitor.compute_jitter();
        assert!(
            jitter < 0.01,
            "Jitter should be ~0.0 for constant velocity, got {:.4}",
            jitter,
        );

        // Alternating velocities → non-zero jitter
        monitor.gaze_velocities.clear();
        for i in 0..20 {
            let v = if i % 2 == 0 { 1.0 } else { 3.0 };
            monitor.record_gaze_velocity(v);
        }
        let jitter = monitor.compute_jitter();
        assert!(
            jitter > 0.5,
            "Jitter should be >0.5 for alternating 1/3 deg/s, got {:.4}",
            jitter,
        );
    }

    #[test]
    fn test_status_and_config_sexp() {
        let monitor = FatigueMonitor::new();
        let status = monitor.status_sexp();
        assert!(status.contains(":enabled t"));
        assert!(status.contains(":level :normal"));

        let config = monitor.config_sexp();
        assert!(config.contains(":mild-threshold 20"));
        assert!(config.contains(":perclos-threshold 0.15"));

        let metrics = monitor.metrics_sexp();
        assert!(metrics.contains(":blink-rate"));
        assert!(metrics.contains(":blinks-in-window 0"));
    }

    #[test]
    fn test_teardown_resets_state() {
        let mut monitor = FatigueMonitor::new();
        monitor.session_start_s = 100.0;
        monitor.record_blink(101.0);
        monitor.record_eye_state(101.0, true);
        monitor.record_gaze_velocity(5.0);
        monitor.current_level = FatigueLevel::Critical;

        monitor.teardown();

        assert_eq!(monitor.current_level, FatigueLevel::Normal);
        assert!(monitor.blink_timestamps.is_empty());
        assert!(monitor.eye_closure_samples.is_empty());
        assert!(monitor.gaze_velocities.is_empty());
        assert!((monitor.session_start_s).abs() < f64::EPSILON);
    }
}
