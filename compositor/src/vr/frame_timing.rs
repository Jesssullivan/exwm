//! Frame timing instrumentation for VR performance monitoring.
//!
//! Tracks per-frame timing metrics and maintains rolling statistics
//! for performance analysis and IPC reporting.

#[cfg(feature = "vr")]
use std::time::{Duration, Instant};

/// Rolling frame timing statistics over a window of samples.
#[derive(Debug)]
pub struct FrameTiming {
    /// Per-frame wait time (xrWaitFrame return latency).
    pub wait_times: Vec<f64>,
    /// Per-frame render time (acquire -> render -> release).
    pub render_times: Vec<f64>,
    /// Per-frame submit time (xrEndFrame latency).
    pub submit_times: Vec<f64>,
    /// Per-frame total time.
    pub total_times: Vec<f64>,
    /// Maximum number of samples to keep.
    pub window_size: usize,
    /// Total frames submitted.
    pub total_frames: u64,
    /// Total missed frames (exceeded budget).
    pub missed_frames: u64,
    /// Frame budget in milliseconds (e.g., 11.1 for 90Hz).
    pub budget_ms: f64,
}

impl Default for FrameTiming {
    fn default() -> Self {
        Self::new(1000, 11.1)
    }
}

impl FrameTiming {
    pub fn new(window_size: usize, budget_ms: f64) -> Self {
        Self {
            wait_times: Vec::with_capacity(window_size),
            render_times: Vec::with_capacity(window_size),
            submit_times: Vec::with_capacity(window_size),
            total_times: Vec::with_capacity(window_size),
            window_size,
            total_frames: 0,
            missed_frames: 0,
            budget_ms,
        }
    }

    /// Record a frame's timing data.
    pub fn record_frame(
        &mut self,
        wait_ms: f64,
        render_ms: f64,
        submit_ms: f64,
    ) {
        let total = wait_ms + render_ms + submit_ms;

        Self::push_sample(&mut self.wait_times, wait_ms, self.window_size);
        Self::push_sample(&mut self.render_times, render_ms, self.window_size);
        Self::push_sample(&mut self.submit_times, submit_ms, self.window_size);
        Self::push_sample(&mut self.total_times, total, self.window_size);

        self.total_frames += 1;
        if total > self.budget_ms {
            self.missed_frames += 1;
        }
    }

    fn push_sample(samples: &mut Vec<f64>, value: f64, window_size: usize) {
        samples.push(value);
        if samples.len() > window_size {
            samples.remove(0);
        }
    }

    /// Compute percentile from a sorted slice.
    fn percentile(sorted: &[f64], p: f64) -> f64 {
        if sorted.is_empty() {
            return 0.0;
        }
        let idx = ((sorted.len() as f64 - 1.0) * p / 100.0).round() as usize;
        sorted[idx.min(sorted.len() - 1)]
    }

    /// Get timing statistics as percentiles.
    pub fn stats(&self) -> FrameTimingStats {
        let mut wait = self.wait_times.clone();
        let mut render = self.render_times.clone();
        let mut submit = self.submit_times.clone();
        let mut total = self.total_times.clone();

        wait.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        render.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        submit.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        total.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        FrameTimingStats {
            wait_p50: Self::percentile(&wait, 50.0),
            wait_p99: Self::percentile(&wait, 99.0),
            render_p50: Self::percentile(&render, 50.0),
            render_p99: Self::percentile(&render, 99.0),
            submit_p50: Self::percentile(&submit, 50.0),
            submit_p99: Self::percentile(&submit, 99.0),
            total_p50: Self::percentile(&total, 50.0),
            total_p95: Self::percentile(&total, 95.0),
            total_p99: Self::percentile(&total, 99.0),
            fps: if !total.is_empty() {
                1000.0 / Self::percentile(&total, 50.0)
            } else {
                0.0
            },
            missed_pct: if self.total_frames > 0 {
                (self.missed_frames as f64 / self.total_frames as f64) * 100.0
            } else {
                0.0
            },
            total_frames: self.total_frames,
            missed_frames: self.missed_frames,
        }
    }

    /// Format stats as an s-expression for IPC.
    pub fn stats_sexp(&self) -> String {
        let s = self.stats();
        format!(
            "(:wait-p50 {:.1} :render-p50 {:.1} :submit-p50 {:.1} :total-p50 {:.1} :total-p99 {:.1} :missed-pct {:.1} :fps {:.0} :total-frames {} :missed-frames {})",
            s.wait_p50, s.render_p50, s.submit_p50, s.total_p50, s.total_p99,
            s.missed_pct, s.fps, s.total_frames, s.missed_frames,
        )
    }
}

/// Computed frame timing statistics.
#[derive(Debug, Clone)]
pub struct FrameTimingStats {
    pub wait_p50: f64,
    pub wait_p99: f64,
    pub render_p50: f64,
    pub render_p99: f64,
    pub submit_p50: f64,
    pub submit_p99: f64,
    pub total_p50: f64,
    pub total_p95: f64,
    pub total_p99: f64,
    pub fps: f64,
    pub missed_pct: f64,
    pub total_frames: u64,
    pub missed_frames: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_stats() {
        let ft = FrameTiming::new(100, 11.1);
        let stats = ft.stats();
        assert_eq!(stats.total_frames, 0);
        assert_eq!(stats.missed_frames, 0);
    }

    #[test]
    fn test_record_frame() {
        let mut ft = FrameTiming::new(100, 11.1);
        ft.record_frame(2.0, 3.0, 0.5);
        assert_eq!(ft.total_frames, 1);
        assert_eq!(ft.missed_frames, 0);
        let stats = ft.stats();
        assert!((stats.total_p50 - 5.5).abs() < 0.1);
    }

    #[test]
    fn test_missed_frame_detection() {
        let mut ft = FrameTiming::new(100, 11.1);
        ft.record_frame(5.0, 5.0, 2.0); // 12ms > 11.1ms budget
        assert_eq!(ft.missed_frames, 1);
        ft.record_frame(3.0, 3.0, 1.0); // 7ms < 11.1ms budget
        assert_eq!(ft.missed_frames, 1);
        assert_eq!(ft.total_frames, 2);
    }

    #[test]
    fn test_window_size_trim() {
        let mut ft = FrameTiming::new(5, 11.1);
        for i in 0..10 {
            ft.record_frame(i as f64, 1.0, 0.5);
        }
        assert_eq!(ft.wait_times.len(), 5);
        assert_eq!(ft.total_frames, 10);
    }

    #[test]
    fn test_stats_sexp_format() {
        let mut ft = FrameTiming::new(100, 11.1);
        ft.record_frame(2.0, 3.0, 0.5);
        let sexp = ft.stats_sexp();
        assert!(sexp.starts_with("(:wait-p50"));
        assert!(sexp.contains(":fps"));
        assert!(sexp.contains(":total-frames 1"));
    }
}
