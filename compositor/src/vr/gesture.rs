//! Gesture recognition from hand skeleton data.
//!
//! Detects pinch, grab, point, open palm, thumbs up, and swipe gestures
//! from `HandSkeleton` joint positions.  Emits events with debouncing
//! and hold-duration tracking.  Compiled unconditionally (no openxrs dependency).

use tracing::debug;

use super::hand_tracking::{Hand, HandJoint, HandSkeleton, JOINT_COUNT};

// ── Gesture types ──────────────────────────────────────────

/// Recognized gesture types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GestureType {
    /// Thumb and index fingertips close together.
    Pinch,
    /// All fingertips close to palm (fist).
    Grab,
    /// Index finger extended, others curled.
    Point,
    /// All fingers extended, palm facing forward.
    OpenPalm,
    /// Thumb extended upward, others curled.
    Thumbsup,
    /// Rapid hand movement in a direction.
    Swipe,
}

impl GestureType {
    /// String representation for IPC.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Pinch => "pinch",
            Self::Grab => "grab",
            Self::Point => "point",
            Self::OpenPalm => "open-palm",
            Self::Thumbsup => "thumbsup",
            Self::Swipe => "swipe",
        }
    }
}

/// Direction of a swipe gesture.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SwipeDirection {
    Left,
    Right,
    Up,
    Down,
}

impl SwipeDirection {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Left => "left",
            Self::Right => "right",
            Self::Up => "up",
            Self::Down => "down",
        }
    }
}

// ── Events ─────────────────────────────────────────────────

/// Events emitted by gesture recognition.
#[derive(Debug, Clone, PartialEq)]
pub enum GestureEvent {
    /// A gesture has just been recognized.
    Started {
        hand: Hand,
        gesture: GestureType,
        confidence: f32,
    },
    /// A gesture is being held.
    Held {
        hand: Hand,
        gesture: GestureType,
        duration_ms: f64,
    },
    /// A previously active gesture was released.
    Released {
        hand: Hand,
        gesture: GestureType,
    },
    /// A swipe gesture was detected.
    Swipe {
        hand: Hand,
        direction: SwipeDirection,
        velocity: f32,
    },
}

// ── Config ─────────────────────────────────────────────────

/// Configuration for gesture recognition thresholds.
#[derive(Debug, Clone)]
pub struct GestureConfig {
    /// Enable gesture recognition.
    pub enabled: bool,
    /// Maximum distance (meters) between thumb and index tips for pinch.
    pub pinch_threshold_m: f32,
    /// Maximum average distance (meters) of fingertips to palm for grab.
    pub grab_threshold_m: f32,
    /// Maximum curl angle (degrees) for a finger to be "extended".
    pub point_angle_deg: f32,
    /// Minimum palm velocity (m/s) to trigger a swipe.
    pub swipe_min_velocity: f32,
    /// Minimum distance (meters) for a swipe gesture.
    pub swipe_min_distance_m: f32,
    /// Debounce time (ms) before a new gesture can start.
    pub debounce_ms: f64,
    /// Time (ms) before a Started gesture transitions to Held.
    pub hold_threshold_ms: f64,
}

impl Default for GestureConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            pinch_threshold_m: 0.025,
            grab_threshold_m: 0.040,
            point_angle_deg: 30.0,
            swipe_min_velocity: 0.5,
            swipe_min_distance_m: 0.100,
            debounce_ms: 100.0,
            hold_threshold_ms: 300.0,
        }
    }
}

// ── Per-hand tracking ──────────────────────────────────────

/// Active gesture state for a single hand.
#[derive(Debug, Clone)]
struct HandGestureState {
    /// Currently active gesture (if any).
    active_gesture: Option<GestureType>,
    /// Duration the current gesture has been held (ms).
    hold_duration_ms: f64,
    /// Whether we have emitted a Held event (vs just Started).
    held_emitted: bool,
    /// Time since last gesture ended (for debounce).
    cooldown_ms: f64,
    /// Previous palm position for swipe detection.
    prev_palm_pos: Option<[f32; 3]>,
    /// Accumulated swipe displacement.
    swipe_displacement: [f32; 3],
    /// Whether we are in a potential swipe.
    swipe_tracking: bool,
}

impl HandGestureState {
    fn new() -> Self {
        Self {
            active_gesture: None,
            hold_duration_ms: 0.0,
            held_emitted: false,
            cooldown_ms: 0.0,
            prev_palm_pos: None,
            swipe_displacement: [0.0; 3],
            swipe_tracking: false,
        }
    }

    fn reset(&mut self) {
        *self = Self::new();
    }
}

// ── State ──────────────────────────────────────────────────

/// A gesture-to-action binding.
#[derive(Debug, Clone)]
pub struct GestureBinding {
    /// Gesture name (e.g. "pinch", "grab").
    pub gesture: String,
    /// Hand name (e.g. "left", "right").
    pub hand: String,
    /// Action identifier (e.g. "select", "close").
    pub action: String,
}

/// Central gesture recognition state.
pub struct GestureState {
    /// Configuration.
    pub config: GestureConfig,
    /// Left hand gesture tracking.
    left: HandGestureState,
    /// Right hand gesture tracking.
    right: HandGestureState,
    /// Gesture-to-action bindings.
    bindings: Vec<GestureBinding>,
}

impl GestureState {
    /// Create a new gesture state with defaults.
    pub fn new() -> Self {
        Self {
            config: GestureConfig::default(),
            left: HandGestureState::new(),
            right: HandGestureState::new(),
            bindings: Vec::new(),
        }
    }

    /// Get per-hand state.
    fn hand_state(&self, hand: Hand) -> &HandGestureState {
        match hand {
            Hand::Left => &self.left,
            Hand::Right => &self.right,
        }
    }

    /// Get mutable per-hand state.
    fn hand_state_mut(&mut self, hand: Hand) -> &mut HandGestureState {
        match hand {
            Hand::Left => &mut self.left,
            Hand::Right => &mut self.right,
        }
    }

    /// Process skeleton data for both hands and return any events.
    pub fn update(
        &mut self,
        left_skeleton: &HandSkeleton,
        right_skeleton: &HandSkeleton,
        dt_ms: f64,
    ) -> Vec<GestureEvent> {
        if !self.config.enabled {
            return Vec::new();
        }

        let mut events = Vec::new();

        // Process each hand independently
        let left_events = self.update_hand(Hand::Left, left_skeleton, dt_ms);
        let right_events = self.update_hand(Hand::Right, right_skeleton, dt_ms);

        events.extend(left_events);
        events.extend(right_events);
        events
    }

    /// Process a single hand's skeleton.
    fn update_hand(
        &mut self,
        hand: Hand,
        skeleton: &HandSkeleton,
        dt_ms: f64,
    ) -> Vec<GestureEvent> {
        let mut events = Vec::new();

        if !skeleton.tracking_active {
            // Hand lost — release any active gesture
            let hs = self.hand_state_mut(hand);
            if let Some(gesture) = hs.active_gesture.take() {
                events.push(GestureEvent::Released { hand, gesture });
                hs.hold_duration_ms = 0.0;
                hs.held_emitted = false;
            }
            hs.prev_palm_pos = None;
            hs.swipe_tracking = false;
            return events;
        }

        // Detect swipe from palm velocity
        let swipe_event = self.detect_swipe(hand, skeleton, dt_ms);
        if let Some(evt) = swipe_event {
            events.push(evt);
        }

        // Detect static gestures
        let detected = self.classify_gesture(skeleton);

        let hs = self.hand_state_mut(hand);

        // Update cooldown
        if hs.active_gesture.is_none() {
            hs.cooldown_ms += dt_ms;
        }

        match (hs.active_gesture, detected) {
            (Some(current), Some(new_gesture)) if current == new_gesture => {
                // Same gesture continues — update hold
                hs.hold_duration_ms += dt_ms;
                if hs.hold_duration_ms >= self.config.hold_threshold_ms && !hs.held_emitted {
                    hs.held_emitted = true;
                    events.push(GestureEvent::Held {
                        hand,
                        gesture: current,
                        duration_ms: hs.hold_duration_ms,
                    });
                }
            }
            (Some(current), _) => {
                // Gesture changed or ended
                hs.active_gesture = None;
                hs.hold_duration_ms = 0.0;
                hs.held_emitted = false;
                hs.cooldown_ms = 0.0;
                events.push(GestureEvent::Released {
                    hand,
                    gesture: current,
                });

                // Start new gesture if detected (skip debounce for immediate transition)
                if let Some(new_gesture) = detected {
                    hs.active_gesture = Some(new_gesture);
                    hs.hold_duration_ms = 0.0;
                    hs.held_emitted = false;
                    events.push(GestureEvent::Started {
                        hand,
                        gesture: new_gesture,
                        confidence: skeleton.confidence,
                    });
                }
            }
            (None, Some(new_gesture)) => {
                // New gesture detected — check debounce
                if hs.cooldown_ms >= self.config.debounce_ms {
                    hs.active_gesture = Some(new_gesture);
                    hs.hold_duration_ms = 0.0;
                    hs.held_emitted = false;
                    debug!("Gesture started: {:?} on {:?}", new_gesture, hand);
                    events.push(GestureEvent::Started {
                        hand,
                        gesture: new_gesture,
                        confidence: skeleton.confidence,
                    });
                }
            }
            (None, None) => {
                // No gesture
            }
        }

        events
    }

    /// Classify static gesture from skeleton joint positions.
    fn classify_gesture(&self, skeleton: &HandSkeleton) -> Option<GestureType> {
        if skeleton.joints.len() != JOINT_COUNT {
            return None;
        }

        let pinch_dist = joint_distance(
            &skeleton.joints[HandJoint::ThumbTip.index()].position,
            &skeleton.joints[HandJoint::IndexTip.index()].position,
        );

        // Pinch: thumb tip close to index tip
        if pinch_dist < self.config.pinch_threshold_m {
            return Some(GestureType::Pinch);
        }

        // Compute fingertip-to-palm distances
        let palm_pos = &skeleton.joints[HandJoint::Palm.index()].position;
        let fingertip_dists: Vec<f32> = HandJoint::fingertip_joints()
            .iter()
            .map(|j| joint_distance(&skeleton.joints[j.index()].position, palm_pos))
            .collect();

        let thumb_dist = fingertip_dists[0];
        let index_dist = fingertip_dists[1];
        let middle_dist = fingertip_dists[2];
        let ring_dist = fingertip_dists[3];
        let little_dist = fingertip_dists[4];

        // Grab: all fingertips close to palm
        let avg_dist = fingertip_dists.iter().sum::<f32>() / fingertip_dists.len() as f32;
        if avg_dist < self.config.grab_threshold_m {
            return Some(GestureType::Grab);
        }

        // Use a threshold to classify "extended" vs "curled"
        // Finger is "extended" if its tip is far from palm
        let extend_threshold = self.config.grab_threshold_m * 1.5;
        let thumb_extended = thumb_dist > extend_threshold;
        let index_extended = index_dist > extend_threshold;
        let middle_extended = middle_dist > extend_threshold;
        let ring_extended = ring_dist > extend_threshold;
        let little_extended = little_dist > extend_threshold;

        // Point: index extended, others curled
        if index_extended && !middle_extended && !ring_extended && !little_extended {
            return Some(GestureType::Point);
        }

        // Thumbs up: thumb extended, others curled
        if thumb_extended && !index_extended && !middle_extended && !ring_extended && !little_extended
        {
            return Some(GestureType::Thumbsup);
        }

        // Open palm: all fingers extended
        if thumb_extended && index_extended && middle_extended && ring_extended && little_extended {
            return Some(GestureType::OpenPalm);
        }

        None
    }

    /// Detect swipe from palm velocity.
    fn detect_swipe(
        &mut self,
        hand: Hand,
        skeleton: &HandSkeleton,
        dt_ms: f64,
    ) -> Option<GestureEvent> {
        let palm_pos = skeleton.joints[HandJoint::Palm.index()].position;
        let hs = self.hand_state_mut(hand);

        let prev = match hs.prev_palm_pos {
            Some(p) => p,
            None => {
                hs.prev_palm_pos = Some(palm_pos);
                return None;
            }
        };

        hs.prev_palm_pos = Some(palm_pos);

        if dt_ms <= 0.0 {
            return None;
        }

        let dt_s = (dt_ms / 1000.0) as f32;
        let dx = palm_pos[0] - prev[0];
        let dy = palm_pos[1] - prev[1];
        let displacement = (dx * dx + dy * dy).sqrt();
        let velocity = displacement / dt_s;

        if velocity >= self.config.swipe_min_velocity {
            hs.swipe_displacement[0] += dx;
            hs.swipe_displacement[1] += dy;
            hs.swipe_tracking = true;

            let total_disp = (hs.swipe_displacement[0].powi(2)
                + hs.swipe_displacement[1].powi(2))
            .sqrt();

            if total_disp >= self.config.swipe_min_distance_m {
                let sx = hs.swipe_displacement[0];
                let sy = hs.swipe_displacement[1];

                // Determine dominant axis
                let direction = if sx.abs() > sy.abs() {
                    if sx > 0.0 {
                        SwipeDirection::Right
                    } else {
                        SwipeDirection::Left
                    }
                } else if sy > 0.0 {
                    SwipeDirection::Up
                } else {
                    SwipeDirection::Down
                };

                // Reset swipe tracking
                hs.swipe_displacement = [0.0; 3];
                hs.swipe_tracking = false;

                debug!("Swipe detected: {:?} on {:?}, velocity={:.2}", direction, hand, velocity);
                return Some(GestureEvent::Swipe {
                    hand,
                    direction,
                    velocity,
                });
            }
        } else if hs.swipe_tracking {
            // Velocity dropped below threshold — reset
            hs.swipe_displacement = [0.0; 3];
            hs.swipe_tracking = false;
        }

        None
    }

    /// Whether a specific gesture is currently active on a hand.
    pub fn is_active(&self, hand: Hand, gesture: GestureType) -> bool {
        self.hand_state(hand).active_gesture == Some(gesture)
    }

    // ── IPC binding methods ───────────────────────────────

    /// Add a gesture-to-action binding (IPC adapter, string-based).
    pub fn add_binding(&mut self, gesture: &str, hand: &str, action: &str) {
        // Remove existing binding for same gesture+hand
        self.bindings
            .retain(|b| !(b.gesture == gesture && b.hand == hand));
        self.bindings.push(GestureBinding {
            gesture: gesture.to_string(),
            hand: hand.to_string(),
            action: action.to_string(),
        });
    }

    /// Remove a gesture binding. Returns true if a binding was removed.
    pub fn remove_binding(&mut self, gesture: &str, hand: &str) -> bool {
        let before = self.bindings.len();
        self.bindings
            .retain(|b| !(b.gesture == gesture && b.hand == hand));
        self.bindings.len() < before
    }

    /// Number of active bindings.
    pub fn binding_count(&self) -> usize {
        self.bindings.len()
    }

    /// Generate s-expression listing all bindings.
    pub fn bindings_sexp(&self) -> String {
        if self.bindings.is_empty() {
            return "nil".to_string();
        }
        let mut s = String::from("(");
        for (i, b) in self.bindings.iter().enumerate() {
            if i > 0 {
                s.push(' ');
            }
            s.push_str(&format!(
                "(:gesture \"{}\" :hand :{} :action \"{}\")",
                b.gesture, b.hand, b.action,
            ));
        }
        s.push(')');
        s
    }

    /// Reset all gesture state.
    pub fn reset(&mut self) {
        self.left.reset();
        self.right.reset();
    }

    /// Generate s-expression for IPC status.
    pub fn status_sexp(&self) -> String {
        let left_gesture = self
            .left
            .active_gesture
            .map(|g| g.as_str().to_string())
            .unwrap_or_else(|| "nil".to_string());
        let right_gesture = self
            .right
            .active_gesture
            .map(|g| g.as_str().to_string())
            .unwrap_or_else(|| "nil".to_string());
        format!(
            "(:enabled {} :left (:gesture {} :hold-ms {:.0}) :right (:gesture {} :hold-ms {:.0}))",
            if self.config.enabled { "t" } else { "nil" },
            left_gesture,
            self.left.hold_duration_ms,
            right_gesture,
            self.right.hold_duration_ms,
        )
    }

    /// Generate s-expression for IPC config.
    pub fn config_sexp(&self) -> String {
        format!(
            "(:enabled {} :pinch-threshold-m {:.3} :grab-threshold-m {:.3} :point-angle-deg {:.1} :swipe-min-velocity {:.2} :swipe-min-distance-m {:.3} :debounce-ms {:.0} :hold-threshold-ms {:.0})",
            if self.config.enabled { "t" } else { "nil" },
            self.config.pinch_threshold_m,
            self.config.grab_threshold_m,
            self.config.point_angle_deg,
            self.config.swipe_min_velocity,
            self.config.swipe_min_distance_m,
            self.config.debounce_ms,
            self.config.hold_threshold_ms,
        )
    }
}

/// Euclidean distance between two 3D points.
fn joint_distance(a: &[f32; 3], b: &[f32; 3]) -> f32 {
    let dx = b[0] - a[0];
    let dy = b[1] - a[1];
    let dz = b[2] - a[2];
    (dx * dx + dy * dy + dz * dz).sqrt()
}

// ── Test helpers ───────────────────────────────────────────

#[cfg(test)]
fn make_skeleton(hand: Hand, active: bool) -> HandSkeleton {
    use super::hand_tracking::JointPose;

    let joints = (0..JOINT_COUNT)
        .map(|_| JointPose {
            position: [0.0, 0.0, 0.0],
            orientation: [0.0, 0.0, 0.0, 1.0],
            radius: 0.01,
            valid: active,
        })
        .collect();
    HandSkeleton {
        hand,
        joints,
        timestamp_ns: 1000,
        tracking_active: active,
        confidence: if active { 0.9 } else { 0.0 },
    }
}

#[cfg(test)]
fn set_joint_pos(skeleton: &mut HandSkeleton, joint: HandJoint, x: f32, y: f32, z: f32) {
    skeleton.joints[joint.index()].position = [x, y, z];
}

// ── Tests ──────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_state() {
        let state = GestureState::new();
        assert!(state.config.enabled);
        assert!(state.left.active_gesture.is_none());
        assert!(state.right.active_gesture.is_none());
    }

    #[test]
    fn test_pinch_detection() {
        let mut state = GestureState::new();
        state.config.debounce_ms = 0.0; // disable debounce

        let mut left = make_skeleton(Hand::Left, true);
        let right = make_skeleton(Hand::Right, false);

        // Place thumb tip and index tip very close together
        set_joint_pos(&mut left, HandJoint::Palm, 0.0, 0.0, 0.0);
        set_joint_pos(&mut left, HandJoint::ThumbTip, 0.1, 0.1, 0.0);
        set_joint_pos(&mut left, HandJoint::IndexTip, 0.11, 0.1, 0.0); // 0.01m apart

        // Place other fingertips far from index (not grab)
        set_joint_pos(&mut left, HandJoint::MiddleTip, 0.2, 0.2, 0.0);
        set_joint_pos(&mut left, HandJoint::RingTip, 0.2, 0.2, 0.0);
        set_joint_pos(&mut left, HandJoint::LittleTip, 0.2, 0.2, 0.0);

        let events = state.update(&left, &right, 16.0);
        assert!(
            events.iter().any(|e| matches!(e,
                GestureEvent::Started { gesture: GestureType::Pinch, .. }
            )),
            "Expected pinch started, got {:?}",
            events,
        );
    }

    #[test]
    fn test_grab_detection() {
        let mut state = GestureState::new();
        state.config.debounce_ms = 0.0;

        let mut left = make_skeleton(Hand::Left, true);
        let right = make_skeleton(Hand::Right, false);

        // Place palm at origin, all fingertips close to palm
        set_joint_pos(&mut left, HandJoint::Palm, 0.0, 0.0, 0.0);
        set_joint_pos(&mut left, HandJoint::ThumbTip, 0.02, 0.02, 0.0);
        set_joint_pos(&mut left, HandJoint::IndexTip, 0.02, -0.02, 0.0);
        set_joint_pos(&mut left, HandJoint::MiddleTip, 0.03, 0.0, 0.0);
        set_joint_pos(&mut left, HandJoint::RingTip, 0.02, 0.01, 0.0);
        set_joint_pos(&mut left, HandJoint::LittleTip, 0.01, 0.01, 0.0);

        let events = state.update(&left, &right, 16.0);
        assert!(
            events.iter().any(|e| matches!(e,
                GestureEvent::Started { gesture: GestureType::Grab, .. }
            )),
            "Expected grab started, got {:?}",
            events,
        );
    }

    #[test]
    fn test_point_detection() {
        let mut state = GestureState::new();
        state.config.debounce_ms = 0.0;

        let mut left = make_skeleton(Hand::Left, true);
        let right = make_skeleton(Hand::Right, false);

        // Palm at origin, index extended, others curled
        set_joint_pos(&mut left, HandJoint::Palm, 0.0, 0.0, 0.0);
        set_joint_pos(&mut left, HandJoint::ThumbTip, 0.03, 0.0, 0.0); // curled (within extend threshold)
        set_joint_pos(&mut left, HandJoint::IndexTip, 0.0, 0.15, 0.0); // extended far
        set_joint_pos(&mut left, HandJoint::MiddleTip, 0.02, 0.0, 0.0); // curled
        set_joint_pos(&mut left, HandJoint::RingTip, 0.02, 0.0, 0.0); // curled
        set_joint_pos(&mut left, HandJoint::LittleTip, 0.02, 0.0, 0.0); // curled

        let events = state.update(&left, &right, 16.0);
        assert!(
            events.iter().any(|e| matches!(e,
                GestureEvent::Started { gesture: GestureType::Point, .. }
            )),
            "Expected point started, got {:?}",
            events,
        );
    }

    #[test]
    fn test_open_palm_detection() {
        let mut state = GestureState::new();
        state.config.debounce_ms = 0.0;

        let mut left = make_skeleton(Hand::Left, true);
        let right = make_skeleton(Hand::Right, false);

        // All fingertips far from palm
        set_joint_pos(&mut left, HandJoint::Palm, 0.0, 0.0, 0.0);
        set_joint_pos(&mut left, HandJoint::ThumbTip, -0.1, 0.08, 0.0);
        set_joint_pos(&mut left, HandJoint::IndexTip, -0.03, 0.12, 0.0);
        set_joint_pos(&mut left, HandJoint::MiddleTip, 0.0, 0.13, 0.0);
        set_joint_pos(&mut left, HandJoint::RingTip, 0.03, 0.12, 0.0);
        set_joint_pos(&mut left, HandJoint::LittleTip, 0.06, 0.10, 0.0);

        let events = state.update(&left, &right, 16.0);
        assert!(
            events.iter().any(|e| matches!(e,
                GestureEvent::Started { gesture: GestureType::OpenPalm, .. }
            )),
            "Expected open-palm started, got {:?}",
            events,
        );
    }

    #[test]
    fn test_gesture_release_on_tracking_lost() {
        let mut state = GestureState::new();
        state.config.debounce_ms = 0.0;

        let mut left = make_skeleton(Hand::Left, true);
        let right = make_skeleton(Hand::Right, false);

        // Start a pinch
        set_joint_pos(&mut left, HandJoint::Palm, 0.0, 0.0, 0.0);
        set_joint_pos(&mut left, HandJoint::ThumbTip, 0.1, 0.1, 0.0);
        set_joint_pos(&mut left, HandJoint::IndexTip, 0.11, 0.1, 0.0);
        set_joint_pos(&mut left, HandJoint::MiddleTip, 0.2, 0.2, 0.0);
        set_joint_pos(&mut left, HandJoint::RingTip, 0.2, 0.2, 0.0);
        set_joint_pos(&mut left, HandJoint::LittleTip, 0.2, 0.2, 0.0);
        state.update(&left, &right, 16.0);
        assert!(state.is_active(Hand::Left, GestureType::Pinch));

        // Lose tracking
        left.tracking_active = false;
        let events = state.update(&left, &right, 16.0);
        assert!(
            events.iter().any(|e| matches!(e,
                GestureEvent::Released { gesture: GestureType::Pinch, .. }
            )),
            "Expected release on tracking lost, got {:?}",
            events,
        );
        assert!(!state.is_active(Hand::Left, GestureType::Pinch));
    }

    #[test]
    fn test_hold_event() {
        let mut state = GestureState::new();
        state.config.debounce_ms = 0.0;
        state.config.hold_threshold_ms = 100.0;

        let mut left = make_skeleton(Hand::Left, true);
        let right = make_skeleton(Hand::Right, false);

        // Set up pinch
        set_joint_pos(&mut left, HandJoint::Palm, 0.0, 0.0, 0.0);
        set_joint_pos(&mut left, HandJoint::ThumbTip, 0.1, 0.1, 0.0);
        set_joint_pos(&mut left, HandJoint::IndexTip, 0.11, 0.1, 0.0);
        set_joint_pos(&mut left, HandJoint::MiddleTip, 0.2, 0.2, 0.0);
        set_joint_pos(&mut left, HandJoint::RingTip, 0.2, 0.2, 0.0);
        set_joint_pos(&mut left, HandJoint::LittleTip, 0.2, 0.2, 0.0);

        // Frame 1: started
        state.update(&left, &right, 16.0);
        assert!(state.is_active(Hand::Left, GestureType::Pinch));

        // Frame 2: not yet held
        let events = state.update(&left, &right, 50.0);
        assert!(
            !events.iter().any(|e| matches!(e, GestureEvent::Held { .. })),
            "Should not emit Held yet",
        );

        // Frame 3: past hold threshold
        let events = state.update(&left, &right, 60.0);
        assert!(
            events.iter().any(|e| matches!(e, GestureEvent::Held { .. })),
            "Expected Held event, got {:?}",
            events,
        );
    }

    #[test]
    fn test_debounce() {
        let mut state = GestureState::new();
        state.config.debounce_ms = 200.0;

        let mut left = make_skeleton(Hand::Left, true);
        let right = make_skeleton(Hand::Right, false);

        // Set up pinch
        set_joint_pos(&mut left, HandJoint::Palm, 0.0, 0.0, 0.0);
        set_joint_pos(&mut left, HandJoint::ThumbTip, 0.1, 0.1, 0.0);
        set_joint_pos(&mut left, HandJoint::IndexTip, 0.11, 0.1, 0.0);
        set_joint_pos(&mut left, HandJoint::MiddleTip, 0.2, 0.2, 0.0);
        set_joint_pos(&mut left, HandJoint::RingTip, 0.2, 0.2, 0.0);
        set_joint_pos(&mut left, HandJoint::LittleTip, 0.2, 0.2, 0.0);

        // Cooldown starts at 0, not enough for debounce
        let events = state.update(&left, &right, 16.0);
        assert!(
            !events.iter().any(|e| matches!(e, GestureEvent::Started { .. })),
            "Should be debounced, got {:?}",
            events,
        );

        // Accumulate cooldown past debounce
        // Reset and update again with enough time
        state.left.cooldown_ms = 250.0;
        let events = state.update(&left, &right, 16.0);
        assert!(
            events.iter().any(|e| matches!(e, GestureEvent::Started { .. })),
            "Should start after debounce, got {:?}",
            events,
        );
    }

    #[test]
    fn test_is_active() {
        let state = GestureState::new();
        assert!(!state.is_active(Hand::Left, GestureType::Pinch));
        assert!(!state.is_active(Hand::Right, GestureType::Grab));
    }

    #[test]
    fn test_reset() {
        let mut state = GestureState::new();
        state.left.active_gesture = Some(GestureType::Pinch);
        state.left.hold_duration_ms = 500.0;
        state.right.active_gesture = Some(GestureType::Grab);

        state.reset();
        assert!(state.left.active_gesture.is_none());
        assert!(state.right.active_gesture.is_none());
        assert_eq!(state.left.hold_duration_ms, 0.0);
    }

    #[test]
    fn test_disabled_no_events() {
        let mut state = GestureState::new();
        state.config.enabled = false;

        let left = make_skeleton(Hand::Left, true);
        let right = make_skeleton(Hand::Right, false);

        let events = state.update(&left, &right, 16.0);
        assert!(events.is_empty());
    }

    #[test]
    fn test_status_sexp() {
        let state = GestureState::new();
        let sexp = state.status_sexp();
        assert!(sexp.contains(":enabled t"));
        assert!(sexp.contains(":left (:gesture nil"));
        assert!(sexp.contains(":right (:gesture nil"));
    }

    #[test]
    fn test_config_sexp() {
        let state = GestureState::new();
        let sexp = state.config_sexp();
        assert!(sexp.contains(":enabled t"));
        assert!(sexp.contains(":pinch-threshold-m 0.025"));
        assert!(sexp.contains(":grab-threshold-m 0.040"));
        assert!(sexp.contains(":debounce-ms 100"));
    }

    #[test]
    fn test_gesture_type_as_str() {
        assert_eq!(GestureType::Pinch.as_str(), "pinch");
        assert_eq!(GestureType::Grab.as_str(), "grab");
        assert_eq!(GestureType::Point.as_str(), "point");
        assert_eq!(GestureType::OpenPalm.as_str(), "open-palm");
        assert_eq!(GestureType::Thumbsup.as_str(), "thumbsup");
        assert_eq!(GestureType::Swipe.as_str(), "swipe");
    }

    #[test]
    fn test_thumbsup_detection() {
        let mut state = GestureState::new();
        state.config.debounce_ms = 0.0;

        let mut left = make_skeleton(Hand::Left, true);
        let right = make_skeleton(Hand::Right, false);

        // Thumb extended, others curled
        set_joint_pos(&mut left, HandJoint::Palm, 0.0, 0.0, 0.0);
        set_joint_pos(&mut left, HandJoint::ThumbTip, 0.0, 0.12, 0.0); // extended up
        set_joint_pos(&mut left, HandJoint::IndexTip, 0.02, 0.0, 0.0); // curled
        set_joint_pos(&mut left, HandJoint::MiddleTip, 0.02, 0.0, 0.0); // curled
        set_joint_pos(&mut left, HandJoint::RingTip, 0.02, 0.0, 0.0); // curled
        set_joint_pos(&mut left, HandJoint::LittleTip, 0.02, 0.0, 0.0); // curled

        let events = state.update(&left, &right, 16.0);
        assert!(
            events.iter().any(|e| matches!(e,
                GestureEvent::Started { gesture: GestureType::Thumbsup, .. }
            )),
            "Expected thumbsup started, got {:?}",
            events,
        );
    }
}
