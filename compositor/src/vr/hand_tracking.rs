//! OpenXR hand tracking data structures and state management.
//!
//! Models 26 joints per hand per the XR_EXT_hand_tracking spec.
//! Provides skeleton data, joint distance queries, and smoothing.
//! Compiled unconditionally (no openxrs dependency).

use tracing::debug;

// ── Joint definitions ──────────────────────────────────────

/// The 26 hand joints defined by XR_EXT_hand_tracking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HandJoint {
    Palm,
    Wrist,
    ThumbMetacarpal,
    ThumbProximal,
    ThumbDistal,
    ThumbTip,
    IndexMetacarpal,
    IndexProximal,
    IndexIntermediate,
    IndexDistal,
    IndexTip,
    MiddleMetacarpal,
    MiddleProximal,
    MiddleIntermediate,
    MiddleDistal,
    MiddleTip,
    RingMetacarpal,
    RingProximal,
    RingIntermediate,
    RingDistal,
    RingTip,
    LittleMetacarpal,
    LittleProximal,
    LittleIntermediate,
    LittleDistal,
    LittleTip,
}

/// Total number of joints per hand.
pub const JOINT_COUNT: usize = 26;

impl HandJoint {
    /// Convert joint enum to array index (0-25).
    pub fn index(&self) -> usize {
        *self as usize
    }

    /// String representation for IPC.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Palm => "palm",
            Self::Wrist => "wrist",
            Self::ThumbMetacarpal => "thumb-metacarpal",
            Self::ThumbProximal => "thumb-proximal",
            Self::ThumbDistal => "thumb-distal",
            Self::ThumbTip => "thumb-tip",
            Self::IndexMetacarpal => "index-metacarpal",
            Self::IndexProximal => "index-proximal",
            Self::IndexIntermediate => "index-intermediate",
            Self::IndexDistal => "index-distal",
            Self::IndexTip => "index-tip",
            Self::MiddleMetacarpal => "middle-metacarpal",
            Self::MiddleProximal => "middle-proximal",
            Self::MiddleIntermediate => "middle-intermediate",
            Self::MiddleDistal => "middle-distal",
            Self::MiddleTip => "middle-tip",
            Self::RingMetacarpal => "ring-metacarpal",
            Self::RingProximal => "ring-proximal",
            Self::RingIntermediate => "ring-intermediate",
            Self::RingDistal => "ring-distal",
            Self::RingTip => "ring-tip",
            Self::LittleMetacarpal => "little-metacarpal",
            Self::LittleProximal => "little-proximal",
            Self::LittleIntermediate => "little-intermediate",
            Self::LittleDistal => "little-distal",
            Self::LittleTip => "little-tip",
        }
    }

    /// Fingertip joints for convenience.
    pub fn fingertip_joints() -> [HandJoint; 5] {
        [
            Self::ThumbTip,
            Self::IndexTip,
            Self::MiddleTip,
            Self::RingTip,
            Self::LittleTip,
        ]
    }
}

// ── Hand enum ──────────────────────────────────────────────

/// Which hand.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Hand {
    Left,
    Right,
}

impl Hand {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Left => "left",
            Self::Right => "right",
        }
    }
}

// ── Joint pose ─────────────────────────────────────────────

/// Pose data for a single joint.
#[derive(Debug, Clone)]
pub struct JointPose {
    /// Position in meters (x, y, z).
    pub position: [f32; 3],
    /// Orientation quaternion (x, y, z, w).
    pub orientation: [f32; 4],
    /// Joint radius in meters.
    pub radius: f32,
    /// Whether this joint has valid tracking data.
    pub valid: bool,
}

impl Default for JointPose {
    fn default() -> Self {
        Self {
            position: [0.0, 0.0, 0.0],
            orientation: [0.0, 0.0, 0.0, 1.0],
            radius: 0.01,
            valid: false,
        }
    }
}

// ── Hand skeleton ──────────────────────────────────────────

/// Complete skeleton data for one hand.
#[derive(Debug, Clone)]
pub struct HandSkeleton {
    /// Which hand this skeleton represents.
    pub hand: Hand,
    /// 26 joint poses indexed by HandJoint.
    pub joints: Vec<JointPose>,
    /// Timestamp of last update in nanoseconds.
    pub timestamp_ns: u64,
    /// Whether hand tracking is currently active.
    pub tracking_active: bool,
    /// Overall tracking confidence (0.0-1.0).
    pub confidence: f32,
}

impl HandSkeleton {
    /// Create a new skeleton with default joint poses.
    pub fn new(hand: Hand) -> Self {
        let joints = (0..JOINT_COUNT).map(|_| JointPose::default()).collect();
        Self {
            hand,
            joints,
            timestamp_ns: 0,
            tracking_active: false,
            confidence: 0.0,
        }
    }

    /// Reset all joint data to defaults.
    pub fn reset(&mut self) {
        for joint in &mut self.joints {
            *joint = JointPose::default();
        }
        self.timestamp_ns = 0;
        self.tracking_active = false;
        self.confidence = 0.0;
    }
}

// ── Config ─────────────────────────────────────────────────

/// Configuration for hand tracking.
#[derive(Debug, Clone)]
pub struct HandTrackingConfig {
    /// Enable hand tracking processing.
    pub enabled: bool,
    /// Minimum confidence threshold (0.0-1.0) to consider tracking valid.
    pub min_confidence: f32,
    /// Smoothing factor (0.0 = no smoothing, 1.0 = maximum).
    pub smoothing: f32,
    /// Prediction time in milliseconds.
    pub prediction_ms: f32,
}

impl Default for HandTrackingConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            min_confidence: 0.5,
            smoothing: 0.3,
            prediction_ms: 20.0,
        }
    }
}

// ── State ──────────────────────────────────────────────────

/// Central hand tracking state.
pub struct HandTrackingState {
    /// Configuration.
    pub config: HandTrackingConfig,
    /// Left hand skeleton.
    pub left: HandSkeleton,
    /// Right hand skeleton.
    pub right: HandSkeleton,
    /// Whether any hand is actively tracked.
    pub active: bool,
}

impl HandTrackingState {
    /// Create a new hand tracking state with defaults.
    pub fn new() -> Self {
        Self {
            config: HandTrackingConfig::default(),
            left: HandSkeleton::new(Hand::Left),
            right: HandSkeleton::new(Hand::Right),
            active: false,
        }
    }

    /// Get the skeleton for a given hand.
    pub fn skeleton(&self, hand: Hand) -> &HandSkeleton {
        match hand {
            Hand::Left => &self.left,
            Hand::Right => &self.right,
        }
    }

    /// Get a mutable reference to the skeleton for a given hand.
    fn skeleton_mut(&mut self, hand: Hand) -> &mut HandSkeleton {
        match hand {
            Hand::Left => &mut self.left,
            Hand::Right => &mut self.right,
        }
    }

    /// Update hand skeleton with new joint data.
    ///
    /// `joints` must contain exactly 26 entries (one per HandJoint).
    /// Applies smoothing if configured and previous data is valid.
    pub fn update_hand(
        &mut self,
        hand: Hand,
        joints: Vec<JointPose>,
        timestamp_ns: u64,
        confidence: f32,
    ) {
        if !self.config.enabled {
            return;
        }

        if joints.len() != JOINT_COUNT {
            debug!(
                "Hand tracking: expected {} joints, got {} for {:?}",
                JOINT_COUNT,
                joints.len(),
                hand,
            );
            return;
        }

        let skel = self.skeleton_mut(hand);
        let alpha = self.config.smoothing;

        // Apply smoothing if previous data was valid
        if skel.tracking_active && alpha > 0.0 {
            for (i, new_joint) in joints.into_iter().enumerate() {
                if new_joint.valid && skel.joints[i].valid {
                    let old = &skel.joints[i];
                    let smoothed_pos = [
                        lerp(old.position[0], new_joint.position[0], 1.0 - alpha),
                        lerp(old.position[1], new_joint.position[1], 1.0 - alpha),
                        lerp(old.position[2], new_joint.position[2], 1.0 - alpha),
                    ];
                    skel.joints[i] = JointPose {
                        position: smoothed_pos,
                        orientation: new_joint.orientation,
                        radius: new_joint.radius,
                        valid: true,
                    };
                } else {
                    skel.joints[i] = new_joint;
                }
            }
        } else {
            skel.joints = joints;
        }

        skel.timestamp_ns = timestamp_ns;
        skel.confidence = confidence;
        skel.tracking_active = confidence >= self.config.min_confidence;

        self.active = self.left.tracking_active || self.right.tracking_active;
    }

    /// Euclidean distance between two joints on the same hand (in meters).
    pub fn joint_distance(&self, hand: Hand, j1: HandJoint, j2: HandJoint) -> f32 {
        let skel = self.skeleton(hand);
        let p1 = &skel.joints[j1.index()].position;
        let p2 = &skel.joints[j2.index()].position;
        let dx = p2[0] - p1[0];
        let dy = p2[1] - p1[1];
        let dz = p2[2] - p1[2];
        (dx * dx + dy * dy + dz * dz).sqrt()
    }

    /// Get the position of a fingertip joint.
    pub fn fingertip_position(&self, hand: Hand, finger: HandJoint) -> [f32; 3] {
        let skel = self.skeleton(hand);
        skel.joints[finger.index()].position
    }

    /// Compute the palm normal vector from the palm orientation quaternion.
    ///
    /// Extracts the "up" direction (Y axis) from the quaternion,
    /// which represents the normal to the palm surface.
    pub fn palm_normal(&self, hand: Hand) -> [f32; 3] {
        let skel = self.skeleton(hand);
        let q = &skel.joints[HandJoint::Palm.index()].orientation;
        // Rotate unit Y vector [0,1,0] by quaternion q
        // Result: 2*(q.x*q.y - q.w*q.z), 1 - 2*(q.x^2 + q.z^2), 2*(q.y*q.z + q.w*q.x)
        let (qx, qy, qz, qw) = (q[0], q[1], q[2], q[3]);
        [
            2.0 * (qx * qy - qw * qz),
            1.0 - 2.0 * (qx * qx + qz * qz),
            2.0 * (qy * qz + qw * qx),
        ]
    }

    /// Whether a given hand is actively tracked with sufficient confidence.
    pub fn is_tracking(&self, hand: Hand) -> bool {
        let skel = self.skeleton(hand);
        skel.tracking_active && skel.confidence >= self.config.min_confidence
    }

    /// Reset all tracking data.
    pub fn reset(&mut self) {
        self.left.reset();
        self.right.reset();
        self.active = false;
    }

    /// Generate s-expression for IPC status.
    pub fn status_sexp(&self) -> String {
        format!(
            "(:enabled {} :active {} :min-confidence {:.2} :smoothing {:.2} :prediction-ms {:.1} :left (:tracking {} :confidence {:.2} :timestamp {}) :right (:tracking {} :confidence {:.2} :timestamp {}))",
            if self.config.enabled { "t" } else { "nil" },
            if self.active { "t" } else { "nil" },
            self.config.min_confidence,
            self.config.smoothing,
            self.config.prediction_ms,
            if self.left.tracking_active { "t" } else { "nil" },
            self.left.confidence,
            self.left.timestamp_ns,
            if self.right.tracking_active { "t" } else { "nil" },
            self.right.confidence,
            self.right.timestamp_ns,
        )
    }
}

// ── IPC adapter types ─────────────────────────────────────

/// Named position for IPC serialization.
#[derive(Debug, Clone)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

/// Named quaternion for IPC serialization.
#[derive(Debug, Clone)]
pub struct Quat4 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub w: f32,
}

/// Joint info formatted for IPC consumption.
#[derive(Debug, Clone)]
pub struct IpcJointInfo {
    pub name: String,
    pub position: Vec3,
    pub orientation: Quat4,
    pub radius: f32,
}

/// Parse hand string ("left" or "right") to Hand enum.
fn parse_hand(s: &str) -> Option<Hand> {
    match s {
        "left" => Some(Hand::Left),
        "right" => Some(Hand::Right),
        _ => None,
    }
}

/// Parse joint name string to HandJoint enum.
fn parse_joint(s: &str) -> Option<HandJoint> {
    match s {
        "palm" => Some(HandJoint::Palm),
        "wrist" => Some(HandJoint::Wrist),
        "thumb-metacarpal" => Some(HandJoint::ThumbMetacarpal),
        "thumb-proximal" => Some(HandJoint::ThumbProximal),
        "thumb-distal" => Some(HandJoint::ThumbDistal),
        "thumb-tip" => Some(HandJoint::ThumbTip),
        "index-metacarpal" => Some(HandJoint::IndexMetacarpal),
        "index-proximal" => Some(HandJoint::IndexProximal),
        "index-intermediate" => Some(HandJoint::IndexIntermediate),
        "index-distal" => Some(HandJoint::IndexDistal),
        "index-tip" => Some(HandJoint::IndexTip),
        "middle-metacarpal" => Some(HandJoint::MiddleMetacarpal),
        "middle-proximal" => Some(HandJoint::MiddleProximal),
        "middle-intermediate" => Some(HandJoint::MiddleIntermediate),
        "middle-distal" => Some(HandJoint::MiddleDistal),
        "middle-tip" => Some(HandJoint::MiddleTip),
        "ring-metacarpal" => Some(HandJoint::RingMetacarpal),
        "ring-proximal" => Some(HandJoint::RingProximal),
        "ring-intermediate" => Some(HandJoint::RingIntermediate),
        "ring-distal" => Some(HandJoint::RingDistal),
        "ring-tip" => Some(HandJoint::RingTip),
        "little-metacarpal" => Some(HandJoint::LittleMetacarpal),
        "little-proximal" => Some(HandJoint::LittleProximal),
        "little-intermediate" => Some(HandJoint::LittleIntermediate),
        "little-distal" => Some(HandJoint::LittleDistal),
        "little-tip" => Some(HandJoint::LittleTip),
        _ => None,
    }
}

/// All joint names in order, matching HandJoint enum indices.
const JOINT_NAMES: [&str; JOINT_COUNT] = [
    "palm", "wrist",
    "thumb-metacarpal", "thumb-proximal", "thumb-distal", "thumb-tip",
    "index-metacarpal", "index-proximal", "index-intermediate", "index-distal", "index-tip",
    "middle-metacarpal", "middle-proximal", "middle-intermediate", "middle-distal", "middle-tip",
    "ring-metacarpal", "ring-proximal", "ring-intermediate", "ring-distal", "ring-tip",
    "little-metacarpal", "little-proximal", "little-intermediate", "little-distal", "little-tip",
];

fn joint_to_ipc(name: &str, pose: &JointPose) -> IpcJointInfo {
    IpcJointInfo {
        name: name.to_string(),
        position: Vec3 {
            x: pose.position[0],
            y: pose.position[1],
            z: pose.position[2],
        },
        orientation: Quat4 {
            x: pose.orientation[0],
            y: pose.orientation[1],
            z: pose.orientation[2],
            w: pose.orientation[3],
        },
        radius: pose.radius,
    }
}

impl HandTrackingState {
    /// IPC adapter: get a single joint by hand and joint name strings.
    pub fn get_joint(&self, hand_str: &str, joint_name: &str) -> Option<IpcJointInfo> {
        let hand = parse_hand(hand_str)?;
        let joint = parse_joint(joint_name)?;
        let skel = self.skeleton(hand);
        if !skel.tracking_active {
            return None;
        }
        Some(joint_to_ipc(joint_name, &skel.joints[joint.index()]))
    }

    /// IPC adapter: get all joints for a hand as IpcJointInfo.
    pub fn get_skeleton(&self, hand_str: &str) -> Option<Vec<IpcJointInfo>> {
        let hand = parse_hand(hand_str)?;
        let skel = self.skeleton(hand);
        if !skel.tracking_active {
            return None;
        }
        let joints = skel
            .joints
            .iter()
            .enumerate()
            .map(|(i, pose)| joint_to_ipc(JOINT_NAMES[i], pose))
            .collect();
        Some(joints)
    }

    /// IPC adapter: compute distance between two joints by name strings.
    pub fn joint_distance_by_name(
        &self,
        hand_str: &str,
        joint_a: &str,
        joint_b: &str,
    ) -> Option<f32> {
        let hand = parse_hand(hand_str)?;
        let ja = parse_joint(joint_a)?;
        let jb = parse_joint(joint_b)?;
        Some(self.joint_distance(hand, ja, jb))
    }
}

/// Linear interpolation helper.
fn lerp(a: f32, b: f32, t: f32) -> f32 {
    a + (b - a) * t
}

/// Create a test joint with a given position and valid flag.
#[cfg(test)]
fn test_joint(x: f32, y: f32, z: f32) -> JointPose {
    JointPose {
        position: [x, y, z],
        orientation: [0.0, 0.0, 0.0, 1.0],
        radius: 0.01,
        valid: true,
    }
}

/// Create a full set of 26 default joint poses for testing.
#[cfg(test)]
fn test_joints_default() -> Vec<JointPose> {
    (0..JOINT_COUNT).map(|_| JointPose::default()).collect()
}

// ── Tests ──────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_state() {
        let state = HandTrackingState::new();
        assert!(state.config.enabled);
        assert!(!state.active);
        assert!(!state.left.tracking_active);
        assert!(!state.right.tracking_active);
        assert_eq!(state.left.joints.len(), JOINT_COUNT);
        assert_eq!(state.right.joints.len(), JOINT_COUNT);
    }

    #[test]
    fn test_joint_count() {
        assert_eq!(HandJoint::LittleTip.index(), 25);
        assert_eq!(HandJoint::Palm.index(), 0);
        assert_eq!(JOINT_COUNT, 26);
    }

    #[test]
    fn test_update_hand_valid() {
        let mut state = HandTrackingState::new();
        let joints: Vec<JointPose> = (0..JOINT_COUNT)
            .map(|i| test_joint(i as f32 * 0.01, 0.0, 0.0))
            .collect();

        state.update_hand(Hand::Left, joints, 1000, 0.9);
        assert!(state.left.tracking_active);
        assert!(state.active);
        assert_eq!(state.left.timestamp_ns, 1000);
        assert!((state.left.confidence - 0.9).abs() < f32::EPSILON);
    }

    #[test]
    fn test_update_hand_low_confidence() {
        let mut state = HandTrackingState::new();
        state.config.min_confidence = 0.5;
        let joints = test_joints_default();

        state.update_hand(Hand::Left, joints, 1000, 0.2);
        assert!(!state.left.tracking_active);
        assert!(!state.active);
    }

    #[test]
    fn test_update_hand_wrong_joint_count() {
        let mut state = HandTrackingState::new();
        let joints = vec![JointPose::default(); 10]; // wrong count

        state.update_hand(Hand::Left, joints, 1000, 0.9);
        // Should be ignored — still not active
        assert!(!state.left.tracking_active);
    }

    #[test]
    fn test_joint_distance() {
        let mut state = HandTrackingState::new();
        let mut joints = test_joints_default();
        // Place thumb tip at (0, 0, 0) and index tip at (3, 4, 0)
        joints[HandJoint::ThumbTip.index()] = test_joint(0.0, 0.0, 0.0);
        joints[HandJoint::IndexTip.index()] = test_joint(3.0, 4.0, 0.0);

        state.update_hand(Hand::Right, joints, 1000, 0.9);

        let dist = state.joint_distance(Hand::Right, HandJoint::ThumbTip, HandJoint::IndexTip);
        assert!((dist - 5.0).abs() < 0.001, "Expected 5.0, got {}", dist);
    }

    #[test]
    fn test_fingertip_position() {
        let mut state = HandTrackingState::new();
        let mut joints = test_joints_default();
        joints[HandJoint::IndexTip.index()] = test_joint(0.1, 0.2, 0.3);

        state.update_hand(Hand::Left, joints, 1000, 0.9);

        let pos = state.fingertip_position(Hand::Left, HandJoint::IndexTip);
        assert!((pos[0] - 0.1).abs() < 0.001);
        assert!((pos[1] - 0.2).abs() < 0.001);
        assert!((pos[2] - 0.3).abs() < 0.001);
    }

    #[test]
    fn test_palm_normal_identity() {
        let mut state = HandTrackingState::new();
        let mut joints = test_joints_default();
        // Identity quaternion [0,0,0,1] -> palm normal is Y-up [0,1,0]
        joints[HandJoint::Palm.index()] = JointPose {
            position: [0.0, 0.0, 0.0],
            orientation: [0.0, 0.0, 0.0, 1.0],
            radius: 0.01,
            valid: true,
        };

        state.update_hand(Hand::Left, joints, 1000, 0.9);

        let normal = state.palm_normal(Hand::Left);
        assert!((normal[0]).abs() < 0.001);
        assert!((normal[1] - 1.0).abs() < 0.001);
        assert!((normal[2]).abs() < 0.001);
    }

    #[test]
    fn test_is_tracking() {
        let mut state = HandTrackingState::new();
        assert!(!state.is_tracking(Hand::Left));
        assert!(!state.is_tracking(Hand::Right));

        let joints = (0..JOINT_COUNT)
            .map(|_| test_joint(0.0, 0.0, 0.0))
            .collect();
        state.update_hand(Hand::Left, joints, 1000, 0.9);
        assert!(state.is_tracking(Hand::Left));
        assert!(!state.is_tracking(Hand::Right));
    }

    #[test]
    fn test_reset() {
        let mut state = HandTrackingState::new();
        let joints = (0..JOINT_COUNT)
            .map(|_| test_joint(1.0, 2.0, 3.0))
            .collect();
        state.update_hand(Hand::Left, joints, 1000, 0.9);
        assert!(state.active);

        state.reset();
        assert!(!state.active);
        assert!(!state.left.tracking_active);
        assert!(!state.right.tracking_active);
        assert_eq!(state.left.timestamp_ns, 0);
    }

    #[test]
    fn test_smoothing() {
        let mut state = HandTrackingState::new();
        state.config.smoothing = 0.5;

        // First update — no smoothing applied (no previous data)
        let joints1: Vec<JointPose> = (0..JOINT_COUNT)
            .map(|_| test_joint(1.0, 0.0, 0.0))
            .collect();
        state.update_hand(Hand::Left, joints1, 1000, 0.9);
        let pos_after_first = state.left.joints[0].position[0];
        assert!((pos_after_first - 1.0).abs() < 0.001);

        // Second update — smoothing applied (alpha=0.5, so lerp(1.0, 2.0, 0.5) = 1.5)
        let joints2: Vec<JointPose> = (0..JOINT_COUNT)
            .map(|_| test_joint(2.0, 0.0, 0.0))
            .collect();
        state.update_hand(Hand::Left, joints2, 2000, 0.9);
        let pos_after_second = state.left.joints[0].position[0];
        assert!(
            (pos_after_second - 1.5).abs() < 0.001,
            "Expected ~1.5 after smoothing, got {}",
            pos_after_second
        );
    }

    #[test]
    fn test_disabled_ignores_updates() {
        let mut state = HandTrackingState::new();
        state.config.enabled = false;

        let joints = (0..JOINT_COUNT)
            .map(|_| test_joint(1.0, 2.0, 3.0))
            .collect();
        state.update_hand(Hand::Left, joints, 1000, 0.9);
        assert!(!state.left.tracking_active);
        assert!(!state.active);
    }

    #[test]
    fn test_status_sexp() {
        let state = HandTrackingState::new();
        let sexp = state.status_sexp();
        assert!(sexp.contains(":enabled t"));
        assert!(sexp.contains(":active nil"));
        assert!(sexp.contains(":min-confidence 0.50"));
        assert!(sexp.contains(":smoothing 0.30"));
        assert!(sexp.contains(":left (:tracking nil"));
        assert!(sexp.contains(":right (:tracking nil"));
    }

    #[test]
    fn test_hand_as_str() {
        assert_eq!(Hand::Left.as_str(), "left");
        assert_eq!(Hand::Right.as_str(), "right");
    }

    #[test]
    fn test_joint_as_str() {
        assert_eq!(HandJoint::Palm.as_str(), "palm");
        assert_eq!(HandJoint::ThumbTip.as_str(), "thumb-tip");
        assert_eq!(HandJoint::IndexTip.as_str(), "index-tip");
        assert_eq!(HandJoint::LittleTip.as_str(), "little-tip");
    }
}
