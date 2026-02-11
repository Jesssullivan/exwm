//! VR virtual keyboard for text input.
//!
//! Provides a keyboard layout engine with hit testing, modifier handling,
//! and multi-layout support (QWERTY, Dvorak, Colemak).  Works with
//! hand tracking or controller pointer input.
//! Compiled unconditionally (no openxrs dependency).

use tracing::debug;

// ── Layout enum ────────────────────────────────────────────

/// Supported keyboard layouts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeyboardLayout {
    Qwerty,
    Dvorak,
    Colemak,
}

impl KeyboardLayout {
    /// String representation for IPC.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Qwerty => "qwerty",
            Self::Dvorak => "dvorak",
            Self::Colemak => "colemak",
        }
    }
}

// ── Key definition ─────────────────────────────────────────

/// Definition of a single key on the virtual keyboard.
#[derive(Debug, Clone)]
pub struct KeyDef {
    /// Display label (e.g. "A", "Shift", "Enter").
    pub label: String,
    /// Key code string (e.g. "a", "shift", "backspace").
    pub key_code: String,
    /// X position on keyboard plane (mm from left edge).
    pub x: f32,
    /// Y position on keyboard plane (mm from top edge).
    pub y: f32,
    /// Key width (mm).
    pub width: f32,
    /// Key height (mm).
    pub height: f32,
    /// Whether this key is a modifier (shift, ctrl, alt, meta).
    pub is_modifier: bool,
}

impl KeyDef {
    /// Whether point (px, py) is inside this key's bounding box.
    pub fn contains(&self, px: f32, py: f32) -> bool {
        px >= self.x && px <= self.x + self.width && py >= self.y && py <= self.y + self.height
    }
}

// ── Events ─────────────────────────────────────────────────

/// Events emitted by the virtual keyboard.
#[derive(Debug, Clone, PartialEq)]
pub enum KeyboardEvent {
    /// A key was pressed down.
    KeyDown { key: String },
    /// A key was released.
    KeyUp { key: String },
    /// Text was committed to the input buffer.
    TextInput { text: String },
    /// The keyboard layout was changed.
    LayoutChanged { layout: KeyboardLayout },
}

// ── Config ─────────────────────────────────────────────────

/// Configuration for the virtual keyboard.
#[derive(Debug, Clone)]
pub struct VirtualKeyboardConfig {
    /// Active keyboard layout.
    pub layout: KeyboardLayout,
    /// Key size in millimeters.
    pub key_size_mm: f32,
    /// Enable haptic feedback on key press.
    pub haptic_on_press: bool,
    /// Auto-capitalize first letter after sentence-ending punctuation.
    pub auto_capitalize: bool,
    /// Enable text prediction (stub).
    pub prediction: bool,
}

impl Default for VirtualKeyboardConfig {
    fn default() -> Self {
        Self {
            layout: KeyboardLayout::Qwerty,
            key_size_mm: 20.0,
            haptic_on_press: true,
            auto_capitalize: true,
            prediction: false,
        }
    }
}

// ── State ──────────────────────────────────────────────────

/// Central virtual keyboard state.
pub struct VirtualKeyboardState {
    /// Configuration.
    pub config: VirtualKeyboardConfig,
    /// Whether the keyboard is currently visible.
    pub visible: bool,
    /// Current key layout.
    pub keys: Vec<KeyDef>,
    /// Currently held modifier keys.
    pub active_modifiers: Vec<String>,
    /// Accumulated text input buffer.
    pub input_buffer: String,
    /// Whether shift is active (for the next character).
    shift_active: bool,
    /// Whether caps lock is active.
    caps_lock: bool,
}

impl VirtualKeyboardState {
    /// Create a new virtual keyboard with QWERTY layout.
    pub fn new() -> Self {
        let keys = generate_layout(KeyboardLayout::Qwerty, 20.0);
        Self {
            config: VirtualKeyboardConfig::default(),
            visible: false,
            keys,
            active_modifiers: Vec::new(),
            input_buffer: String::new(),
            shift_active: false,
            caps_lock: false,
        }
    }

    /// Show the virtual keyboard.
    pub fn show(&mut self) {
        self.visible = true;
        debug!("Virtual keyboard shown");
    }

    /// Hide the virtual keyboard.
    pub fn hide(&mut self) {
        self.visible = false;
        debug!("Virtual keyboard hidden");
    }

    /// Toggle keyboard visibility.
    pub fn toggle(&mut self) {
        if self.visible {
            self.hide();
        } else {
            self.show();
        }
    }

    /// Find the key at a given position (mm coordinates).
    pub fn hit_test(&self, x: f32, y: f32) -> Option<&KeyDef> {
        self.keys.iter().find(|k| k.contains(x, y))
    }

    /// Process a key press and return events.
    pub fn press_key(&mut self, key_code: &str) -> Vec<KeyboardEvent> {
        let mut events = Vec::new();

        events.push(KeyboardEvent::KeyDown {
            key: key_code.to_string(),
        });

        match key_code {
            "shift" => {
                if !self.active_modifiers.contains(&"shift".to_string()) {
                    self.active_modifiers.push("shift".to_string());
                }
                self.shift_active = !self.shift_active;
            }
            "caps" => {
                self.caps_lock = !self.caps_lock;
            }
            "ctrl" => {
                if !self.active_modifiers.contains(&"ctrl".to_string()) {
                    self.active_modifiers.push("ctrl".to_string());
                }
            }
            "alt" => {
                if !self.active_modifiers.contains(&"alt".to_string()) {
                    self.active_modifiers.push("alt".to_string());
                }
            }
            "meta" => {
                if !self.active_modifiers.contains(&"meta".to_string()) {
                    self.active_modifiers.push("meta".to_string());
                }
            }
            "backspace" => {
                self.input_buffer.pop();
            }
            "enter" => {
                events.push(KeyboardEvent::TextInput {
                    text: "\n".to_string(),
                });
                self.input_buffer.push('\n');
            }
            "space" => {
                let ch = ' ';
                self.input_buffer.push(ch);
                events.push(KeyboardEvent::TextInput {
                    text: ch.to_string(),
                });
            }
            "tab" => {
                self.input_buffer.push('\t');
                events.push(KeyboardEvent::TextInput {
                    text: "\t".to_string(),
                });
            }
            _ => {
                // Regular character key
                let ch = if key_code.len() == 1 {
                    let c = key_code.chars().next().unwrap();
                    if self.shift_active || self.caps_lock {
                        let upper = c.to_uppercase().next().unwrap_or(c);
                        // Consume one-shot shift
                        if self.shift_active && !self.caps_lock {
                            self.shift_active = false;
                            self.active_modifiers.retain(|m| m != "shift");
                        }
                        upper
                    } else {
                        c
                    }
                } else {
                    // Multi-char key codes (e.g. function keys) — emit as-is
                    let text = key_code.to_string();
                    self.input_buffer.push_str(&text);
                    events.push(KeyboardEvent::TextInput { text });
                    return events;
                };

                self.input_buffer.push(ch);
                events.push(KeyboardEvent::TextInput {
                    text: ch.to_string(),
                });
            }
        }

        events
    }

    /// Process a key release and return events.
    pub fn release_key(&mut self, key_code: &str) -> Vec<KeyboardEvent> {
        let mut events = Vec::new();

        events.push(KeyboardEvent::KeyUp {
            key: key_code.to_string(),
        });

        // Release modifier if it was held
        match key_code {
            "ctrl" | "alt" | "meta" => {
                self.active_modifiers.retain(|m| m != key_code);
            }
            _ => {}
        }

        events
    }

    /// Flush and return the input buffer contents.
    pub fn commit_text(&mut self) -> Option<String> {
        if self.input_buffer.is_empty() {
            return None;
        }
        let text = self.input_buffer.clone();
        self.input_buffer.clear();
        Some(text)
    }

    /// Switch to a different keyboard layout.
    pub fn set_layout(&mut self, layout: KeyboardLayout) {
        self.config.layout = layout;
        self.keys = generate_layout(layout, self.config.key_size_mm);
        debug!("Virtual keyboard layout changed to {:?}", layout);
    }

    /// IPC adapter: switch layout by name string.
    ///
    /// Accepts "qwerty", "dvorak", or "colemak".
    /// Returns `Err` with a message if the layout name is unrecognized.
    pub fn set_layout_by_name(&mut self, name: &str) -> Result<(), String> {
        let layout = match name {
            "qwerty" => KeyboardLayout::Qwerty,
            "dvorak" => KeyboardLayout::Dvorak,
            "colemak" => KeyboardLayout::Colemak,
            other => return Err(format!("unknown layout: {}", other)),
        };
        self.set_layout(layout);
        Ok(())
    }

    /// Generate s-expression for IPC status.
    pub fn status_sexp(&self) -> String {
        let mods = if self.active_modifiers.is_empty() {
            "nil".to_string()
        } else {
            format!(
                "({})",
                self.active_modifiers
                    .iter()
                    .map(|m| format!("\"{}\"", m))
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        };
        format!(
            "(:visible {} :layout {} :key-count {} :modifiers {} :shift {} :caps-lock {} :buffer-len {} :haptic {} :auto-capitalize {} :prediction {})",
            if self.visible { "t" } else { "nil" },
            self.config.layout.as_str(),
            self.keys.len(),
            mods,
            if self.shift_active { "t" } else { "nil" },
            if self.caps_lock { "t" } else { "nil" },
            self.input_buffer.len(),
            if self.config.haptic_on_press { "t" } else { "nil" },
            if self.config.auto_capitalize { "t" } else { "nil" },
            if self.config.prediction { "t" } else { "nil" },
        )
    }
}

// ── Layout generation ──────────────────────────────────────

/// Generate key definitions for a given layout.
///
/// Keys are positioned on a 2D plane with coordinates in millimeters.
/// Standard key size is `key_size_mm`; special keys get larger widths.
pub fn generate_layout(layout: KeyboardLayout, key_size: f32) -> Vec<KeyDef> {
    let rows = match layout {
        KeyboardLayout::Qwerty => vec![
            // Row 0: number row
            vec![
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
            ],
            // Row 1
            vec!["q", "w", "e", "r", "t", "y", "u", "i", "o", "p"],
            // Row 2
            vec!["a", "s", "d", "f", "g", "h", "j", "k", "l"],
            // Row 3
            vec!["z", "x", "c", "v", "b", "n", "m"],
        ],
        KeyboardLayout::Dvorak => vec![
            vec![
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
            ],
            vec![
                "\"", ",", ".", "p", "y", "f", "g", "c", "r", "l",
            ],
            vec!["a", "o", "e", "u", "i", "d", "h", "t", "n"],
            vec!["q", "j", "k", "x", "b", "m", "w"],
        ],
        KeyboardLayout::Colemak => vec![
            vec![
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
            ],
            vec!["q", "w", "f", "p", "g", "j", "l", "u", "y"],
            vec!["a", "r", "s", "t", "d", "h", "n", "e", "i", "o"],
            vec!["z", "x", "c", "v", "b", "k", "m"],
        ],
    };

    let gap = key_size * 0.1; // 10% gap between keys
    let mut keys = Vec::new();

    for (row_idx, row) in rows.iter().enumerate() {
        let y = row_idx as f32 * (key_size + gap);
        // Stagger rows slightly (like a real keyboard)
        let x_offset = match row_idx {
            1 => key_size * 0.25,
            2 => key_size * 0.5,
            3 => key_size * 0.75,
            _ => 0.0,
        };

        for (col_idx, key_str) in row.iter().enumerate() {
            let x = x_offset + col_idx as f32 * (key_size + gap);
            keys.push(KeyDef {
                label: key_str.to_uppercase(),
                key_code: key_str.to_string(),
                x,
                y,
                width: key_size,
                height: key_size,
                is_modifier: false,
            });
        }
    }

    // Add special keys on the bottom row
    let bottom_y = rows.len() as f32 * (key_size + gap);
    let special_keys = vec![
        ("Shift", "shift", 0.0, key_size * 1.5, true),
        ("Space", "space", key_size * 1.5 + gap, key_size * 5.0, false),
        (
            "Bksp",
            "backspace",
            key_size * 6.5 + gap * 2.0,
            key_size * 1.5,
            false,
        ),
        (
            "Enter",
            "enter",
            key_size * 8.0 + gap * 3.0,
            key_size * 1.5,
            false,
        ),
    ];

    for (label, code, x, width, is_mod) in special_keys {
        keys.push(KeyDef {
            label: label.to_string(),
            key_code: code.to_string(),
            x,
            y: bottom_y,
            width,
            height: key_size,
            is_modifier: is_mod,
        });
    }

    // Add modifier keys row
    let mod_y = (rows.len() as f32 + 1.0) * (key_size + gap);
    let mod_keys = vec![
        ("Ctrl", "ctrl", 0.0, key_size * 1.2),
        ("Alt", "alt", key_size * 1.2 + gap, key_size * 1.2),
        ("Meta", "meta", key_size * 2.4 + gap * 2.0, key_size * 1.2),
        ("Caps", "caps", key_size * 3.6 + gap * 3.0, key_size * 1.2),
        ("Tab", "tab", key_size * 4.8 + gap * 4.0, key_size * 1.2),
    ];

    for (label, code, x, width) in mod_keys {
        keys.push(KeyDef {
            label: label.to_string(),
            key_code: code.to_string(),
            x,
            y: mod_y,
            width,
            height: key_size,
            is_modifier: code == "ctrl" || code == "alt" || code == "meta" || code == "shift",
        });
    }

    keys
}

// ── Tests ──────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_state() {
        let state = VirtualKeyboardState::new();
        assert!(!state.visible);
        assert!(!state.keys.is_empty());
        assert!(state.input_buffer.is_empty());
        assert!(state.active_modifiers.is_empty());
        assert_eq!(state.config.layout, KeyboardLayout::Qwerty);
    }

    #[test]
    fn test_show_hide_toggle() {
        let mut state = VirtualKeyboardState::new();
        assert!(!state.visible);

        state.show();
        assert!(state.visible);

        state.hide();
        assert!(!state.visible);

        state.toggle();
        assert!(state.visible);

        state.toggle();
        assert!(!state.visible);
    }

    #[test]
    fn test_press_regular_key() {
        let mut state = VirtualKeyboardState::new();
        let events = state.press_key("a");

        assert_eq!(events.len(), 2); // KeyDown + TextInput
        assert!(matches!(&events[0], KeyboardEvent::KeyDown { key } if key == "a"));
        assert!(matches!(&events[1], KeyboardEvent::TextInput { text } if text == "a"));
        assert_eq!(state.input_buffer, "a");
    }

    #[test]
    fn test_shift_capitalizes() {
        let mut state = VirtualKeyboardState::new();
        state.press_key("shift");
        assert!(state.shift_active);

        let events = state.press_key("a");
        assert!(
            events.iter().any(|e| matches!(e, KeyboardEvent::TextInput { text } if text == "A")),
            "Expected uppercase A, got {:?}",
            events,
        );
        // Shift should be consumed (one-shot)
        assert!(!state.shift_active);
        assert_eq!(state.input_buffer, "A");
    }

    #[test]
    fn test_caps_lock() {
        let mut state = VirtualKeyboardState::new();
        state.press_key("caps");
        assert!(state.caps_lock);

        let events = state.press_key("b");
        assert!(
            events.iter().any(|e| matches!(e, KeyboardEvent::TextInput { text } if text == "B")),
        );

        // Caps lock stays on
        let events = state.press_key("c");
        assert!(
            events.iter().any(|e| matches!(e, KeyboardEvent::TextInput { text } if text == "C")),
        );
        assert_eq!(state.input_buffer, "BC");

        // Toggle caps lock off
        state.press_key("caps");
        assert!(!state.caps_lock);
    }

    #[test]
    fn test_backspace() {
        let mut state = VirtualKeyboardState::new();
        state.press_key("h");
        state.press_key("i");
        assert_eq!(state.input_buffer, "hi");

        state.press_key("backspace");
        assert_eq!(state.input_buffer, "h");
    }

    #[test]
    fn test_space_and_enter() {
        let mut state = VirtualKeyboardState::new();
        state.press_key("a");
        state.press_key("space");
        state.press_key("b");
        state.press_key("enter");
        assert_eq!(state.input_buffer, "a b\n");
    }

    #[test]
    fn test_commit_text() {
        let mut state = VirtualKeyboardState::new();
        state.press_key("h");
        state.press_key("i");

        let text = state.commit_text();
        assert_eq!(text, Some("hi".to_string()));
        assert!(state.input_buffer.is_empty());

        // Commit again — empty
        assert!(state.commit_text().is_none());
    }

    #[test]
    fn test_hit_test() {
        let state = VirtualKeyboardState::new();
        // First key should be at (0, 0) with size 20x20
        let first_key = &state.keys[0];
        let hit = state.hit_test(first_key.x + 1.0, first_key.y + 1.0);
        assert!(hit.is_some());
        assert_eq!(hit.unwrap().key_code, first_key.key_code);

        // Way outside the keyboard
        let miss = state.hit_test(-100.0, -100.0);
        assert!(miss.is_none());
    }

    #[test]
    fn test_release_key_modifier() {
        let mut state = VirtualKeyboardState::new();
        state.press_key("ctrl");
        assert!(state.active_modifiers.contains(&"ctrl".to_string()));

        let events = state.release_key("ctrl");
        assert!(matches!(&events[0], KeyboardEvent::KeyUp { key } if key == "ctrl"));
        assert!(!state.active_modifiers.contains(&"ctrl".to_string()));
    }

    #[test]
    fn test_set_layout() {
        let mut state = VirtualKeyboardState::new();
        let qwerty_count = state.keys.len();

        state.set_layout(KeyboardLayout::Dvorak);
        assert_eq!(state.config.layout, KeyboardLayout::Dvorak);
        // Should have roughly same number of keys
        assert!(state.keys.len() > 0);

        state.set_layout(KeyboardLayout::Colemak);
        assert_eq!(state.config.layout, KeyboardLayout::Colemak);
        assert!(state.keys.len() > 0);

        // Restore
        state.set_layout(KeyboardLayout::Qwerty);
        assert_eq!(state.keys.len(), qwerty_count);
    }

    #[test]
    fn test_generate_layout_has_all_keys() {
        let keys = generate_layout(KeyboardLayout::Qwerty, 20.0);
        // 10 + 10 + 9 + 7 = 36 alpha/num keys + special + modifier = 45
        assert!(
            keys.len() >= 36,
            "Expected at least 36 keys, got {}",
            keys.len()
        );

        // Check some specific keys exist
        let has_q = keys.iter().any(|k| k.key_code == "q");
        let has_space = keys.iter().any(|k| k.key_code == "space");
        let has_shift = keys.iter().any(|k| k.key_code == "shift");
        assert!(has_q, "Missing 'q' key");
        assert!(has_space, "Missing 'space' key");
        assert!(has_shift, "Missing 'shift' key");
    }

    #[test]
    fn test_status_sexp() {
        let state = VirtualKeyboardState::new();
        let sexp = state.status_sexp();
        assert!(sexp.contains(":visible nil"));
        assert!(sexp.contains(":layout qwerty"));
        assert!(sexp.contains(":modifiers nil"));
        assert!(sexp.contains(":shift nil"));
        assert!(sexp.contains(":caps-lock nil"));
        assert!(sexp.contains(":buffer-len 0"));
    }

    #[test]
    fn test_status_sexp_with_modifiers() {
        let mut state = VirtualKeyboardState::new();
        state.press_key("ctrl");
        state.press_key("alt");

        let sexp = state.status_sexp();
        assert!(sexp.contains(":modifiers (\"ctrl\" \"alt\")"));
    }

    #[test]
    fn test_layout_as_str() {
        assert_eq!(KeyboardLayout::Qwerty.as_str(), "qwerty");
        assert_eq!(KeyboardLayout::Dvorak.as_str(), "dvorak");
        assert_eq!(KeyboardLayout::Colemak.as_str(), "colemak");
    }

    #[test]
    fn test_key_contains() {
        let key = KeyDef {
            label: "A".to_string(),
            key_code: "a".to_string(),
            x: 10.0,
            y: 20.0,
            width: 20.0,
            height: 20.0,
            is_modifier: false,
        };
        assert!(key.contains(15.0, 25.0)); // center
        assert!(key.contains(10.0, 20.0)); // top-left corner
        assert!(key.contains(30.0, 40.0)); // bottom-right corner
        assert!(!key.contains(9.0, 25.0)); // just outside left
        assert!(!key.contains(31.0, 25.0)); // just outside right
    }
}
