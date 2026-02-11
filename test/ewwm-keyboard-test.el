;;; ewwm-keyboard-test.el --- Tests for VR virtual keyboard module  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-vr-keyboard)

;; Forward-declare functions from module under test
(declare-function ewwm-vr-keyboard--insert-text "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard--handle-special "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard--on-text-input "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard--on-visibility-changed "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard--on-layout-changed "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard-mode-line-string "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard-show "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard-hide "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard-toggle "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard-set-layout "ewwm-vr-keyboard")
(declare-function ewwm-vr-keyboard-status "ewwm-vr-keyboard")
(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-vr-keyboard-layout)
(defvar ewwm-vr-keyboard-key-size)
(defvar ewwm-vr-keyboard-haptic)
(defvar ewwm-vr-keyboard-auto-show)
(defvar ewwm-vr-keyboard-auto-capitalize)
(defvar ewwm-vr-keyboard-prediction)
(defvar ewwm-vr-keyboard--visible)
(defvar ewwm-vr-keyboard--current-layout)
(defvar ewwm-vr-keyboard-show-hook)
(defvar ewwm-vr-keyboard-hide-hook)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest ewwm-keyboard/provides-feature ()
  "ewwm-vr-keyboard provides its feature."
  (should (featurep 'ewwm-vr-keyboard)))

(ert-deftest ewwm-keyboard/group-exists ()
  "ewwm-vr-keyboard customization group exists."
  (should (get 'ewwm-vr-keyboard 'custom-group)))

;; ── Defcustom defaults ──────────────────────────────────────

(ert-deftest ewwm-keyboard/layout-default ()
  "Default layout is qwerty."
  (should (eq (default-value 'ewwm-vr-keyboard-layout) 'qwerty)))

(ert-deftest ewwm-keyboard/key-size-default ()
  "Default key-size is 0.03."
  (should (= (default-value 'ewwm-vr-keyboard-key-size) 0.03)))

(ert-deftest ewwm-keyboard/haptic-default ()
  "Default haptic is t."
  (should (eq (default-value 'ewwm-vr-keyboard-haptic) t)))

(ert-deftest ewwm-keyboard/auto-show-default ()
  "Default auto-show is nil."
  (should-not (default-value 'ewwm-vr-keyboard-auto-show)))

(ert-deftest ewwm-keyboard/auto-capitalize-default ()
  "Default auto-capitalize is t."
  (should (eq (default-value 'ewwm-vr-keyboard-auto-capitalize) t)))

(ert-deftest ewwm-keyboard/prediction-default ()
  "Default prediction is nil."
  (should-not (default-value 'ewwm-vr-keyboard-prediction)))

;; ── Visibility state ────────────────────────────────────────

(ert-deftest ewwm-keyboard/show-sets-visible ()
  "show sets visible to t."
  (let ((ewwm-vr-keyboard--visible nil)
        (ewwm-vr-keyboard-show-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p) (lambda () nil)))
      (ewwm-vr-keyboard-show)
      (should ewwm-vr-keyboard--visible))))

(ert-deftest ewwm-keyboard/hide-sets-invisible ()
  "hide sets visible to nil."
  (let ((ewwm-vr-keyboard--visible t)
        (ewwm-vr-keyboard-hide-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p) (lambda () nil)))
      (ewwm-vr-keyboard-hide)
      (should-not ewwm-vr-keyboard--visible))))

(ert-deftest ewwm-keyboard/toggle-flips-visible ()
  "toggle flips visibility."
  (let ((ewwm-vr-keyboard--visible nil)
        (ewwm-vr-keyboard-show-hook nil)
        (ewwm-vr-keyboard-hide-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p) (lambda () nil)))
      (ewwm-vr-keyboard-toggle)
      (should ewwm-vr-keyboard--visible)
      (ewwm-vr-keyboard-toggle)
      (should-not ewwm-vr-keyboard--visible))))

;; ── Text insertion ──────────────────────────────────────────

(ert-deftest ewwm-keyboard/insert-text-at-point ()
  "insert-text inserts at point in a temp buffer."
  (with-temp-buffer
    (ewwm-vr-keyboard--insert-text "hello")
    (should (equal (buffer-string) "hello"))))

(ert-deftest ewwm-keyboard/insert-text-appends ()
  "insert-text appends to existing text."
  (with-temp-buffer
    (insert "foo")
    (ewwm-vr-keyboard--insert-text "bar")
    (should (equal (buffer-string) "foobar"))))

(ert-deftest ewwm-keyboard/insert-text-nil-ignored ()
  "insert-text ignores nil input."
  (with-temp-buffer
    (ewwm-vr-keyboard--insert-text nil)
    (should (equal (buffer-string) ""))))

(ert-deftest ewwm-keyboard/insert-text-empty-ignored ()
  "insert-text ignores empty string."
  (with-temp-buffer
    (ewwm-vr-keyboard--insert-text "")
    (should (equal (buffer-string) ""))))

(ert-deftest ewwm-keyboard/handle-special-backspace ()
  "handle-special backspace deletes one char."
  (with-temp-buffer
    (insert "abc")
    (ewwm-vr-keyboard--handle-special 'backspace)
    (should (equal (buffer-string) "ab"))))

(ert-deftest ewwm-keyboard/handle-special-return ()
  "handle-special return inserts newline."
  (with-temp-buffer
    (insert "line1")
    (ewwm-vr-keyboard--handle-special 'return)
    (should (equal (buffer-string) "line1\n"))))

(ert-deftest ewwm-keyboard/handle-special-tab ()
  "handle-special tab inserts tab character."
  (with-temp-buffer
    (ewwm-vr-keyboard--handle-special 'tab)
    (should (equal (buffer-string) "\t"))))

;; ── Mode-line ───────────────────────────────────────────────

(ert-deftest ewwm-keyboard/mode-line-visible ()
  "Mode-line returns \" [KB]\" when visible."
  (let ((ewwm-vr-keyboard--visible t))
    (should (equal (ewwm-vr-keyboard-mode-line-string) " [KB]"))))

(ert-deftest ewwm-keyboard/mode-line-hidden ()
  "Mode-line returns nil when hidden."
  (let ((ewwm-vr-keyboard--visible nil))
    (should-not (ewwm-vr-keyboard-mode-line-string))))

;; ── Layout change ───────────────────────────────────────────

(ert-deftest ewwm-keyboard/layout-change-updates-state ()
  "Layout change updates current-layout."
  (let ((ewwm-vr-keyboard--current-layout 'qwerty))
    (ewwm-vr-keyboard--on-layout-changed '(:layout "dvorak"))
    (should (eq ewwm-vr-keyboard--current-layout 'dvorak))))

;; ── IPC events ──────────────────────────────────────────────

(ert-deftest ewwm-keyboard/on-text-input-inserts ()
  "on-text-input inserts text into current buffer."
  (with-temp-buffer
    (ewwm-vr-keyboard--on-text-input '(:text "typed"))
    (should (equal (buffer-string) "typed"))))

(ert-deftest ewwm-keyboard/on-visibility-changed-shows ()
  "on-visibility-changed sets visible to t."
  (let ((ewwm-vr-keyboard--visible nil)
        (ewwm-vr-keyboard-show-hook nil)
        (ewwm-vr-keyboard-hide-hook nil))
    (ewwm-vr-keyboard--on-visibility-changed '(:visible t))
    (should ewwm-vr-keyboard--visible)))

(ert-deftest ewwm-keyboard/on-visibility-changed-hides ()
  "on-visibility-changed sets visible to nil."
  (let ((ewwm-vr-keyboard--visible t)
        (ewwm-vr-keyboard-show-hook nil)
        (ewwm-vr-keyboard-hide-hook nil))
    (ewwm-vr-keyboard--on-visibility-changed '(:visible nil))
    (should-not ewwm-vr-keyboard--visible)))

;; ── Interactive command checks ──────────────────────────────

(ert-deftest ewwm-keyboard/show-interactive ()
  "ewwm-vr-keyboard-show is interactive."
  (should (commandp 'ewwm-vr-keyboard-show)))

(ert-deftest ewwm-keyboard/hide-interactive ()
  "ewwm-vr-keyboard-hide is interactive."
  (should (commandp 'ewwm-vr-keyboard-hide)))

(ert-deftest ewwm-keyboard/toggle-interactive ()
  "ewwm-vr-keyboard-toggle is interactive."
  (should (commandp 'ewwm-vr-keyboard-toggle)))

(ert-deftest ewwm-keyboard/set-layout-interactive ()
  "ewwm-vr-keyboard-set-layout is interactive."
  (should (commandp 'ewwm-vr-keyboard-set-layout)))

(ert-deftest ewwm-keyboard/status-interactive ()
  "ewwm-vr-keyboard-status is interactive."
  (should (commandp 'ewwm-vr-keyboard-status)))

;;; ewwm-keyboard-test.el ends here
