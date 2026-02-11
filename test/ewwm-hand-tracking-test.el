;;; ewwm-hand-tracking-test.el --- Tests for hand tracking module  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-vr-hand)

;; Forward-declare functions from module under test
(declare-function ewwm-vr-hand--on-tracking-started "ewwm-vr-hand")
(declare-function ewwm-vr-hand--on-tracking-lost "ewwm-vr-hand")
(declare-function ewwm-vr-hand--on-confidence-update "ewwm-vr-hand")
(declare-function ewwm-vr-hand-mode-line-string "ewwm-vr-hand")
(declare-function ewwm-vr-hand-status "ewwm-vr-hand")
(declare-function ewwm-vr-hand-toggle "ewwm-vr-hand")
(declare-function ewwm-vr-hand-configure "ewwm-vr-hand")
(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-vr-hand-enable)
(defvar ewwm-vr-hand-min-confidence)
(defvar ewwm-vr-hand-smoothing)
(defvar ewwm-vr-hand-prediction-ms)
(defvar ewwm-vr-hand-dominant)
(defvar ewwm-vr-hand-show-skeleton)
(defvar ewwm-vr-hand--left-active)
(defvar ewwm-vr-hand--right-active)
(defvar ewwm-vr-hand--left-confidence)
(defvar ewwm-vr-hand--right-confidence)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest ewwm-hand/provides-feature ()
  "ewwm-vr-hand provides its feature."
  (should (featurep 'ewwm-vr-hand)))

(ert-deftest ewwm-hand/group-exists ()
  "ewwm-vr-hand customization group exists."
  (should (get 'ewwm-vr-hand 'custom-group)))

;; ── Defcustom defaults ──────────────────────────────────────

(ert-deftest ewwm-hand/enable-default ()
  "Default enable is t."
  (should (eq (default-value 'ewwm-vr-hand-enable) t)))

(ert-deftest ewwm-hand/min-confidence-default ()
  "Default min-confidence is 0.5."
  (should (= (default-value 'ewwm-vr-hand-min-confidence) 0.5)))

(ert-deftest ewwm-hand/smoothing-default ()
  "Default smoothing is 0.3."
  (should (= (default-value 'ewwm-vr-hand-smoothing) 0.3)))

(ert-deftest ewwm-hand/prediction-ms-default ()
  "Default prediction-ms is 20.0."
  (should (= (default-value 'ewwm-vr-hand-prediction-ms) 20.0)))

(ert-deftest ewwm-hand/dominant-default ()
  "Default dominant hand is right."
  (should (eq (default-value 'ewwm-vr-hand-dominant) 'right)))

;; ── Function existence ──────────────────────────────────────

(ert-deftest ewwm-hand/status-exists ()
  "ewwm-vr-hand-status is defined."
  (should (fboundp 'ewwm-vr-hand-status)))

(ert-deftest ewwm-hand/toggle-exists ()
  "ewwm-vr-hand-toggle is defined."
  (should (fboundp 'ewwm-vr-hand-toggle)))

(ert-deftest ewwm-hand/configure-exists ()
  "ewwm-vr-hand-configure is defined."
  (should (fboundp 'ewwm-vr-hand-configure)))

(ert-deftest ewwm-hand/mode-line-string-exists ()
  "ewwm-vr-hand-mode-line-string is defined."
  (should (fboundp 'ewwm-vr-hand-mode-line-string)))

;; ── Hand tracking state management ──────────────────────────

(ert-deftest ewwm-hand/on-tracking-started-left ()
  "on-tracking-started sets left hand active."
  (let ((ewwm-vr-hand--left-active nil)
        (ewwm-vr-hand--right-active nil))
    (ewwm-vr-hand--on-tracking-started '(:hand left))
    (should ewwm-vr-hand--left-active)
    (should-not ewwm-vr-hand--right-active)))

(ert-deftest ewwm-hand/on-tracking-started-right ()
  "on-tracking-started sets right hand active."
  (let ((ewwm-vr-hand--left-active nil)
        (ewwm-vr-hand--right-active nil))
    (ewwm-vr-hand--on-tracking-started '(:hand right))
    (should-not ewwm-vr-hand--left-active)
    (should ewwm-vr-hand--right-active)))

(ert-deftest ewwm-hand/on-tracking-lost-left ()
  "on-tracking-lost clears left hand active."
  (let ((ewwm-vr-hand--left-active t)
        (ewwm-vr-hand--left-confidence 0.9)
        (ewwm-vr-hand--right-active t)
        (ewwm-vr-hand--right-confidence 0.8))
    (ewwm-vr-hand--on-tracking-lost '(:hand left))
    (should-not ewwm-vr-hand--left-active)
    (should (= ewwm-vr-hand--left-confidence 0.0))
    ;; Right hand should be unaffected
    (should ewwm-vr-hand--right-active)))

(ert-deftest ewwm-hand/on-tracking-lost-right ()
  "on-tracking-lost clears right hand active."
  (let ((ewwm-vr-hand--left-active t)
        (ewwm-vr-hand--right-active t)
        (ewwm-vr-hand--right-confidence 0.9)
        (ewwm-vr-hand--left-confidence 0.8))
    (ewwm-vr-hand--on-tracking-lost '(:hand right))
    (should-not ewwm-vr-hand--right-active)
    (should (= ewwm-vr-hand--right-confidence 0.0))
    (should ewwm-vr-hand--left-active)))

(ert-deftest ewwm-hand/on-confidence-update-left ()
  "on-confidence-update updates left hand confidence."
  (let ((ewwm-vr-hand--left-confidence 0.0)
        (ewwm-vr-hand--right-confidence 0.0))
    (ewwm-vr-hand--on-confidence-update '(:hand left :confidence 0.85))
    (should (= ewwm-vr-hand--left-confidence 0.85))
    (should (= ewwm-vr-hand--right-confidence 0.0))))

(ert-deftest ewwm-hand/on-confidence-update-right ()
  "on-confidence-update updates right hand confidence."
  (let ((ewwm-vr-hand--left-confidence 0.0)
        (ewwm-vr-hand--right-confidence 0.0))
    (ewwm-vr-hand--on-confidence-update '(:hand right :confidence 0.72))
    (should (= ewwm-vr-hand--right-confidence 0.72))
    (should (= ewwm-vr-hand--left-confidence 0.0))))

(ert-deftest ewwm-hand/on-confidence-update-nil-ignored ()
  "on-confidence-update ignores nil confidence."
  (let ((ewwm-vr-hand--left-confidence 0.5))
    (ewwm-vr-hand--on-confidence-update '(:hand left :confidence nil))
    (should (= ewwm-vr-hand--left-confidence 0.5))))

;; ── Mode-line string ────────────────────────────────────────

(ert-deftest ewwm-hand/mode-line-disabled ()
  "Mode-line returns nil when disabled."
  (let ((ewwm-vr-hand-enable nil))
    (should-not (ewwm-vr-hand-mode-line-string))))

(ert-deftest ewwm-hand/mode-line-both-hands ()
  "Mode-line returns \" [H:L+R]\" when both hands tracked."
  (let ((ewwm-vr-hand-enable t)
        (ewwm-vr-hand--left-active t)
        (ewwm-vr-hand--right-active t))
    (should (equal (ewwm-vr-hand-mode-line-string) " [H:L+R]"))))

(ert-deftest ewwm-hand/mode-line-left-only ()
  "Mode-line returns \" [H:L]\" when only left tracked."
  (let ((ewwm-vr-hand-enable t)
        (ewwm-vr-hand--left-active t)
        (ewwm-vr-hand--right-active nil))
    (should (equal (ewwm-vr-hand-mode-line-string) " [H:L]"))))

(ert-deftest ewwm-hand/mode-line-right-only ()
  "Mode-line returns \" [H:R]\" when only right tracked."
  (let ((ewwm-vr-hand-enable t)
        (ewwm-vr-hand--left-active nil)
        (ewwm-vr-hand--right-active t))
    (should (equal (ewwm-vr-hand-mode-line-string) " [H:R]"))))

(ert-deftest ewwm-hand/mode-line-no-hands ()
  "Mode-line returns \" [H:-]\" when enabled but no hands."
  (let ((ewwm-vr-hand-enable t)
        (ewwm-vr-hand--left-active nil)
        (ewwm-vr-hand--right-active nil))
    (should (equal (ewwm-vr-hand-mode-line-string) " [H:-]"))))

;; ── Interactive command checks ──────────────────────────────

(ert-deftest ewwm-hand/status-interactive ()
  "ewwm-vr-hand-status is interactive."
  (should (commandp 'ewwm-vr-hand-status)))

(ert-deftest ewwm-hand/toggle-interactive ()
  "ewwm-vr-hand-toggle is interactive."
  (should (commandp 'ewwm-vr-hand-toggle)))

(ert-deftest ewwm-hand/configure-interactive ()
  "ewwm-vr-hand-configure is interactive."
  (should (commandp 'ewwm-vr-hand-configure)))

;; ── Toggle behavior ─────────────────────────────────────────

(ert-deftest ewwm-hand/toggle-flips-enable ()
  "Toggle flips the enable flag."
  (let ((ewwm-vr-hand-enable t))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p) (lambda () nil)))
      (ewwm-vr-hand-toggle)
      (should-not ewwm-vr-hand-enable)
      (ewwm-vr-hand-toggle)
      (should ewwm-vr-hand-enable))))

;; ── Configure sends IPC ─────────────────────────────────────

(ert-deftest ewwm-hand/configure-sends-ipc ()
  "configure sends IPC when connected."
  (let ((ewwm-vr-hand-enable t)
        (ewwm-vr-hand-min-confidence 0.5)
        (ewwm-vr-hand-smoothing 0.3)
        (ewwm-vr-hand-prediction-ms 20.0)
        (ewwm-vr-hand-show-skeleton nil)
        (ewwm-vr-hand-dominant 'right)
        (sent-msgs nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p) (lambda () t))
              ((symbol-function 'ewwm-ipc-send)
               (lambda (msg) (push msg sent-msgs))))
      (ewwm-vr-hand-configure)
      (should (= (length sent-msgs) 1))
      (should (eq (plist-get (car sent-msgs) :type) :hand-tracking-configure)))))

;;; ewwm-hand-tracking-test.el ends here
