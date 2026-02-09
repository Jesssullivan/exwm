;;; ewwm-vr-wink-test.el --- Week 13 wink tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-wink)

;; Forward-declare so `let' creates dynamic binding (needed for `boundp')
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-vr-wink/enable-defcustom ()
  "Default wink enable is t."
  (should (eq (default-value 'ewwm-vr-wink-enable) t)))

(ert-deftest ewwm-vr-wink/left-action-defcustom ()
  "Default left action is previous-buffer."
  (should (eq (default-value 'ewwm-vr-wink-left-action) #'previous-buffer)))

(ert-deftest ewwm-vr-wink/right-action-defcustom ()
  "Default right action is next-buffer."
  (should (eq (default-value 'ewwm-vr-wink-right-action) #'next-buffer)))

(ert-deftest ewwm-vr-wink/double-left-action-defcustom ()
  "Default double-left action is mode-line-other-buffer."
  (should (eq (default-value 'ewwm-vr-wink-double-left-action) #'mode-line-other-buffer)))

(ert-deftest ewwm-vr-wink/double-right-action-defcustom ()
  "Default double-right action is delete-window."
  (should (eq (default-value 'ewwm-vr-wink-double-right-action) #'delete-window)))

(ert-deftest ewwm-vr-wink/confidence-min-defcustom ()
  "Default blink confidence min is 0.7."
  (should (= (default-value 'ewwm-vr-blink-confidence-min) 0.7)))

(ert-deftest ewwm-vr-wink/feedback-defcustom ()
  "Default wink feedback is t."
  (should (eq (default-value 'ewwm-vr-wink-feedback) t)))

(ert-deftest ewwm-vr-wink/sequences-defcustom ()
  "Default wink sequences is nil."
  (should (null (default-value 'ewwm-vr-wink-sequences))))

(ert-deftest ewwm-vr-wink/sequence-timeout-defcustom ()
  "Default sequence timeout is 1500."
  (should (= (default-value 'ewwm-vr-wink-sequence-timeout-ms) 1500)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-vr-wink/last-wink-exists ()
  "ewwm-vr-wink--last-wink variable exists."
  (should (boundp 'ewwm-vr-wink--last-wink)))

(ert-deftest ewwm-vr-wink/blink-count-exists ()
  "ewwm-vr-wink--blink-count variable exists."
  (should (boundp 'ewwm-vr-wink--blink-count)))

(ert-deftest ewwm-vr-wink/blink-rate-exists ()
  "ewwm-vr-wink--blink-rate variable exists."
  (should (boundp 'ewwm-vr-wink--blink-rate)))

(ert-deftest ewwm-vr-wink/wink-history-exists ()
  "ewwm-vr-wink--wink-history variable exists."
  (should (boundp 'ewwm-vr-wink--wink-history)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-vr-wink/calibrate-interactive ()
  "ewwm-vr-wink-calibrate is interactive."
  (should (commandp 'ewwm-vr-wink-calibrate)))

(ert-deftest ewwm-vr-wink/status-interactive ()
  "ewwm-vr-wink-status is interactive."
  (should (commandp 'ewwm-vr-wink-status)))

(ert-deftest ewwm-vr-wink/set-actions-interactive ()
  "ewwm-vr-wink-set-actions is interactive."
  (should (commandp 'ewwm-vr-wink-set-actions)))

;; ── IPC event handlers ──────────────────────────────────────

(ert-deftest ewwm-vr-wink/blink-event-updates-count ()
  "Blink event increments blink count."
  (let ((ewwm-vr-wink--blink-count 0)
        (ewwm-vr-wink--blink-rate 0.0)
        (ewwm-vr-blink-confidence-min 0.5)
        (ewwm-vr-blink-hook nil))
    (ewwm-vr-wink--on-blink '(:eye left :duration-ms 150 :confidence 0.8 :rate 12.0))
    (should (= ewwm-vr-wink--blink-count 1))
    (should (= ewwm-vr-wink--blink-rate 12.0))))

(ert-deftest ewwm-vr-wink/blink-event-low-confidence ()
  "Blink event below confidence threshold is ignored."
  (let ((ewwm-vr-wink--blink-count 0)
        (ewwm-vr-blink-confidence-min 0.9)
        (ewwm-vr-blink-hook nil))
    (ewwm-vr-wink--on-blink '(:eye left :duration-ms 150 :confidence 0.5))
    (should (= ewwm-vr-wink--blink-count 0))))

(ert-deftest ewwm-vr-wink/blink-hook-runs ()
  "Blink event runs ewwm-vr-blink-hook."
  (let ((ewwm-vr-wink--blink-count 0)
        (ewwm-vr-blink-confidence-min 0.5)
        (hook-args nil))
    (let ((ewwm-vr-blink-hook
           (list (lambda (eye dur conf) (setq hook-args (list eye dur conf))))))
      (ewwm-vr-wink--on-blink '(:eye left :duration-ms 150 :confidence 0.8))
      (should (equal hook-args '(left 150 0.8))))))

(ert-deftest ewwm-vr-wink/wink-event-dispatches ()
  "Wink event dispatches correct action."
  (let ((ewwm-vr-wink-enable t)
        (ewwm-vr-wink--last-wink nil)
        (ewwm-vr-wink--last-wink-time nil)
        (ewwm-vr-wink--wink-history (make-ring 20))
        (ewwm-vr-wink--sequence-acc nil)
        (ewwm-vr-wink--sequence-start-time nil)
        (ewwm-vr-blink-confidence-min 0.5)
        (ewwm-vr-wink-sequences nil)
        (ewwm-vr-wink-feedback nil)
        (called nil))
    (let ((ewwm-vr-wink-left-action (lambda () (setq called 'left)))
          (ewwm-vr-wink-hook nil))
      (ewwm-vr-wink--on-wink '(:side left :confidence 0.9))
      (should (eq called 'left))
      (should (eq ewwm-vr-wink--last-wink 'left)))))

(ert-deftest ewwm-vr-wink/wink-disabled ()
  "Wink event is ignored when disabled."
  (let ((ewwm-vr-wink-enable nil)
        (ewwm-vr-wink--last-wink nil)
        (ewwm-vr-wink--wink-history (make-ring 20)))
    (ewwm-vr-wink--on-wink '(:side left :confidence 0.9))
    (should-not ewwm-vr-wink--last-wink)))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-vr-wink/register-events ()
  "ewwm-vr-wink--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-wink--register-events)
    (should (assq :blink ewwm-ipc--event-handlers))
    (should (assq :wink ewwm-ipc--event-handlers))
    (should (assq :wink-calibration-result ewwm-ipc--event-handlers))))

(ert-deftest ewwm-vr-wink/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-wink--register-events)
    (ewwm-vr-wink--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :blink))
                        ewwm-ipc--event-handlers))
               1))))

;; ── Minor mode ──────────────────────────────────────────────

(ert-deftest ewwm-vr-wink/minor-mode-exists ()
  "ewwm-vr-wink-mode minor mode exists."
  (should (fboundp 'ewwm-vr-wink-mode)))

;; ── Mode-line ───────────────────────────────────────────────

(ert-deftest ewwm-vr-wink/mode-line-no-wink ()
  "Mode-line nil when no recent wink."
  (let ((ewwm-vr-wink-feedback t)
        (ewwm-vr-wink--last-wink nil)
        (ewwm-vr-wink--last-wink-time nil)
        (ewwm-vr-wink--blink-rate 0.0))
    (should-not (ewwm-vr-wink-mode-line-string))))

(ert-deftest ewwm-vr-wink/mode-line-recent-wink ()
  "Mode-line shows wink when recent."
  (let ((ewwm-vr-wink-feedback t)
        (ewwm-vr-wink--last-wink 'left)
        (ewwm-vr-wink--last-wink-time (current-time))
        (ewwm-vr-wink--blink-rate 0.0))
    (should (string= (ewwm-vr-wink-mode-line-string) " [Wink:L]"))))

(ert-deftest ewwm-vr-wink/mode-line-with-rate ()
  "Mode-line shows wink and blink rate."
  (let ((ewwm-vr-wink-feedback t)
        (ewwm-vr-wink--last-wink 'right)
        (ewwm-vr-wink--last-wink-time (current-time))
        (ewwm-vr-wink--blink-rate 15.0))
    (should (string= (ewwm-vr-wink-mode-line-string) " [Wink:R 15bpm]"))))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-vr-wink/teardown-clears-state ()
  "ewwm-vr-wink-teardown clears all state."
  (let ((ewwm-vr-wink--last-wink 'left)
        (ewwm-vr-wink--last-wink-time (current-time))
        (ewwm-vr-wink--blink-count 42)
        (ewwm-vr-wink--blink-rate 15.0)
        (ewwm-vr-wink--sequence-acc '(:left))
        (ewwm-vr-wink--sequence-start-time (current-time))
        (ewwm-vr-wink--calibrating t)
        (ewwm-vr-wink--calibration-phase 'left)
        (ewwm-vr-wink--calibration-trials '(100 200))
        (ewwm-vr-wink--wink-history (make-ring 20)))
    (ewwm-vr-wink-teardown)
    (should-not ewwm-vr-wink--last-wink)
    (should-not ewwm-vr-wink--last-wink-time)
    (should (= ewwm-vr-wink--blink-count 0))
    (should (= ewwm-vr-wink--blink-rate 0.0))
    (should-not ewwm-vr-wink--sequence-acc)
    (should-not ewwm-vr-wink--calibrating)
    (should-not ewwm-vr-wink--calibration-phase)
    (should-not ewwm-vr-wink--calibration-trials)))

;; ── Hooks exist ─────────────────────────────────────────────

(ert-deftest ewwm-vr-wink/wink-hook-exists ()
  "ewwm-vr-wink-hook exists."
  (should (boundp 'ewwm-vr-wink-hook)))

(ert-deftest ewwm-vr-wink/blink-hook-exists ()
  "ewwm-vr-blink-hook exists."
  (should (boundp 'ewwm-vr-blink-hook)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-vr-wink/provides-feature ()
  "ewwm-vr-wink provides its feature."
  (should (featurep 'ewwm-vr-wink)))

;;; ewwm-vr-wink-test.el ends here
