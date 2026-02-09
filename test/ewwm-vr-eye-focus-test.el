;;; ewwm-vr-eye-focus-test.el --- Week 12 gaze focus tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-vr-eye)

;; Forward-declare dynamic variables
(defvar ewwm-ipc--event-handlers)

;; ── Focus defcustoms ──────────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/enable-defcustom ()
  "Default enable is t."
  (should (eq (default-value 'ewwm-vr-eye-enable) t)))

(ert-deftest ewwm-vr-eye-focus/policy-defcustom ()
  "Default focus policy is gaze-primary."
  (should (eq (default-value 'ewwm-vr-eye-focus-policy) 'gaze-primary)))

(ert-deftest ewwm-vr-eye-focus/dwell-ms-defcustom ()
  "Default dwell threshold is 200ms."
  (should (= (default-value 'ewwm-vr-eye-dwell-ms) 200)))

(ert-deftest ewwm-vr-eye-focus/cooldown-ms-defcustom ()
  "Default cooldown is 500ms."
  (should (= (default-value 'ewwm-vr-eye-cooldown-ms) 500)))

(ert-deftest ewwm-vr-eye-focus/saccade-threshold-defcustom ()
  "Default saccade threshold is 300 deg/s."
  (should (= (default-value 'ewwm-vr-eye-saccade-threshold) 300)))

(ert-deftest ewwm-vr-eye-focus/max-jitter-defcustom ()
  "Default max jitter is 50px."
  (should (= (default-value 'ewwm-vr-eye-max-jitter-px) 50)))

(ert-deftest ewwm-vr-eye-focus/reading-detection-defcustom ()
  "Default reading detection is t."
  (should (eq (default-value 'ewwm-vr-eye-reading-detection) t)))

(ert-deftest ewwm-vr-eye-focus/hysteresis-defcustom ()
  "Default hysteresis is t."
  (should (eq (default-value 'ewwm-vr-eye-hysteresis) t)))

(ert-deftest ewwm-vr-eye-focus/show-dwell-progress-defcustom ()
  "Default show dwell progress is t."
  (should (eq (default-value 'ewwm-vr-eye-show-dwell-progress) t)))

(ert-deftest ewwm-vr-eye-focus/focus-exceptions-defcustom ()
  "Default focus exceptions is nil."
  (should (null (default-value 'ewwm-vr-eye-focus-exceptions))))

(ert-deftest ewwm-vr-eye-focus/cross-workspace-defcustom ()
  "Default cross-workspace focus is nil."
  (should (null (default-value 'ewwm-vr-eye-cross-workspace-focus))))

(ert-deftest ewwm-vr-eye-focus/fallback-defcustom ()
  "Default fallback is head-gaze."
  (should (eq (default-value 'ewwm-vr-eye-fallback) 'head-gaze)))

(ert-deftest ewwm-vr-eye-focus/analytics-enable-defcustom ()
  "Default analytics enable is t."
  (should (eq (default-value 'ewwm-vr-eye-analytics-enable) t)))

;; ── Focus state variables ─────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/dwell-surface-exists ()
  "Dwell surface variable exists."
  (should (boundp 'ewwm-vr-eye--dwell-surface)))

(ert-deftest ewwm-vr-eye-focus/dwell-progress-exists ()
  "Dwell progress variable exists."
  (should (boundp 'ewwm-vr-eye--dwell-progress)))

(ert-deftest ewwm-vr-eye-focus/cooldown-remaining-exists ()
  "Cooldown remaining variable exists."
  (should (boundp 'ewwm-vr-eye--cooldown-remaining)))

(ert-deftest ewwm-vr-eye-focus/in-saccade-exists ()
  "In-saccade variable exists."
  (should (boundp 'ewwm-vr-eye--in-saccade)))

(ert-deftest ewwm-vr-eye-focus/in-reading-exists ()
  "In-reading variable exists."
  (should (boundp 'ewwm-vr-eye--in-reading)))

(ert-deftest ewwm-vr-eye-focus/focus-ring-exists ()
  "Focus ring variable exists."
  (should (boundp 'ewwm-vr-eye--focus-ring)))

(ert-deftest ewwm-vr-eye-focus/last-focus-method-exists ()
  "Last focus method variable exists."
  (should (boundp 'ewwm-last-focus-method)))

(ert-deftest ewwm-vr-eye-focus/analytics-exists ()
  "Analytics variable exists."
  (should (boundp 'ewwm-vr-eye--analytics)))

;; ── Hooks ─────────────────────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/dwell-hook-exists ()
  "Gaze dwell hook exists."
  (should (boundp 'ewwm-vr-gaze-dwell-hook)))

(ert-deftest ewwm-vr-eye-focus/focus-hook-exists ()
  "Gaze focus hook exists."
  (should (boundp 'ewwm-vr-gaze-focus-hook)))

;; ── Interactive commands ──────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/focus-back-interactive ()
  "ewwm-vr-eye-focus-back is interactive."
  (should (commandp 'ewwm-vr-eye-focus-back)))

(ert-deftest ewwm-vr-eye-focus/set-focus-policy-interactive ()
  "ewwm-vr-eye-set-focus-policy is interactive."
  (should (commandp 'ewwm-vr-eye-set-focus-policy)))

(ert-deftest ewwm-vr-eye-focus/set-dwell-threshold-interactive ()
  "ewwm-vr-eye-set-dwell-threshold is interactive."
  (should (commandp 'ewwm-vr-eye-set-dwell-threshold)))

(ert-deftest ewwm-vr-eye-focus/analytics-interactive ()
  "ewwm-vr-eye-analytics is interactive."
  (should (commandp 'ewwm-vr-eye-analytics)))

(ert-deftest ewwm-vr-eye-focus/focus-config-interactive ()
  "ewwm-vr-eye-focus-config is interactive."
  (should (commandp 'ewwm-vr-eye-focus-config)))

;; ── Policy validation ─────────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/set-policy-invalid ()
  "Setting invalid focus policy signals error."
  (should-error (ewwm-vr-eye-set-focus-policy 'invalid)))

(ert-deftest ewwm-vr-eye-focus/set-policy-valid ()
  "Setting valid focus policy updates variable."
  (let ((ewwm-vr-eye-focus-policy 'gaze-primary))
    (ewwm-vr-eye-set-focus-policy 'gaze-only)
    (should (eq ewwm-vr-eye-focus-policy 'gaze-only))))

;; ── Dwell threshold validation ────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/set-dwell-clamps-low ()
  "Dwell threshold is clamped to minimum 50ms."
  (let ((ewwm-vr-eye-dwell-ms 200))
    (ewwm-vr-eye-set-dwell-threshold 10)
    (should (= ewwm-vr-eye-dwell-ms 50))))

(ert-deftest ewwm-vr-eye-focus/set-dwell-clamps-high ()
  "Dwell threshold is clamped to maximum 2000ms."
  (let ((ewwm-vr-eye-dwell-ms 200))
    (ewwm-vr-eye-set-dwell-threshold 5000)
    (should (= ewwm-vr-eye-dwell-ms 2000))))

;; ── Dwell progress event ──────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/dwell-progress-event ()
  "Dwell progress event updates state."
  (let ((ewwm-vr-eye--dwell-progress 0.0)
        (ewwm-vr-eye--dwell-surface nil))
    (ewwm-vr-eye--on-gaze-dwell-progress '(:elapsed-ms 100 :threshold-ms 200 :surface-id 42))
    (should (= ewwm-vr-eye--dwell-progress 0.5))
    (should (= ewwm-vr-eye--dwell-surface 42))))

(ert-deftest ewwm-vr-eye-focus/dwell-progress-clamps ()
  "Dwell progress clamps to 1.0."
  (let ((ewwm-vr-eye--dwell-progress 0.0)
        (ewwm-vr-eye--dwell-surface nil))
    (ewwm-vr-eye--on-gaze-dwell-progress '(:elapsed-ms 300 :threshold-ms 200 :surface-id 42))
    (should (= ewwm-vr-eye--dwell-progress 1.0))))

;; ── Cooldown event ────────────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/cooldown-event ()
  "Cooldown event updates remaining."
  (let ((ewwm-vr-eye--cooldown-remaining 0))
    (ewwm-vr-eye--on-gaze-cooldown '(:remaining-ms 350))
    (should (= ewwm-vr-eye--cooldown-remaining 350))))

;; ── Saccade state event ───────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/saccade-state-event ()
  "Saccade state event updates state."
  (let ((ewwm-vr-eye--in-saccade nil))
    (ewwm-vr-eye--on-gaze-saccade-state '(:active t))
    (should ewwm-vr-eye--in-saccade)))

(ert-deftest ewwm-vr-eye-focus/saccade-state-event-off ()
  "Saccade state event clears state."
  (let ((ewwm-vr-eye--in-saccade t))
    (ewwm-vr-eye--on-gaze-saccade-state '(:active nil))
    (should-not ewwm-vr-eye--in-saccade)))

;; ── Reading mode event ────────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/reading-mode-event ()
  "Reading mode event updates state."
  (let ((ewwm-vr-eye--in-reading nil))
    (ewwm-vr-eye--on-gaze-reading-mode '(:active t))
    (should ewwm-vr-eye--in-reading)))

;; ── Focus should-focus-p ──────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/should-focus-disabled ()
  "Disabled policy rejects focus."
  (let ((ewwm-vr-eye-focus-policy 'disabled)
        (ewwm-vr-eye-focus-exceptions nil)
        (ewwm-vr-eye-cross-workspace-focus nil))
    (should-not (ewwm-vr-eye--should-focus-p 42))))

(ert-deftest ewwm-vr-eye-focus/should-focus-gaze-assist ()
  "Gaze-assist policy rejects auto-focus."
  (let ((ewwm-vr-eye-focus-policy 'gaze-assist)
        (ewwm-vr-eye-focus-exceptions nil)
        (ewwm-vr-eye-cross-workspace-focus nil))
    (should-not (ewwm-vr-eye--should-focus-p 42))))

(ert-deftest ewwm-vr-eye-focus/should-focus-gaze-primary ()
  "Gaze-primary policy allows focus."
  (let ((ewwm-vr-eye-focus-policy 'gaze-primary)
        (ewwm-vr-eye-focus-exceptions nil)
        (ewwm-vr-eye-cross-workspace-focus nil)
        (ewwm--surface-buffer-alist nil))
    (should (ewwm-vr-eye--should-focus-p 42))))

;; ── Mode-line format (Week 12 updates) ────────────────────────

(ert-deftest ewwm-vr-eye-focus/mode-line-saccade ()
  "Mode-line shows saccade indicator."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-eye--in-saccade t)
        (ewwm-vr-eye--cooldown-remaining 0)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-eye--in-reading nil)
        (ewwm-vr-eye-show-dwell-progress t)
        (ewwm-vr-eye--dwell-surface nil)
        (ewwm-vr-eye--dwell-progress 0.0)
        (ewwm-vr-gaze-surface nil))
    (should (string= (ewwm-vr-eye-mode-line-string) " [Eye:>>>]"))))

(ert-deftest ewwm-vr-eye-focus/mode-line-cooldown ()
  "Mode-line shows cooldown with remaining time."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-eye--in-saccade nil)
        (ewwm-vr-eye--cooldown-remaining 250)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-eye--in-reading nil)
        (ewwm-vr-eye-show-dwell-progress t)
        (ewwm-vr-eye--dwell-surface nil)
        (ewwm-vr-eye--dwell-progress 0.0)
        (ewwm-vr-gaze-surface nil))
    (should (string= (ewwm-vr-eye-mode-line-string) " [Eye:COOL 250ms]"))))

(ert-deftest ewwm-vr-eye-focus/mode-line-reading ()
  "Mode-line shows reading mode."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-eye--in-saccade nil)
        (ewwm-vr-eye--cooldown-remaining 0)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-eye--in-reading t)
        (ewwm-vr-eye-show-dwell-progress t)
        (ewwm-vr-eye--dwell-surface nil)
        (ewwm-vr-eye--dwell-progress 0.0)
        (ewwm-vr-gaze-surface 42))
    (should (string= (ewwm-vr-eye-mode-line-string) " [Eye:READ 42]"))))

(ert-deftest ewwm-vr-eye-focus/mode-line-dwell-progress ()
  "Mode-line shows dwell progress bar."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-eye--in-saccade nil)
        (ewwm-vr-eye--cooldown-remaining 0)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-eye--in-reading nil)
        (ewwm-vr-eye-show-dwell-progress t)
        (ewwm-vr-eye--dwell-surface 42)
        (ewwm-vr-eye--dwell-progress 0.6)
        (ewwm-vr-gaze-surface 42))
    (let ((result (ewwm-vr-eye-mode-line-string)))
      (should (string-match-p "\\[Eye:.*42\\]" result)))))

(ert-deftest ewwm-vr-eye-focus/mode-line-normal ()
  "Mode-line shows Eye:ID for normal tracking."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-eye--in-saccade nil)
        (ewwm-vr-eye--cooldown-remaining 0)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-eye--in-reading nil)
        (ewwm-vr-eye-show-dwell-progress t)
        (ewwm-vr-eye--dwell-surface nil)
        (ewwm-vr-eye--dwell-progress 0.0)
        (ewwm-vr-gaze-surface 42))
    (should (string= (ewwm-vr-eye-mode-line-string) " [Eye:42]"))))

(ert-deftest ewwm-vr-eye-focus/mode-line-no-target ()
  "Mode-line shows Eye:--- when no target."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-eye--in-saccade nil)
        (ewwm-vr-eye--cooldown-remaining 0)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-eye--in-reading nil)
        (ewwm-vr-eye-show-dwell-progress t)
        (ewwm-vr-eye--dwell-surface nil)
        (ewwm-vr-eye--dwell-progress 0.0)
        (ewwm-vr-gaze-surface nil))
    (should (string= (ewwm-vr-eye-mode-line-string) " [Eye:---]"))))

;; ── Event registration includes focus events ──────────────────

(ert-deftest ewwm-vr-eye-focus/register-events-focus ()
  "Event registration includes gaze focus events."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-eye--register-events)
    (should (assq :gaze-dwell ewwm-ipc--event-handlers))
    (should (assq :gaze-dwell-progress ewwm-ipc--event-handlers))
    (should (assq :gaze-focus-request ewwm-ipc--event-handlers))
    (should (assq :gaze-cooldown ewwm-ipc--event-handlers))
    (should (assq :gaze-saccade-state ewwm-ipc--event-handlers))
    (should (assq :gaze-reading-mode ewwm-ipc--event-handlers))))

;; ── Teardown clears focus state ───────────────────────────────

(ert-deftest ewwm-vr-eye-focus/teardown-clears-focus-state ()
  "Teardown clears all focus state."
  (let ((ewwm-vr-gaze-surface 42)
        (ewwm-vr-gaze-position '(100 . 200))
        (ewwm-vr-gaze-confidence 0.9)
        (ewwm-vr-gaze-source-active :openxr)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-gaze-calibrated-p t)
        (ewwm-vr-eye--tracking-active t)
        (ewwm-vr-eye--gaze-position '(100 . 200))
        (ewwm-vr-eye--dwell-surface 42)
        (ewwm-vr-eye--dwell-progress 0.5)
        (ewwm-vr-eye--cooldown-remaining 300)
        (ewwm-vr-eye--in-saccade t)
        (ewwm-vr-eye--in-reading t)
        (ewwm-last-focus-method 'gaze)
        (ewwm-vr-eye--analytics '(:switches 5)))
    (ewwm-vr-eye-teardown)
    (should-not ewwm-vr-eye--dwell-surface)
    (should (= ewwm-vr-eye--dwell-progress 0.0))
    (should (= ewwm-vr-eye--cooldown-remaining 0))
    (should-not ewwm-vr-eye--in-saccade)
    (should-not ewwm-vr-eye--in-reading)
    (should-not ewwm-last-focus-method)
    (should-not ewwm-vr-eye--analytics)))

;; ── Focus-back with empty ring ────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/focus-back-empty ()
  "Focus back on empty ring messages (no crash)."
  (let ((ewwm-vr-eye--focus-ring (make-ring 10)))
    (ewwm-vr-eye-focus-back)))

;; ── Minor mode keymap ─────────────────────────────────────────

(ert-deftest ewwm-vr-eye-focus/keymap-bindings ()
  "Minor mode keymap has focus bindings."
  (should (fboundp 'ewwm-vr-eye-mode))
  (let ((map (symbol-value 'ewwm-vr-eye-mode-map)))
    (when map
      (should (lookup-key map (kbd "C-c e b")))
      (should (lookup-key map (kbd "C-c e p")))
      (should (lookup-key map (kbd "C-c e d")))
      (should (lookup-key map (kbd "C-c e a")))
      (should (lookup-key map (kbd "C-c e C"))))))

(provide 'ewwm-vr-eye-focus-test)
;;; ewwm-vr-eye-focus-test.el ends here
