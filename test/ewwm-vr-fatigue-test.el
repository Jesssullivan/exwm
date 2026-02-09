;;; ewwm-vr-fatigue-test.el --- Week 13 fatigue tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-fatigue)

;; Forward-declare so `let' creates dynamic binding (needed for `boundp')
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/enable-defcustom ()
  "Default enable is t."
  (should (eq (default-value 'ewwm-vr-fatigue-enable) t)))

(ert-deftest ewwm-vr-fatigue/alert-threshold-defcustom ()
  "Default alert threshold is 25."
  (should (= (default-value 'ewwm-vr-fatigue-alert-threshold) 25)))

(ert-deftest ewwm-vr-fatigue/perclos-threshold-defcustom ()
  "Default PERCLOS threshold is 0.15."
  (should (= (default-value 'ewwm-vr-fatigue-perclos-threshold) 0.15)))

(ert-deftest ewwm-vr-fatigue/log-enabled-defcustom ()
  "Default log enabled is t."
  (should (eq (default-value 'ewwm-vr-fatigue-log-enabled) t)))

(ert-deftest ewwm-vr-fatigue/check-interval-defcustom ()
  "Default check interval is 60."
  (should (= (default-value 'ewwm-vr-fatigue-check-interval) 60)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/level-exists ()
  "Fatigue level variable exists."
  (should (boundp 'ewwm-vr-fatigue--level)))

(ert-deftest ewwm-vr-fatigue/blink-rate-exists ()
  "Blink rate variable exists."
  (should (boundp 'ewwm-vr-fatigue--blink-rate)))

(ert-deftest ewwm-vr-fatigue/saccade-jitter-exists ()
  "Saccade jitter variable exists."
  (should (boundp 'ewwm-vr-fatigue--saccade-jitter)))

(ert-deftest ewwm-vr-fatigue/perclos-exists ()
  "PERCLOS variable exists."
  (should (boundp 'ewwm-vr-fatigue--perclos)))

(ert-deftest ewwm-vr-fatigue/session-start-exists ()
  "Session start variable exists."
  (should (boundp 'ewwm-vr-fatigue--session-start)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/status-interactive ()
  "ewwm-vr-fatigue-status is interactive."
  (should (commandp 'ewwm-vr-fatigue-status)))

(ert-deftest ewwm-vr-fatigue/reset-interactive ()
  "ewwm-vr-fatigue-reset is interactive."
  (should (commandp 'ewwm-vr-fatigue-reset)))

;; ── IPC event handlers ──────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/alert-event-updates-state ()
  "Fatigue alert event updates state variables."
  (let ((ewwm-vr-fatigue-enable t)
        (ewwm-vr-fatigue--level 'normal)
        (ewwm-vr-fatigue--blink-rate 0.0)
        (ewwm-vr-fatigue--saccade-jitter 0.0)
        (ewwm-vr-fatigue--perclos 0.0)
        (ewwm-vr-fatigue--last-alert-level nil)
        (ewwm-vr-fatigue-log-enabled nil)
        (ewwm-vr-fatigue-alert-hook nil))
    (ewwm-vr-fatigue--on-fatigue-alert
     '(:level "mild" :blink-rate 22.0 :saccade-jitter 1.5 :perclos 0.08))
    (should (eq ewwm-vr-fatigue--level 'mild))
    (should (= ewwm-vr-fatigue--blink-rate 22.0))
    (should (= ewwm-vr-fatigue--saccade-jitter 1.5))
    (should (= ewwm-vr-fatigue--perclos 0.08))))

(ert-deftest ewwm-vr-fatigue/alert-event-disabled ()
  "Fatigue alert ignored when disabled."
  (let ((ewwm-vr-fatigue-enable nil)
        (ewwm-vr-fatigue--level 'normal)
        (ewwm-vr-fatigue--blink-rate 0.0))
    (ewwm-vr-fatigue--on-fatigue-alert
     '(:level "mild" :blink-rate 22.0))
    (should (eq ewwm-vr-fatigue--level 'normal))
    (should (= ewwm-vr-fatigue--blink-rate 0.0))))

(ert-deftest ewwm-vr-fatigue/alert-hook-runs ()
  "Fatigue alert runs hook."
  (let ((ewwm-vr-fatigue-enable t)
        (ewwm-vr-fatigue--level 'normal)
        (ewwm-vr-fatigue--blink-rate 0.0)
        (ewwm-vr-fatigue--saccade-jitter 0.0)
        (ewwm-vr-fatigue--perclos 0.0)
        (ewwm-vr-fatigue--session-start nil)
        (ewwm-vr-fatigue--last-alert-level nil)
        (ewwm-vr-fatigue-log-enabled nil)
        (hook-args nil))
    (let ((ewwm-vr-fatigue-alert-hook
           (list (lambda (lvl metrics) (setq hook-args (list lvl metrics))))))
      (ewwm-vr-fatigue--on-fatigue-alert '(:level "mild" :blink-rate 22.0))
      (should (eq (car hook-args) 'mild)))))

(ert-deftest ewwm-vr-fatigue/metrics-event-updates-state ()
  "Metrics event updates state."
  (let ((ewwm-vr-fatigue-enable t)
        (ewwm-vr-fatigue--blink-rate 0.0)
        (ewwm-vr-fatigue--saccade-jitter 0.0)
        (ewwm-vr-fatigue--perclos 0.0)
        (ewwm-vr-fatigue--level 'normal))
    (ewwm-vr-fatigue--on-fatigue-metrics
     '(:blink-rate 18.0 :saccade-jitter 1.2 :perclos 0.06 :level "normal"))
    (should (= ewwm-vr-fatigue--blink-rate 18.0))
    (should (= ewwm-vr-fatigue--saccade-jitter 1.2))
    (should (= ewwm-vr-fatigue--perclos 0.06))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/register-events ()
  "ewwm-vr-fatigue--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-fatigue--register-events)
    (should (assq :fatigue-alert ewwm-ipc--event-handlers))
    (should (assq :fatigue-metrics ewwm-ipc--event-handlers))))

(ert-deftest ewwm-vr-fatigue/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-fatigue--register-events)
    (ewwm-vr-fatigue--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :fatigue-alert))
                        ewwm-ipc--event-handlers))
               1))))

;; ── Mode-line ───────────────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/mode-line-normal ()
  "Mode-line nil when normal."
  (let ((ewwm-vr-fatigue-enable t)
        (ewwm-vr-fatigue--level 'normal))
    (should-not (ewwm-vr-fatigue-mode-line-string))))

(ert-deftest ewwm-vr-fatigue/mode-line-mild ()
  "Mode-line shows MILD."
  (let ((ewwm-vr-fatigue-enable t)
        (ewwm-vr-fatigue--level 'mild))
    (should (string= (ewwm-vr-fatigue-mode-line-string) " [Fat:MILD]"))))

(ert-deftest ewwm-vr-fatigue/mode-line-significant ()
  "Mode-line shows HIGH for significant."
  (let ((ewwm-vr-fatigue-enable t)
        (ewwm-vr-fatigue--level 'significant))
    (should (string= (ewwm-vr-fatigue-mode-line-string) " [Fat:HIGH]"))))

(ert-deftest ewwm-vr-fatigue/mode-line-critical ()
  "Mode-line shows CRIT for critical."
  (let ((ewwm-vr-fatigue-enable t)
        (ewwm-vr-fatigue--level 'critical))
    (should (string= (ewwm-vr-fatigue-mode-line-string) " [Fat:CRIT]"))))

(ert-deftest ewwm-vr-fatigue/mode-line-disabled ()
  "Mode-line nil when disabled."
  (let ((ewwm-vr-fatigue-enable nil)
        (ewwm-vr-fatigue--level 'critical))
    (should-not (ewwm-vr-fatigue-mode-line-string))))

;; ── Minor mode ──────────────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/minor-mode-exists ()
  "ewwm-vr-fatigue-mode minor mode exists."
  (should (fboundp 'ewwm-vr-fatigue-mode)))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/teardown-clears-state ()
  "ewwm-vr-fatigue-teardown clears all state."
  (let ((ewwm-vr-fatigue--level 'critical)
        (ewwm-vr-fatigue--blink-rate 30.0)
        (ewwm-vr-fatigue--saccade-jitter 3.0)
        (ewwm-vr-fatigue--perclos 0.2)
        (ewwm-vr-fatigue--session-start 12345.0)
        (ewwm-vr-fatigue--last-alert-level 'critical))
    (ewwm-vr-fatigue-teardown)
    (should (eq ewwm-vr-fatigue--level 'normal))
    (should (= ewwm-vr-fatigue--blink-rate 0.0))
    (should (= ewwm-vr-fatigue--saccade-jitter 0.0))
    (should (= ewwm-vr-fatigue--perclos 0.0))
    (should-not ewwm-vr-fatigue--session-start)
    (should-not ewwm-vr-fatigue--last-alert-level)))

(ert-deftest ewwm-vr-fatigue/reset-sets-session-start ()
  "ewwm-vr-fatigue-reset resets metrics and sets new session start."
  (let ((ewwm-vr-fatigue--level 'critical)
        (ewwm-vr-fatigue--blink-rate 30.0)
        (ewwm-vr-fatigue--saccade-jitter 3.0)
        (ewwm-vr-fatigue--perclos 0.2)
        (ewwm-vr-fatigue--session-start nil)
        (ewwm-vr-fatigue--last-alert-level 'critical))
    (ewwm-vr-fatigue-reset)
    (should (eq ewwm-vr-fatigue--level 'normal))
    (should (= ewwm-vr-fatigue--blink-rate 0.0))
    (should ewwm-vr-fatigue--session-start)))

;; ── Hooks exist ─────────────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/alert-hook-exists ()
  "Fatigue alert hook exists."
  (should (boundp 'ewwm-vr-fatigue-alert-hook)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-vr-fatigue/provides-feature ()
  "ewwm-vr-fatigue provides its feature."
  (should (featurep 'ewwm-vr-fatigue)))

;;; ewwm-vr-fatigue-test.el ends here
