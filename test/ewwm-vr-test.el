;;; ewwm-vr-test.el --- Week 7 VR subsystem tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr)

;; ── Variables and defcustoms ────────────────────────────────

(ert-deftest ewwm-vr/session-state-variable-exists ()
  "ewwm-vr-session-state variable exists."
  (should (boundp 'ewwm-vr-session-state)))

(ert-deftest ewwm-vr/hmd-name-variable-exists ()
  "ewwm-vr-hmd-name variable exists."
  (should (boundp 'ewwm-vr-hmd-name)))

(ert-deftest ewwm-vr/hmd-info-variable-exists ()
  "ewwm-vr-hmd-info variable exists."
  (should (boundp 'ewwm-vr-hmd-info)))

(ert-deftest ewwm-vr/headless-variable-exists ()
  "ewwm-vr-headless variable exists."
  (should (boundp 'ewwm-vr-headless)))

(ert-deftest ewwm-vr/frame-stats-variable-exists ()
  "ewwm-vr-frame-stats variable exists."
  (should (boundp 'ewwm-vr-frame-stats)))

(ert-deftest ewwm-vr/reference-space-defcustom ()
  "ewwm-vr-reference-space defcustom defaults to local."
  (should (eq (default-value 'ewwm-vr-reference-space) 'local)))

(ert-deftest ewwm-vr/mode-line-defcustom ()
  "ewwm-vr-mode-line defcustom defaults to t."
  (should (eq (default-value 'ewwm-vr-mode-line) t)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-vr/status-command-interactive ()
  "ewwm-vr-status is an interactive command."
  (should (commandp 'ewwm-vr-status)))

(ert-deftest ewwm-vr/set-reference-space-interactive ()
  "ewwm-vr-set-reference-space is an interactive command."
  (should (commandp 'ewwm-vr-set-reference-space)))

(ert-deftest ewwm-vr/restart-interactive ()
  "ewwm-vr-restart is an interactive command."
  (should (commandp 'ewwm-vr-restart)))

(ert-deftest ewwm-vr/frame-timing-interactive ()
  "ewwm-vr-frame-timing is an interactive command."
  (should (commandp 'ewwm-vr-frame-timing)))

;; ── IPC event handling ──────────────────────────────────────

(ert-deftest ewwm-vr/session-state-event-updates-state ()
  "VR session state event updates ewwm-vr-session-state."
  (let ((ewwm-vr-session-state nil)
        (ewwm-vr-headless nil)
        (ewwm-vr-session-state-hook nil))
    (ewwm-vr--on-session-state '(:state :focused :headless nil))
    (should (eq ewwm-vr-session-state :focused))
    (should-not ewwm-vr-headless)))

(ert-deftest ewwm-vr/session-state-event-headless ()
  "VR session state event sets headless flag."
  (let ((ewwm-vr-session-state nil)
        (ewwm-vr-headless nil)
        (ewwm-vr-session-state-hook nil))
    (ewwm-vr--on-session-state '(:state :idle :headless t))
    (should (eq ewwm-vr-session-state :idle))
    (should ewwm-vr-headless)))

(ert-deftest ewwm-vr/session-state-hook-runs ()
  "VR session state change runs the hook."
  (let ((ewwm-vr-session-state nil)
        (ewwm-vr-headless nil)
        (hook-ran nil))
    (let ((ewwm-vr-session-state-hook (list (lambda () (setq hook-ran t)))))
      (ewwm-vr--on-session-state '(:state :ready :headless nil))
      (should hook-ran))))

(ert-deftest ewwm-vr/system-discovered-event ()
  "VR system discovered event populates HMD info."
  (let ((ewwm-vr-hmd-name nil)
        (ewwm-vr-hmd-info nil))
    (ewwm-vr--on-system-discovered
     '(:system-name "Monado HMD"
       :max-resolution (:w 2880 :h 1600)
       :orientation-tracking t
       :position-tracking t))
    (should (string= ewwm-vr-hmd-name "Monado HMD"))
    (should (plist-get ewwm-vr-hmd-info :system-name))
    (should (= (plist-get ewwm-vr-hmd-info :max-width) 2880))
    (should (= (plist-get ewwm-vr-hmd-info :max-height) 1600))
    (should (plist-get ewwm-vr-hmd-info :orientation-tracking))
    (should (plist-get ewwm-vr-hmd-info :position-tracking))))

(ert-deftest ewwm-vr/frame-stats-event ()
  "VR frame stats event populates timing data."
  (let ((ewwm-vr-frame-stats nil))
    (ewwm-vr--on-frame-stats
     '(:fps 90 :total-p50 5.5 :total-p99 9.8
       :missed-pct 0.5 :total-frames 1000 :missed-frames 5
       :wait-p50 2.0 :render-p50 3.0 :submit-p50 0.5))
    (should (= (plist-get ewwm-vr-frame-stats :fps) 90))
    (should (= (plist-get ewwm-vr-frame-stats :total-p50) 5.5))
    (should (= (plist-get ewwm-vr-frame-stats :total-frames) 1000))))

;; ── Mode-line ───────────────────────────────────────────────

(ert-deftest ewwm-vr/mode-line-focused ()
  "Mode-line shows FOCUSED when session is focused."
  (let ((ewwm-vr-session-state :focused)
        (ewwm-vr-mode-line t))
    (should (string= (ewwm-vr-mode-line-string) " [VR:FOCUSED]"))))

(ert-deftest ewwm-vr/mode-line-headless ()
  "Mode-line shows HEADLESS when session is headless."
  (let ((ewwm-vr-session-state :headless)
        (ewwm-vr-mode-line t))
    (should (string= (ewwm-vr-mode-line-string) " [VR:HEADLESS]"))))

(ert-deftest ewwm-vr/mode-line-idle ()
  "Mode-line shows IDLE when session is idle."
  (let ((ewwm-vr-session-state :idle)
        (ewwm-vr-mode-line t))
    (should (string= (ewwm-vr-mode-line-string) " [VR:IDLE]"))))

(ert-deftest ewwm-vr/mode-line-disabled-empty ()
  "Mode-line is empty when VR disabled."
  (let ((ewwm-vr-session-state :disabled)
        (ewwm-vr-mode-line t))
    (should (string= (ewwm-vr-mode-line-string) ""))))

(ert-deftest ewwm-vr/mode-line-nil-empty ()
  "Mode-line is empty when state is nil."
  (let ((ewwm-vr-session-state nil)
        (ewwm-vr-mode-line t))
    (should (string= (ewwm-vr-mode-line-string) ""))))

(ert-deftest ewwm-vr/mode-line-off-returns-nil ()
  "Mode-line returns nil when ewwm-vr-mode-line is nil."
  (let ((ewwm-vr-session-state :focused)
        (ewwm-vr-mode-line nil))
    (should-not (ewwm-vr-mode-line-string))))

;; ── Teardown ────────────────────────────────────────────────

(ert-deftest ewwm-vr/teardown-clears-state ()
  "ewwm-vr-teardown clears all VR state variables."
  (let ((ewwm-vr-session-state :focused)
        (ewwm-vr-hmd-name "Test HMD")
        (ewwm-vr-hmd-info '(:name "test"))
        (ewwm-vr-headless t)
        (ewwm-vr-frame-stats '(:fps 90))
        (ewwm-vr-enabled t))
    (ewwm-vr-teardown)
    (should-not ewwm-vr-session-state)
    (should-not ewwm-vr-hmd-name)
    (should-not ewwm-vr-hmd-info)
    (should-not ewwm-vr-headless)
    (should-not ewwm-vr-frame-stats)
    (should-not ewwm-vr-enabled)))

;; ── Reference space validation ──────────────────────────────

(ert-deftest ewwm-vr/set-reference-space-local ()
  "Setting reference space to local succeeds."
  (let ((ewwm-vr-reference-space 'stage))
    (ewwm-vr-set-reference-space 'local)
    (should (eq ewwm-vr-reference-space 'local))))

(ert-deftest ewwm-vr/set-reference-space-stage ()
  "Setting reference space to stage succeeds."
  (let ((ewwm-vr-reference-space 'local))
    (ewwm-vr-set-reference-space 'stage)
    (should (eq ewwm-vr-reference-space 'stage))))

(ert-deftest ewwm-vr/set-reference-space-view ()
  "Setting reference space to view succeeds."
  (let ((ewwm-vr-reference-space 'local))
    (ewwm-vr-set-reference-space 'view)
    (should (eq ewwm-vr-reference-space 'view))))

(ert-deftest ewwm-vr/set-reference-space-invalid ()
  "Setting invalid reference space signals error."
  (let ((ewwm-vr-reference-space 'local))
    (should-error (ewwm-vr-set-reference-space 'invalid))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-vr/register-events-adds-handlers ()
  "ewwm-vr--register-events adds VR handlers to IPC event alist."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr--register-events)
    (should (assq :vr-session-state ewwm-ipc--event-handlers))
    (should (assq :vr-system-discovered ewwm-ipc--event-handlers))
    (should (assq :vr-frame-stats ewwm-ipc--event-handlers))))

(ert-deftest ewwm-vr/register-events-idempotent ()
  "Calling ewwm-vr--register-events twice doesn't duplicate handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr--register-events)
    (ewwm-vr--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :vr-session-state))
                        ewwm-ipc--event-handlers))
               1))))

;;; ewwm-vr-test.el ends here
