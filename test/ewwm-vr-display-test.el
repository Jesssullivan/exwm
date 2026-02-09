;;; ewwm-vr-display-test.el --- Week 9 VR display tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-display)

;; Forward-declare so `let' creates dynamic binding (needed for `boundp')
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-vr-display/default-mode-defcustom ()
  "Default mode is auto."
  (should (eq (default-value 'ewwm-vr-display-default-mode) 'auto)))

(ert-deftest ewwm-vr-display/default-refresh-rate-defcustom ()
  "Default refresh rate is 90."
  (should (= (default-value 'ewwm-vr-display-default-refresh-rate) 90)))

(ert-deftest ewwm-vr-display/auto-select-defcustom ()
  "Auto-select HMD defaults to t."
  (should (eq (default-value 'ewwm-vr-display-auto-select-hmd) t)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-vr-display/mode-variable-exists ()
  "ewwm-vr-display-mode variable exists."
  (should (boundp 'ewwm-vr-display-mode)))

(ert-deftest ewwm-vr-display/hmd-variable-exists ()
  "ewwm-vr-display-hmd variable exists."
  (should (boundp 'ewwm-vr-display-hmd)))

(ert-deftest ewwm-vr-display/connector-variable-exists ()
  "ewwm-vr-display-connector variable exists."
  (should (boundp 'ewwm-vr-display-connector)))

(ert-deftest ewwm-vr-display/refresh-rate-variable-exists ()
  "ewwm-vr-display-refresh-rate variable exists."
  (should (boundp 'ewwm-vr-display-refresh-rate)))

(ert-deftest ewwm-vr-display/connectors-variable-exists ()
  "ewwm-vr-display-connectors variable exists."
  (should (boundp 'ewwm-vr-display-connectors)))

(ert-deftest ewwm-vr-display/hmd-count-variable-exists ()
  "ewwm-vr-display-hmd-count variable exists."
  (should (boundp 'ewwm-vr-display-hmd-count)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-vr-display/info-interactive ()
  "ewwm-vr-display-info is interactive."
  (should (commandp 'ewwm-vr-display-info)))

(ert-deftest ewwm-vr-display/set-mode-interactive ()
  "ewwm-vr-display-set-mode is interactive."
  (should (commandp 'ewwm-vr-display-set-mode)))

(ert-deftest ewwm-vr-display/select-hmd-interactive ()
  "ewwm-vr-display-select-hmd is interactive."
  (should (commandp 'ewwm-vr-display-select-hmd)))

(ert-deftest ewwm-vr-display/set-refresh-rate-interactive ()
  "ewwm-vr-display-set-refresh-rate is interactive."
  (should (commandp 'ewwm-vr-display-set-refresh-rate)))

(ert-deftest ewwm-vr-display/auto-detect-interactive ()
  "ewwm-vr-display-auto-detect is interactive."
  (should (commandp 'ewwm-vr-display-auto-detect)))

(ert-deftest ewwm-vr-display/list-connectors-interactive ()
  "ewwm-vr-display-list-connectors is interactive."
  (should (commandp 'ewwm-vr-display-list-connectors)))

;; ── Set mode validation ─────────────────────────────────────

(ert-deftest ewwm-vr-display/set-mode-headset ()
  "Setting mode to headset succeeds."
  (let ((ewwm-vr-display-mode nil))
    (ewwm-vr-display-set-mode 'headset)
    (should (eq ewwm-vr-display-mode 'headset))))

(ert-deftest ewwm-vr-display/set-mode-preview ()
  "Setting mode to preview succeeds."
  (let ((ewwm-vr-display-mode nil))
    (ewwm-vr-display-set-mode 'preview)
    (should (eq ewwm-vr-display-mode 'preview))))

(ert-deftest ewwm-vr-display/set-mode-headless ()
  "Setting mode to headless succeeds."
  (let ((ewwm-vr-display-mode nil))
    (ewwm-vr-display-set-mode 'headless)
    (should (eq ewwm-vr-display-mode 'headless))))

(ert-deftest ewwm-vr-display/set-mode-off ()
  "Setting mode to off succeeds."
  (let ((ewwm-vr-display-mode nil))
    (ewwm-vr-display-set-mode 'off)
    (should (eq ewwm-vr-display-mode 'off))))

(ert-deftest ewwm-vr-display/set-mode-invalid ()
  "Setting invalid mode signals error."
  (should-error (ewwm-vr-display-set-mode 'invalid)))

;; ── Set refresh rate validation ─────────────────────────────

(ert-deftest ewwm-vr-display/set-refresh-rate-valid ()
  "Setting valid refresh rate succeeds."
  (let ((ewwm-vr-display-target-refresh-rate nil))
    (ewwm-vr-display-set-refresh-rate 120)
    (should (= ewwm-vr-display-target-refresh-rate 120))))

(ert-deftest ewwm-vr-display/set-refresh-rate-invalid ()
  "Setting non-positive refresh rate signals error."
  (should-error (ewwm-vr-display-set-refresh-rate 0)))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-vr-display/init-sets-defaults ()
  "ewwm-vr-display-init sets target refresh rate from defcustom."
  (let ((ewwm-vr-display-target-refresh-rate nil))
    (ewwm-vr-display-init)
    (should (= ewwm-vr-display-target-refresh-rate 90))))

(ert-deftest ewwm-vr-display/teardown-clears-state ()
  "ewwm-vr-display-teardown clears all state."
  (let ((ewwm-vr-display-mode 'headset)
        (ewwm-vr-display-hmd "Index")
        (ewwm-vr-display-connector "DP-1")
        (ewwm-vr-display-refresh-rate 90)
        (ewwm-vr-display-target-refresh-rate 90)
        (ewwm-vr-display-connectors '(a))
        (ewwm-vr-display-hmd-count 1))
    (ewwm-vr-display-teardown)
    (should-not ewwm-vr-display-mode)
    (should-not ewwm-vr-display-hmd)
    (should-not ewwm-vr-display-connector)
    (should-not ewwm-vr-display-refresh-rate)
    (should-not ewwm-vr-display-target-refresh-rate)
    (should-not ewwm-vr-display-connectors)
    (should (= ewwm-vr-display-hmd-count 0))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-vr-display/register-events ()
  "ewwm-vr-display--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-display--register-events)
    (should (assq :vr-display-mode-changed ewwm-ipc--event-handlers))
    (should (assq :vr-display-hotplug ewwm-ipc--event-handlers))
    (should (assq :vr-display-hmd-selected ewwm-ipc--event-handlers))))

(ert-deftest ewwm-vr-display/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-display--register-events)
    (ewwm-vr-display--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :vr-display-mode-changed))
                        ewwm-ipc--event-handlers))
               1))))

;; ── IPC event handlers ──────────────────────────────────────

(ert-deftest ewwm-vr-display/mode-changed-event ()
  "Mode changed event updates local state."
  (let ((ewwm-vr-display-mode nil)
        (ewwm-vr-display-mode-hook nil))
    (ewwm-vr-display--on-mode-changed '(:mode :headset))
    (should (eq ewwm-vr-display-mode :headset))))

(ert-deftest ewwm-vr-display/mode-changed-hook-runs ()
  "Mode changed event runs hook."
  (let ((ewwm-vr-display-mode nil)
        (hook-ran nil))
    (let ((ewwm-vr-display-mode-hook (list (lambda () (setq hook-ran t)))))
      (ewwm-vr-display--on-mode-changed '(:mode :preview))
      (should hook-ran))))

(ert-deftest ewwm-vr-display/hotplug-event ()
  "Hotplug event runs hook."
  (let ((hook-ran nil))
    (let ((ewwm-vr-display-hotplug-hook (list (lambda () (setq hook-ran t)))))
      (ewwm-vr-display--on-hotplug '(:connector "DP-1" :connected t))
      (should hook-ran))))

(ert-deftest ewwm-vr-display/hmd-selected-event ()
  "HMD selected event updates state."
  (let ((ewwm-vr-display-hmd nil)
        (ewwm-vr-display-connector nil))
    (ewwm-vr-display--on-hmd-selected '(:hmd "Valve Index" :connector "DP-3"))
    (should (equal ewwm-vr-display-hmd "Valve Index"))
    (should (equal ewwm-vr-display-connector "DP-3"))))

;; ── Hooks exist ─────────────────────────────────────────────

(ert-deftest ewwm-vr-display/mode-hook-exists ()
  "Display mode hook variable exists."
  (should (boundp 'ewwm-vr-display-mode-hook)))

(ert-deftest ewwm-vr-display/hotplug-hook-exists ()
  "Hotplug hook variable exists."
  (should (boundp 'ewwm-vr-display-hotplug-hook)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-vr-display/provides-feature ()
  "ewwm-vr-display provides its feature."
  (should (featurep 'ewwm-vr-display)))

;;; ewwm-vr-display-test.el ends here
