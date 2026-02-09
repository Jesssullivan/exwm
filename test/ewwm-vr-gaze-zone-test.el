;;; ewwm-vr-gaze-zone-test.el --- Week 13 gaze zone tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-gaze-zone)

;; Forward-declare so `let' creates dynamic binding (needed for `boundp')
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/enable-defcustom ()
  "Default enable is t."
  (should (eq (default-value 'ewwm-vr-gaze-zone-enable) t)))

(ert-deftest ewwm-vr-gaze-zone/dwell-ms-defcustom ()
  "Default dwell-ms is 200."
  (should (= (default-value 'ewwm-vr-gaze-zone-dwell-ms) 200)))

(ert-deftest ewwm-vr-gaze-zone/layout-defcustom ()
  "Default layout is default."
  (should (eq (default-value 'ewwm-vr-gaze-zone-layout) 'default)))

(ert-deftest ewwm-vr-gaze-zone/custom-map-defcustom ()
  "Default custom map is nil."
  (should (null (default-value 'ewwm-vr-gaze-zone-custom-map))))

(ert-deftest ewwm-vr-gaze-zone/overlay-alpha-defcustom ()
  "Default overlay alpha is 0.15."
  (should (= (default-value 'ewwm-vr-gaze-zone-overlay-alpha) 0.15)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/active-exists ()
  "Active zone variable exists."
  (should (boundp 'ewwm-vr-gaze-zone--active)))

(ert-deftest ewwm-vr-gaze-zone/current-exists ()
  "Current zone variable exists."
  (should (boundp 'ewwm-vr-gaze-zone--current)))

(ert-deftest ewwm-vr-gaze-zone/dwell-progress-exists ()
  "Dwell progress variable exists."
  (should (boundp 'ewwm-vr-gaze-zone--dwell-progress)))

(ert-deftest ewwm-vr-gaze-zone/transient-modifier-exists ()
  "Transient modifier variable exists."
  (should (boundp 'ewwm-vr-gaze-zone--transient-modifier)))

;; ── Layout presets ──────────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/default-layout-has-9-entries ()
  "Default layout has 9 zone entries."
  (let ((ewwm-vr-gaze-zone-layout 'default))
    (should (= (length (ewwm-vr-gaze-zone--get-layout)) 9))))

(ert-deftest ewwm-vr-gaze-zone/vim-layout-has-9-entries ()
  "Vim layout has 9 zone entries."
  (let ((ewwm-vr-gaze-zone-layout 'vim-like))
    (should (= (length (ewwm-vr-gaze-zone--get-layout)) 9))))

(ert-deftest ewwm-vr-gaze-zone/spacemacs-layout-has-9-entries ()
  "Spacemacs layout has 9 zone entries."
  (let ((ewwm-vr-gaze-zone-layout 'spacemacs))
    (should (= (length (ewwm-vr-gaze-zone--get-layout)) 9))))

(ert-deftest ewwm-vr-gaze-zone/default-layout-top-left-is-cx ()
  "Default layout maps top-left to C-x."
  (let ((ewwm-vr-gaze-zone-layout 'default))
    (should (string= (alist-get 'top-left (ewwm-vr-gaze-zone--get-layout)) "C-x"))))

(ert-deftest ewwm-vr-gaze-zone/default-layout-center-is-empty ()
  "Default layout maps center to empty string."
  (let ((ewwm-vr-gaze-zone-layout 'default))
    (should (string= (alist-get 'center (ewwm-vr-gaze-zone--get-layout)) ""))))

(ert-deftest ewwm-vr-gaze-zone/custom-layout-uses-custom-map ()
  "Custom layout uses custom map."
  (let ((ewwm-vr-gaze-zone-layout 'custom)
        (ewwm-vr-gaze-zone-custom-map '((center . "M-x"))))
    (should (string= (alist-get 'center (ewwm-vr-gaze-zone--get-layout)) "M-x"))))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/set-layout-interactive ()
  "ewwm-vr-gaze-zone-set-layout is interactive."
  (should (commandp 'ewwm-vr-gaze-zone-set-layout)))

(ert-deftest ewwm-vr-gaze-zone/status-interactive ()
  "ewwm-vr-gaze-zone-status is interactive."
  (should (commandp 'ewwm-vr-gaze-zone-status)))

(ert-deftest ewwm-vr-gaze-zone/set-layout-invalid ()
  "Setting invalid layout signals error."
  (should-error (ewwm-vr-gaze-zone-set-layout 'bogus)))

;; ── IPC event handlers ──────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/zone-entered-event ()
  "Zone entered event updates current zone."
  (let ((ewwm-vr-gaze-zone-enable t)
        (ewwm-vr-gaze-zone--current nil))
    (ewwm-vr-gaze-zone--on-zone-entered '(:zone "top-left"))
    (should (eq ewwm-vr-gaze-zone--current 'top-left))))

(ert-deftest ewwm-vr-gaze-zone/zone-entered-disabled ()
  "Zone entered ignored when disabled."
  (let ((ewwm-vr-gaze-zone-enable nil)
        (ewwm-vr-gaze-zone--current nil))
    (ewwm-vr-gaze-zone--on-zone-entered '(:zone "top-left"))
    (should-not ewwm-vr-gaze-zone--current)))

(ert-deftest ewwm-vr-gaze-zone/zone-activated-event ()
  "Zone activated event sets active zone and runs hook."
  (let ((ewwm-vr-gaze-zone-enable t)
        (ewwm-vr-gaze-zone--active nil)
        (ewwm-vr-gaze-zone--last-activation-time nil)
        (ewwm-vr-gaze-zone-layout 'default)
        (hook-args nil))
    (let ((ewwm-vr-gaze-zone-activate-hook
           (list (lambda (z m sid) (setq hook-args (list z m sid))))))
      (ewwm-vr-gaze-zone--on-zone-activated '(:zone "top-right" :modifier "M-x" :surface-id 42))
      (should (eq ewwm-vr-gaze-zone--active 'top-right))
      (should (equal hook-args '(top-right "M-x" 42))))))

(ert-deftest ewwm-vr-gaze-zone/zone-deactivated-event ()
  "Zone deactivated event clears active zone."
  (let ((ewwm-vr-gaze-zone--active 'top-left)
        (ewwm-vr-gaze-zone--dwell-progress 0.8))
    (ewwm-vr-gaze-zone--on-zone-deactivated nil)
    (should-not ewwm-vr-gaze-zone--active)
    (should (= ewwm-vr-gaze-zone--dwell-progress 0.0))))

(ert-deftest ewwm-vr-gaze-zone/dwell-progress-event ()
  "Dwell progress event updates fraction."
  (let ((ewwm-vr-gaze-zone--dwell-progress 0.0))
    (ewwm-vr-gaze-zone--on-zone-dwell-progress '(:elapsed-ms 100 :threshold-ms 200))
    (should (= ewwm-vr-gaze-zone--dwell-progress 0.5))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/register-events ()
  "ewwm-vr-gaze-zone--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-gaze-zone--register-events)
    (should (assq :gaze-zone-entered ewwm-ipc--event-handlers))
    (should (assq :gaze-zone-activated ewwm-ipc--event-handlers))
    (should (assq :gaze-zone-deactivated ewwm-ipc--event-handlers))
    (should (assq :gaze-zone-dwell-progress ewwm-ipc--event-handlers))))

(ert-deftest ewwm-vr-gaze-zone/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-gaze-zone--register-events)
    (ewwm-vr-gaze-zone--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :gaze-zone-entered))
                        ewwm-ipc--event-handlers))
               1))))

;; ── Mode-line ───────────────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/mode-line-active ()
  "Mode-line shows zone when active."
  (let ((ewwm-vr-gaze-zone-enable t)
        (ewwm-vr-gaze-zone--active 'top-left)
        (ewwm-vr-gaze-zone-layout 'default))
    (should (string= (ewwm-vr-gaze-zone-mode-line-string) " [Zone:TL C-x]"))))

(ert-deftest ewwm-vr-gaze-zone/mode-line-nil-when-inactive ()
  "Mode-line nil when no active zone."
  (let ((ewwm-vr-gaze-zone-enable t)
        (ewwm-vr-gaze-zone--active nil))
    (should-not (ewwm-vr-gaze-zone-mode-line-string))))

(ert-deftest ewwm-vr-gaze-zone/mode-line-nil-when-disabled ()
  "Mode-line nil when disabled."
  (let ((ewwm-vr-gaze-zone-enable nil)
        (ewwm-vr-gaze-zone--active 'top-left))
    (should-not (ewwm-vr-gaze-zone-mode-line-string))))

(ert-deftest ewwm-vr-gaze-zone/mode-line-nil-for-center ()
  "Mode-line nil when center zone (empty modifier)."
  (let ((ewwm-vr-gaze-zone-enable t)
        (ewwm-vr-gaze-zone--active 'center)
        (ewwm-vr-gaze-zone-layout 'default))
    (should-not (ewwm-vr-gaze-zone-mode-line-string))))

;; ── Minor mode ──────────────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/minor-mode-exists ()
  "ewwm-vr-gaze-zone-mode minor mode exists."
  (should (fboundp 'ewwm-vr-gaze-zone-mode)))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/teardown-clears-state ()
  "ewwm-vr-gaze-zone-teardown clears all state."
  (let ((ewwm-vr-gaze-zone--active 'top-left)
        (ewwm-vr-gaze-zone--current 'top-right)
        (ewwm-vr-gaze-zone--dwell-progress 0.75)
        (ewwm-vr-gaze-zone--transient-modifier "C-")
        (ewwm-vr-gaze-zone--last-activation-time 12345.0))
    (ewwm-vr-gaze-zone-teardown)
    (should-not ewwm-vr-gaze-zone--active)
    (should-not ewwm-vr-gaze-zone--current)
    (should (= ewwm-vr-gaze-zone--dwell-progress 0.0))
    (should-not ewwm-vr-gaze-zone--transient-modifier)
    (should-not ewwm-vr-gaze-zone--last-activation-time)))

;; ── Hooks exist ─────────────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/activate-hook-exists ()
  "Activate hook exists."
  (should (boundp 'ewwm-vr-gaze-zone-activate-hook)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-vr-gaze-zone/provides-feature ()
  "ewwm-vr-gaze-zone provides its feature."
  (should (featurep 'ewwm-vr-gaze-zone)))

;;; ewwm-vr-gaze-zone-test.el ends here
