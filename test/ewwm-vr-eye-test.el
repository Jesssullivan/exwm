;;; ewwm-vr-eye-test.el --- Week 11 eye tracking tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-eye)

;; Forward-declare so `let' creates dynamic binding (needed for `boundp')
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-vr-eye/gaze-source-defcustom ()
  "Default gaze source is auto."
  (should (eq (default-value 'ewwm-vr-gaze-source) 'auto)))

(ert-deftest ewwm-vr-eye/gaze-smoothing-defcustom ()
  "Default smoothing alpha is 0.3."
  (should (= (default-value 'ewwm-vr-gaze-smoothing) 0.3)))

(ert-deftest ewwm-vr-eye/gaze-visualization-defcustom ()
  "Default visualization is dot."
  (should (eq (default-value 'ewwm-vr-gaze-visualization) 'dot)))

(ert-deftest ewwm-vr-eye/gaze-simulate-defcustom ()
  "Default simulate is nil."
  (should (null (default-value 'ewwm-vr-gaze-simulate))))

(ert-deftest ewwm-vr-eye/gaze-confidence-min-defcustom ()
  "Default confidence min is 0.6."
  (should (= (default-value 'ewwm-vr-gaze-confidence-min) 0.6)))

(ert-deftest ewwm-vr-eye/pupil-port-defcustom ()
  "Default pupil port is 50020."
  (should (= (default-value 'ewwm-vr-pupil-port) 50020)))

(ert-deftest ewwm-vr-eye/gaze-mode-line-defcustom ()
  "Default gaze mode-line is t."
  (should (eq (default-value 'ewwm-vr-gaze-mode-line) t)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-vr-eye/gaze-surface-exists ()
  "ewwm-vr-gaze-surface variable exists."
  (should (boundp 'ewwm-vr-gaze-surface)))

(ert-deftest ewwm-vr-eye/gaze-position-exists ()
  "ewwm-vr-gaze-position variable exists."
  (should (boundp 'ewwm-vr-gaze-position)))

(ert-deftest ewwm-vr-eye/gaze-confidence-exists ()
  "ewwm-vr-gaze-confidence variable exists."
  (should (boundp 'ewwm-vr-gaze-confidence)))

(ert-deftest ewwm-vr-eye/gaze-source-active-exists ()
  "ewwm-vr-gaze-source-active variable exists."
  (should (boundp 'ewwm-vr-gaze-source-active)))

(ert-deftest ewwm-vr-eye/gaze-tracking-p-exists ()
  "ewwm-vr-gaze-tracking-p variable exists."
  (should (boundp 'ewwm-vr-gaze-tracking-p)))

(ert-deftest ewwm-vr-eye/gaze-calibrated-p-exists ()
  "ewwm-vr-gaze-calibrated-p variable exists."
  (should (boundp 'ewwm-vr-gaze-calibrated-p)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-vr-eye/calibrate-eyes-interactive ()
  "ewwm-vr-calibrate-eyes is interactive."
  (should (commandp 'ewwm-vr-calibrate-eyes)))

(ert-deftest ewwm-vr-eye/calibrate-eye-point-interactive ()
  "ewwm-vr-calibrate-eye-point is interactive."
  (should (commandp 'ewwm-vr-calibrate-eye-point)))

(ert-deftest ewwm-vr-eye/gaze-health-interactive ()
  "ewwm-vr-gaze-health is interactive."
  (should (commandp 'ewwm-vr-gaze-health)))

(ert-deftest ewwm-vr-eye/set-gaze-source-interactive ()
  "ewwm-vr-set-gaze-source is interactive."
  (should (commandp 'ewwm-vr-set-gaze-source)))

(ert-deftest ewwm-vr-eye/set-gaze-visualization-interactive ()
  "ewwm-vr-set-gaze-visualization is interactive."
  (should (commandp 'ewwm-vr-set-gaze-visualization)))

(ert-deftest ewwm-vr-eye/set-gaze-smoothing-interactive ()
  "ewwm-vr-set-gaze-smoothing is interactive."
  (should (commandp 'ewwm-vr-set-gaze-smoothing)))

(ert-deftest ewwm-vr-eye/gaze-simulate-interactive ()
  "ewwm-vr-gaze-simulate is interactive."
  (should (commandp 'ewwm-vr-gaze-simulate)))

(ert-deftest ewwm-vr-eye/gaze-status-interactive ()
  "ewwm-vr-gaze-status is interactive."
  (should (commandp 'ewwm-vr-gaze-status)))

(ert-deftest ewwm-vr-eye/gaze-at-point-p-exists ()
  "ewwm-vr-gaze-at-point-p function exists."
  (should (fboundp 'ewwm-vr-gaze-at-point-p)))

;; ── Set source validation ───────────────────────────────────

(ert-deftest ewwm-vr-eye/set-gaze-source-invalid ()
  "Setting invalid gaze source signals error."
  (should-error (ewwm-vr-set-gaze-source 'invalid)))

(ert-deftest ewwm-vr-eye/set-visualization-invalid ()
  "Setting invalid visualization signals error."
  (should-error (ewwm-vr-set-gaze-visualization 'invalid)))

;; ── IPC event handlers ──────────────────────────────────────

(ert-deftest ewwm-vr-eye/gaze-data-event ()
  "Gaze data event updates state."
  (let ((ewwm-vr-gaze-surface nil)
        (ewwm-vr-gaze-position nil)
        (ewwm-vr-gaze-confidence 0.0)
        (ewwm-vr-gaze-source-active nil)
        (ewwm-vr-gaze-tracking-p nil)
        (ewwm-vr-eye--gaze-position nil))
    (ewwm-vr-eye--on-gaze-data '(:surface-id 42 :x 100 :y 200 :confidence 0.92 :source :openxr))
    (should (= ewwm-vr-gaze-surface 42))
    (should (equal ewwm-vr-gaze-position '(100 . 200)))
    (should (= ewwm-vr-gaze-confidence 0.92))
    (should (eq ewwm-vr-gaze-source-active :openxr))
    (should ewwm-vr-gaze-tracking-p)))

(ert-deftest ewwm-vr-eye/gaze-target-changed-event ()
  "Gaze target changed event updates state and runs hook."
  (let ((ewwm-vr-gaze-surface nil)
        (hook-args nil))
    (let ((ewwm-vr-gaze-target-change-hook
           (list (lambda (sid prev) (setq hook-args (list sid prev))))))
      (ewwm-vr-eye--on-gaze-target-changed '(:surface-id 42 :prev-surface-id 37))
      (should (= ewwm-vr-gaze-surface 42))
      (should (equal hook-args '(42 37))))))

(ert-deftest ewwm-vr-eye/gaze-fixation-event ()
  "Gaze fixation event runs hook."
  (let ((hook-args nil))
    (let ((ewwm-vr-gaze-fixation-hook
           (list (lambda (sid x y dur) (setq hook-args (list sid x y dur))))))
      (ewwm-vr-eye--on-gaze-fixation '(:surface-id 42 :x 100 :y 200 :duration-ms 450))
      (should (equal hook-args '(42 100 200 450))))))

(ert-deftest ewwm-vr-eye/tracking-lost-event ()
  "Tracking lost event clears tracking state."
  (let ((ewwm-vr-gaze-tracking-p t))
    (let ((ewwm-vr-gaze-tracking-lost-hook nil))
      (ewwm-vr-eye--on-gaze-tracking-lost '(:source :openxr :duration-ms 330))
      (should-not ewwm-vr-gaze-tracking-p))))

(ert-deftest ewwm-vr-eye/calibration-drift-event ()
  "Calibration drift event runs hook."
  (let ((hook-args nil))
    (let ((ewwm-vr-gaze-calibration-drift-hook
           (list (lambda (err) (setq hook-args err)))))
      (ewwm-vr-eye--on-gaze-calibration-drift '(:error-deg 3.5))
      (should (= hook-args 3.5)))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-vr-eye/register-events ()
  "ewwm-vr-eye--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-eye--register-events)
    (should (assq :gaze-data ewwm-ipc--event-handlers))
    (should (assq :gaze-target-changed ewwm-ipc--event-handlers))
    (should (assq :gaze-fixation ewwm-ipc--event-handlers))
    (should (assq :gaze-saccade ewwm-ipc--event-handlers))
    (should (assq :gaze-tracking-lost ewwm-ipc--event-handlers))
    (should (assq :gaze-calibration-drift ewwm-ipc--event-handlers))))

(ert-deftest ewwm-vr-eye/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-eye--register-events)
    (ewwm-vr-eye--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :gaze-data))
                        ewwm-ipc--event-handlers))
               1))))

;; ── Minor mode ──────────────────────────────────────────────

(ert-deftest ewwm-vr-eye/minor-mode-exists ()
  "ewwm-vr-eye-mode minor mode exists."
  (should (fboundp 'ewwm-vr-eye-mode)))

;; ── Mode-line ───────────────────────────────────────────────

(ert-deftest ewwm-vr-eye/mode-line-tracking ()
  "Mode-line shows gaze target when tracking."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-gaze-surface 42)
        (ewwm-vr-eye--in-saccade nil)
        (ewwm-vr-eye--cooldown-remaining 0)
        (ewwm-vr-eye--in-reading nil)
        (ewwm-vr-eye-show-dwell-progress t)
        (ewwm-vr-eye--dwell-surface nil)
        (ewwm-vr-eye--dwell-progress 0.0))
    (should (string= (ewwm-vr-eye-mode-line-string) " [Eye:42]"))))

(ert-deftest ewwm-vr-eye/mode-line-no-target ()
  "Mode-line shows --- when no target."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-gaze-surface nil)
        (ewwm-vr-eye--in-saccade nil)
        (ewwm-vr-eye--cooldown-remaining 0)
        (ewwm-vr-eye--in-reading nil)
        (ewwm-vr-eye-show-dwell-progress t)
        (ewwm-vr-eye--dwell-surface nil)
        (ewwm-vr-eye--dwell-progress 0.0))
    (should (string= (ewwm-vr-eye-mode-line-string) " [Eye:---]"))))

(ert-deftest ewwm-vr-eye/mode-line-not-tracking ()
  "Mode-line nil when not tracking."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-gaze-tracking-p nil)
        (ewwm-vr-eye--in-saccade nil)
        (ewwm-vr-eye--cooldown-remaining 0))
    (should-not (ewwm-vr-eye-mode-line-string))))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-vr-eye/teardown-clears-state ()
  "ewwm-vr-eye-teardown clears all state."
  (let ((ewwm-vr-gaze-surface 42)
        (ewwm-vr-gaze-position '(100 . 200))
        (ewwm-vr-gaze-confidence 0.9)
        (ewwm-vr-gaze-source-active :openxr)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-gaze-calibrated-p t)
        (ewwm-vr-eye--tracking-active t)
        (ewwm-vr-eye--gaze-position '(100 . 200)))
    (ewwm-vr-eye-teardown)
    (should-not ewwm-vr-gaze-surface)
    (should-not ewwm-vr-gaze-position)
    (should (= ewwm-vr-gaze-confidence 0.0))
    (should-not ewwm-vr-gaze-source-active)
    (should-not ewwm-vr-gaze-tracking-p)
    (should-not ewwm-vr-gaze-calibrated-p)
    (should-not ewwm-vr-eye--tracking-active)))

;; ── Hooks exist ─────────────────────────────────────────────

(ert-deftest ewwm-vr-eye/target-change-hook-exists ()
  "Target change hook exists."
  (should (boundp 'ewwm-vr-gaze-target-change-hook)))

(ert-deftest ewwm-vr-eye/fixation-hook-exists ()
  "Fixation hook exists."
  (should (boundp 'ewwm-vr-gaze-fixation-hook)))

(ert-deftest ewwm-vr-eye/tracking-lost-hook-exists ()
  "Tracking lost hook exists."
  (should (boundp 'ewwm-vr-gaze-tracking-lost-hook)))

(ert-deftest ewwm-vr-eye/calibration-drift-hook-exists ()
  "Calibration drift hook exists."
  (should (boundp 'ewwm-vr-gaze-calibration-drift-hook)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-vr-eye/provides-feature ()
  "ewwm-vr-eye provides its feature."
  (should (featurep 'ewwm-vr-eye)))

;;; ewwm-vr-eye-test.el ends here
