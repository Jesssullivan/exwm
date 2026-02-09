;;; ewwm-vr-eye.el --- Eye tracking integration  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(defvar ewwm-vr-eye--gaze-position nil
  "Current gaze position as (X . Y) in screen coordinates.")

(defvar ewwm-vr-eye--tracking-active nil
  "Non-nil when eye tracking is active.")

(defun ewwm-vr-eye-start ()
  "Start eye tracking."
  (interactive)
  (setq ewwm-vr-eye--tracking-active t))

(defun ewwm-vr-eye-stop ()
  "Stop eye tracking."
  (interactive)
  (setq ewwm-vr-eye--tracking-active nil))

(provide 'ewwm-vr-eye)
;;; ewwm-vr-eye.el ends here
