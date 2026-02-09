;;; ewwm-vr-input.el --- VR input handling  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(defvar ewwm-vr-input--controllers nil
  "List of active VR controllers.")

(defvar ewwm-vr-input--hand-tracking nil
  "Non-nil when hand tracking is available.")

(defun ewwm-vr-input-controllers ()
  "Return the list of active controllers."
  ewwm-vr-input--controllers)

(provide 'ewwm-vr-input)
;;; ewwm-vr-input.el ends here
