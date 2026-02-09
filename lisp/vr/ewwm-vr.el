;;; ewwm-vr.el --- VR mode entry point  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(defvar ewwm-vr--active nil
  "Non-nil when VR mode is active.")

(defun ewwm-vr-mode ()
  "Toggle VR mode."
  (interactive)
  (setq ewwm-vr--active (not ewwm-vr--active))
  (message "EWWM VR mode %s" (if ewwm-vr--active "enabled" "disabled")))

(provide 'ewwm-vr)
;;; ewwm-vr.el ends here
