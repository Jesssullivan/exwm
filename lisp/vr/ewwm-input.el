;;; ewwm-input.el --- Input handling for EWWM  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(defvar ewwm-input-global-keys nil
  "Global key bindings for EWWM.")

(defun ewwm-input-set-key (key command)
  "Bind KEY to COMMAND globally in EWWM."
  (push (cons key command) ewwm-input-global-keys))

(provide 'ewwm-input)
;;; ewwm-input.el ends here
