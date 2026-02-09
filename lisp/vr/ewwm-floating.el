;;; ewwm-floating.el --- Floating window support for EWWM  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(defvar-local ewwm-floating--is-floating nil
  "Non-nil if this surface is floating.")

(defun ewwm-floating-toggle (surface-id)
  "Toggle floating mode for SURFACE-ID."
  (when-let ((buf (ewwm--get-buffer surface-id)))
    (with-current-buffer buf
      (setq ewwm-floating--is-floating (not ewwm-floating--is-floating)))))

(provide 'ewwm-floating)
;;; ewwm-floating.el ends here
