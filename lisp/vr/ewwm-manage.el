;;; ewwm-manage.el --- Surface lifecycle for EWWM  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(defun ewwm-manage--on-surface-create (surface-id &optional title class)
  "Handle creation of surface SURFACE-ID with TITLE and CLASS."
  (let ((buf (generate-new-buffer (or title (format "*surface-%d*" surface-id)))))
    (ewwm--set-buffer surface-id buf)
    (when class
      (with-current-buffer buf
        (setq-local ewwm-class-name class)))
    buf))

(defun ewwm-manage--on-surface-destroy (surface-id)
  "Handle destruction of SURFACE-ID."
  (when-let ((buf (ewwm--get-buffer surface-id)))
    (ewwm--remove-buffer surface-id)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(provide 'ewwm-manage)
;;; ewwm-manage.el ends here
