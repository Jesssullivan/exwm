;;; ewwm-layout.el --- Layout management for EWWM  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(defcustom ewwm-layout-default 'tiling
  "Default layout mode."
  :type '(choice (const tiling) (const monocle) (const grid) (const floating))
  :group 'ewwm)

(defvar ewwm-layout--current 'tiling
  "Current layout mode.")

(defun ewwm-layout-set (layout)
  "Set the current LAYOUT mode."
  (setq ewwm-layout--current layout))

(defun ewwm-layout-current ()
  "Return the current layout mode."
  ewwm-layout--current)

(provide 'ewwm-layout)
;;; ewwm-layout.el ends here
