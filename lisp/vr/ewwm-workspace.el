;;; ewwm-workspace.el --- Workspace management for EWWM  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(defgroup ewwm-workspace nil
  "EWWM workspace management."
  :group 'ewwm)

(defcustom ewwm-workspace-number 4
  "Number of workspaces."
  :type 'integer)

(defvar ewwm-workspace--list nil
  "List of workspace configurations.")

(defvar ewwm-workspace-current-index 0
  "Index of the current active workspace.")

(defun ewwm-workspace-switch (index)
  "Switch to workspace at INDEX."
  (interactive "nWorkspace: ")
  (when (and (>= index 0) (< index ewwm-workspace-number))
    (setq ewwm-workspace-current-index index)))

(defun ewwm-workspace-list ()
  "Return the list of workspaces."
  ewwm-workspace--list)

(provide 'ewwm-workspace)
;;; ewwm-workspace.el ends here
