;;; ewwm-input.el --- Input handling for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Global key binding dispatch for ewwm. Replaces EXWM's 1215 lines of
;; X11 grab mechanism with IPC-based key grabs from Week 4.
;; Key presses intercepted by the compositor are forwarded via IPC events;
;; Emacs looks up the key in its handler alist and calls the command.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-input nil
  "EWWM input handling."
  :group 'ewwm)

(defcustom ewwm-input-intercept-mode nil
  "Non-nil to intercept ALL keys and route through Emacs.
When nil (default), only grabbed keys are forwarded to Emacs.
Pass-through mode is simpler and avoids latency."
  :type 'boolean
  :group 'ewwm-input)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-input--global-keys nil
  "Alist of (KEY-DESCRIPTION . COMMAND) for global key grabs.
KEY-DESCRIPTION is a string like \"s-r\" or \"C-M-x\".")

(defvar ewwm-input--suppress-focus-sync nil
  "Non-nil to suppress focus sync during programmatic buffer switches.")

;; ── Key registration ─────────────────────────────────────────

(defun ewwm-input-set-key (key command)
  "Bind KEY to COMMAND as a global key grab in EWWM.
KEY should be a key description string (e.g., \"s-r\").
If KEY is a key vector, it is converted to a description string.
Sends a key-grab IPC message to the compositor."
  (let ((key-str (if (stringp key)
                     key
                   (key-description key))))
    ;; Store locally
    (setf (alist-get key-str ewwm-input--global-keys nil nil #'equal) command)
    ;; Send grab to compositor if connected
    (when (fboundp 'ewwm-ipc-send)
      (when (fboundp 'ewwm-ipc-connected-p)
        (when (funcall 'ewwm-ipc-connected-p)
          (funcall 'ewwm-ipc-send `(:type :key-grab :key ,key-str)))))))

(defun ewwm-input-unset-key (key)
  "Remove the global key grab for KEY."
  (let ((key-str (if (stringp key) key (key-description key))))
    (setf (alist-get key-str ewwm-input--global-keys nil 'remove #'equal) nil)
    ;; Send ungrab to compositor
    (when (and (fboundp 'ewwm-ipc-send)
               (fboundp 'ewwm-ipc-connected-p)
               (funcall 'ewwm-ipc-connected-p))
      (funcall 'ewwm-ipc-send `(:type :key-ungrab :key ,key-str)))))

;; ── Key event dispatch ───────────────────────────────────────

(defun ewwm-input--handle-key-event (msg)
  "Handle a :key-pressed IPC event MSG.
Looks up the key in `ewwm-input--global-keys' and calls the command."
  (let* ((key (plist-get msg :key))
         (command (alist-get key ewwm-input--global-keys nil nil #'equal)))
    (if command
        (condition-case err
            (call-interactively command)
          (error
           (message "ewwm-input: error running %s for %s: %s"
                    command key (error-message-string err))))
      (message "ewwm-input: unbound key: %s" key))))

;; ── Register all pending grabs ───────────────────────────────

(defun ewwm-input--register-all-grabs ()
  "Send key-grab IPC messages for all registered keys.
Called after IPC connection is established."
  (when (and (fboundp 'ewwm-ipc-send)
             (fboundp 'ewwm-ipc-connected-p)
             (funcall 'ewwm-ipc-connected-p))
    (dolist (entry ewwm-input--global-keys)
      (funcall 'ewwm-ipc-send `(:type :key-grab :key ,(car entry))))))

;; ── Default key bindings ─────────────────────────────────────

(defun ewwm-input--setup-defaults ()
  "Register default global key bindings.
Does not send IPC — call `ewwm-input--register-all-grabs' after connecting."
  ;; Workspace switching: s-1 through s-9
  (dotimes (i 9)
    (let ((key (format "s-%d" (1+ i)))
          (ws i))
      (setf (alist-get key ewwm-input--global-keys nil nil #'equal)
            (lambda () (interactive)
              (when (fboundp 'ewwm-workspace-switch)
                (funcall 'ewwm-workspace-switch ws))))))
  ;; Move surface to workspace: s-S-1 through s-S-9
  (dotimes (i 9)
    (let ((key (format "s-S-%d" (1+ i)))
          (ws i))
      (setf (alist-get key ewwm-input--global-keys nil nil #'equal)
            (lambda () (interactive)
              (when (and ewwm-surface-id
                         (fboundp 'ewwm-workspace-move-surface))
                (funcall 'ewwm-workspace-move-surface ewwm-surface-id ws))))))
  ;; Layout cycling
  (setf (alist-get "s-SPC" ewwm-input--global-keys nil nil #'equal)
        (lambda () (interactive)
          (when (fboundp 'ewwm-layout-cycle)
            (funcall 'ewwm-layout-cycle))))
  ;; App launcher
  (setf (alist-get "s-&" ewwm-input--global-keys nil nil #'equal)
        (lambda () (interactive)
          (if (fboundp 'ewwm-launch-interactively)
              (funcall 'ewwm-launch-interactively)
            (async-shell-command (read-shell-command "$ ")))))
  ;; Reset
  (setf (alist-get "s-r" ewwm-input--global-keys nil nil #'equal)
        (lambda () (interactive)
          (when (fboundp 'ewwm-reset)
            (funcall 'ewwm-reset)))))

;; ── Intercept mode toggle ────────────────────────────────────

(defun ewwm-input-toggle-intercept ()
  "Toggle between pass-through and intercept input modes."
  (interactive)
  (setq ewwm-input-intercept-mode (not ewwm-input-intercept-mode))
  (message "ewwm-input: %s mode"
           (if ewwm-input-intercept-mode "intercept" "pass-through")))

(provide 'ewwm-input)
;;; ewwm-input.el ends here
