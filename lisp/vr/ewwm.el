;;; ewwm.el --- Emacs Wayland Window Manager  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Main entry point for ewwm. Initializes the compositor, connects IPC,
;; sets up event handlers, and orchestrates all submodules.
;; Analogous to exwm.el which requires and initializes all EXWM submodules.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-ipc)
(require 'ewwm-workspace)
(require 'ewwm-layout)
(require 'ewwm-input)
(require 'ewwm-manage)
(require 'ewwm-floating)
(require 'ewwm-launch)
(require 'ewwm-vr)

;; ── Customization ────────────────────────────────────────────

(defcustom ewwm-compositor-command "ewwm-compositor"
  "Command to start the compositor."
  :type 'string
  :group 'ewwm)

(defcustom ewwm-compositor-args '("--backend" "auto")
  "Arguments passed to the compositor command."
  :type '(repeat string)
  :group 'ewwm)

(defcustom ewwm-focus-policy 'follow-emacs
  "Focus synchronization policy.
`follow-emacs': buffer selection in Emacs focuses compositor surface.
`follow-compositor': compositor focus change updates Emacs buffer.
`manual': no automatic focus sync."
  :type '(choice (const follow-emacs)
                 (const follow-compositor)
                 (const manual))
  :group 'ewwm)

(defcustom ewwm-compositor-startup-timeout 5
  "Seconds to wait for compositor IPC socket to appear."
  :type 'number
  :group 'ewwm)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm--initialized nil
  "Non-nil if ewwm has been initialized.")

;; ── IPC event handler wiring ─────────────────────────────────
;; Replace the stub handlers in ewwm-ipc.el with full implementations.

(defun ewwm--wire-event-handlers ()
  "Wire IPC event handlers to full implementations."
  ;; Override the stub handlers with manage module handlers
  (fset 'ewwm-ipc--on-surface-created #'ewwm-manage--on-create)
  (fset 'ewwm-ipc--on-surface-destroyed #'ewwm-manage--on-destroy)
  (fset 'ewwm-ipc--on-surface-title-changed #'ewwm-manage--on-title-changed)
  (fset 'ewwm-ipc--on-surface-geometry-changed #'ewwm-manage--on-geometry-changed)
  (fset 'ewwm-ipc--on-key-pressed #'ewwm-input--handle-key-event)
  (fset 'ewwm-ipc--on-surface-focused #'ewwm--on-compositor-focus)
  (fset 'ewwm-ipc--on-workspace-changed #'ewwm--on-compositor-workspace-changed)
  ;; Layer-shell usable area changes
  (fset 'ewwm-ipc--on-output-usable-area-changed #'ewwm-layout--on-usable-area-changed))

;; ── Focus management (Stage 5.8) ────────────────────────────

(defun ewwm--setup-focus-sync ()
  "Set up bidirectional focus synchronization."
  (when (memq ewwm-focus-policy '(follow-emacs follow-compositor))
    ;; Emacs -> Compositor: when user switches to ewwm buffer
    (add-hook 'window-selection-change-functions
              #'ewwm--on-window-selection-change)
    ;; Compositor -> Emacs: handled via IPC event (ewwm--on-compositor-focus)
    ))

(defun ewwm--teardown-focus-sync ()
  "Remove focus synchronization hooks."
  (remove-hook 'window-selection-change-functions
               #'ewwm--on-window-selection-change))

(defun ewwm--on-window-selection-change (_frame)
  "Handle Emacs window selection change for focus sync."
  (unless ewwm-input--suppress-focus-sync
    (when (eq ewwm-focus-policy 'follow-emacs)
      (let ((buf (window-buffer (selected-window))))
        (when (ewwm--surface-buffer-p buf)
          (let ((sid (buffer-local-value 'ewwm-surface-id buf)))
            (when (and sid (ewwm-ipc-connected-p))
              (let ((ewwm-input--suppress-focus-sync t))
                (ewwm-ipc-send
                 `(:type :surface-focus :surface-id ,sid))))))))))

(defun ewwm--on-compositor-focus (msg)
  "Handle compositor focus change event MSG."
  (when (memq ewwm-focus-policy '(follow-compositor follow-emacs))
    (let* ((surface-id (plist-get msg :id))
           (buf (ewwm--get-buffer surface-id)))
      (when (and buf (buffer-live-p buf))
        (let ((ewwm-input--suppress-focus-sync t))
          (switch-to-buffer buf))))))

(defun ewwm--on-compositor-workspace-changed (msg)
  "Handle compositor workspace change event MSG."
  (let ((workspace (plist-get msg :workspace)))
    (when (and workspace (boundp 'ewwm-workspace-current-index))
      (setq ewwm-workspace-current-index workspace)
      (message "ewwm: workspace %d (compositor)" workspace))))

;; ── Usable area handler ──────────────────────────────────────

(defun ewwm--on-usable-area-changed (msg)
  "Handle output usable area change event MSG."
  (let ((geometry (plist-get msg :geometry)))
    (when (and geometry (fboundp 'ewwm-layout--set-usable-area))
      (ewwm-layout--set-usable-area geometry))))

;; ── Initialization ───────────────────────────────────────────

(defun ewwm-init (&optional skip-compositor)
  "Initialize ewwm: start compositor, connect IPC, set up modules.
When SKIP-COMPOSITOR is non-nil, skip starting the compositor
\(useful when compositor is already running)."
  (interactive "P")
  (when ewwm--initialized
    (message "ewwm: already initialized, use ewwm-restart to reinit")
    (cl-return-from ewwm-init))
  ;; Step 1: Start compositor
  (unless skip-compositor
    (ewwm--start-compositor))
  ;; Step 2: Wait for IPC socket
  (unless (ewwm--wait-for-socket)
    (error "ewwm: compositor IPC socket did not appear within timeout"))
  ;; Step 3: Initialize modules
  (ewwm-workspace--init)
  (ewwm-input--setup-defaults)
  (ewwm-vr-init)
  ;; Step 4: Wire event handlers
  (ewwm--wire-event-handlers)
  ;; Step 5: Connect IPC
  (unless (ewwm-ipc-connect)
    (error "ewwm: failed to connect to compositor IPC"))
  ;; Step 6: Register key grabs
  (ewwm-input--register-all-grabs)
  ;; Step 7: Query initial surfaces
  (condition-case nil
      (let ((surfaces (ewwm-surface-list)))
        (dolist (surface surfaces)
          (ewwm-manage--on-create surface)))
    (error nil))
  ;; Step 8: Set up focus sync
  (ewwm--setup-focus-sync)
  ;; Step 9: Apply initial layout
  (ewwm-layout--apply-current
   (ewwm--buffers-on-workspace ewwm-workspace-current-index))
  (setq ewwm--initialized t)
  (message "ewwm: initialized"))

(defun ewwm--start-compositor ()
  "Start the compositor subprocess."
  (when (and ewwm--compositor-process
             (process-live-p ewwm--compositor-process))
    (message "ewwm: compositor already running")
    (cl-return-from ewwm--start-compositor))
  (let ((buf (get-buffer-create "*ewwm-compositor*")))
    (with-current-buffer buf (erase-buffer))
    (setq ewwm--compositor-process
          (apply #'start-process
                 "ewwm-compositor" buf
                 ewwm-compositor-command
                 ewwm-compositor-args))
    (message "ewwm: compositor started (pid %d)"
             (process-id ewwm--compositor-process))))

(defun ewwm--wait-for-socket ()
  "Wait for the IPC socket file to appear. Return non-nil if found."
  (let ((socket-path (ewwm-ipc--socket-path))
        (deadline (+ (float-time) ewwm-compositor-startup-timeout)))
    (while (and (< (float-time) deadline)
                (not (file-exists-p socket-path)))
      (sleep-for 0.1))
    (file-exists-p socket-path)))

;; ── Shutdown ─────────────────────────────────────────────────

(defun ewwm-exit ()
  "Shut down ewwm: disconnect IPC, kill compositor, clean up."
  (interactive)
  (ewwm--teardown-focus-sync)
  ;; Clean up VR
  (ewwm-vr-teardown)
  ;; Disconnect IPC
  (ewwm-ipc-disconnect)
  ;; Kill all launched apps
  (ewwm-launch-kill-all)
  ;; Kill surface buffers
  (dolist (buf (ewwm--all-buffers))
    (when (buffer-live-p buf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))))
  (ewwm--clear-surfaces)
  ;; Kill compositor
  (when (and ewwm--compositor-process
             (process-live-p ewwm--compositor-process))
    (kill-process ewwm--compositor-process))
  (setq ewwm--compositor-process nil
        ewwm--initialized nil)
  (message "ewwm: exited"))

(defun ewwm-restart ()
  "Restart ewwm."
  (interactive)
  (ewwm-exit)
  (sit-for 1)
  (ewwm-init))

(defun ewwm-reset ()
  "Reset ewwm to default state (re-layout, re-register keys)."
  (interactive)
  (ewwm-input--register-all-grabs)
  (ewwm-layout-set ewwm-layout-default)
  (message "ewwm: reset"))

;; ── Global minor mode ────────────────────────────────────────

(define-minor-mode ewwm-global-mode
  "Global minor mode for the Emacs Wayland Window Manager."
  :global t
  :lighter " EWWM"
  :group 'ewwm
  (if ewwm-global-mode
      (ewwm-init t)
    (ewwm-exit)))

(provide 'ewwm)
;;; ewwm.el ends here
