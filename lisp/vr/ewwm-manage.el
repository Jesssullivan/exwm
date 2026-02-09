;;; ewwm-manage.el --- Surface lifecycle for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Surface lifecycle management from creation through destruction.
;; Analogous to EXWM's exwm-manage.el (833 lines).
;; Handles IPC events, applies manage rules, triggers layout.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

;; ── Customization ────────────────────────────────────────────

(defcustom ewwm-manage-rules nil
  "Alist of (PREDICATE . ACTIONS) for surface classification.
PREDICATE is a function taking a surface plist (from IPC event),
returning non-nil if the rule matches.
ACTIONS is a plist of actions to apply:
  :workspace N      - assign to workspace N
  :floating BOOL    - start in floating mode
  :fullscreen BOOL  - start in fullscreen
  :geometry PLIST   - initial geometry (:x X :y Y :w W :h H)"
  :type '(alist :key-type function :value-type plist)
  :group 'ewwm)

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-manage-finish-hook nil
  "Hook run after a surface is managed (buffer created, mode set).
Functions called with the new buffer as current buffer.")

(defvar ewwm-manage-close-hook nil
  "Hook run before a surface is closed.
Functions called with the buffer as current buffer.")

;; ── Surface creation ─────────────────────────────────────────

(defun ewwm-manage--on-create (msg)
  "Handle surface creation from IPC event MSG.
Creates buffer, applies manage rules, assigns workspace, triggers layout."
  (let* ((surface-id (plist-get msg :id))
         (app-id (plist-get msg :app-id))
         (title (plist-get msg :title))
         (x11-p (plist-get msg :x11))
         (buf (ewwm--create-surface-buffer surface-id app-id title)))
    ;; Set X11 fields if applicable
    (when x11-p
      (with-current-buffer buf
        (setq ewwm-x11-p t
              ewwm-x11-class (or (plist-get msg :x11-class) app-id)
              ewwm-x11-instance (or (plist-get msg :x11-instance) ""))))
    ;; Apply manage rules
    (let ((actions (ewwm-manage--match-rules msg)))
      (when actions
        (with-current-buffer buf
          ;; Workspace assignment
          (when-let ((ws (plist-get actions :workspace)))
            (setq ewwm-workspace ws))
          ;; Floating
          (when (plist-get actions :floating)
            (setq ewwm-surface-state 'floating))
          ;; Fullscreen
          (when (plist-get actions :fullscreen)
            (setq ewwm-surface-state 'fullscreen))
          ;; Geometry
          (when-let ((geom (plist-get actions :geometry)))
            (setq ewwm-geometry geom)))))
    ;; Set major mode
    (with-current-buffer buf
      ;; Default workspace to current if not assigned by rule
      (unless (plist-get (ewwm-manage--match-rules msg) :workspace)
        (setq ewwm-workspace
              (if (boundp 'ewwm-workspace-current-index)
                  ewwm-workspace-current-index
                0)))
      (ewwm-mode))
    ;; Run hooks
    (with-current-buffer buf
      (run-hooks 'ewwm-manage-finish-hook))
    ;; Trigger layout if surface is on current workspace
    (when (and (boundp 'ewwm-workspace-current-index)
               (= (buffer-local-value 'ewwm-workspace buf)
                  ewwm-workspace-current-index)
               (fboundp 'ewwm-layout--apply-current))
      (funcall 'ewwm-layout--apply-current
               (ewwm--buffers-on-workspace ewwm-workspace-current-index)))
    buf))

;; ── Surface destruction ──────────────────────────────────────

(defun ewwm-manage--on-destroy (msg)
  "Handle surface destruction from IPC event MSG."
  (let* ((surface-id (plist-get msg :id))
         (buf (ewwm--get-buffer surface-id)))
    (when buf
      ;; Run close hook
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (run-hooks 'ewwm-manage-close-hook)))
      ;; Clean up
      (ewwm--destroy-surface-buffer surface-id)
      ;; Re-layout
      (when (and (boundp 'ewwm-workspace-current-index)
                 (fboundp 'ewwm-layout--apply-current))
        (funcall 'ewwm-layout--apply-current
                 (ewwm--buffers-on-workspace
                  ewwm-workspace-current-index))))))

;; ── Surface title changed ────────────────────────────────────

(defun ewwm-manage--on-title-changed (msg)
  "Handle surface title change from IPC event MSG."
  (ewwm--update-surface-title (plist-get msg :id) (plist-get msg :title)))

;; ── Surface geometry changed ─────────────────────────────────

(defun ewwm-manage--on-geometry-changed (msg)
  "Handle surface geometry change from IPC event MSG."
  (let ((geometry (plist-get msg :geometry)))
    (ewwm--update-surface-geometry (plist-get msg :id) geometry)))

;; ── Manage rule matching ─────────────────────────────────────

(defun ewwm-manage--match-rules (surface-data)
  "Match SURFACE-DATA against `ewwm-manage-rules'.
Returns the actions plist from the first matching rule, or nil."
  (cl-loop for (predicate . actions) in ewwm-manage-rules
           when (condition-case nil
                    (funcall predicate surface-data)
                  (error nil))
           return actions))

;; ── Interactive surface close ────────────────────────────────

(defun ewwm-fullscreen-toggle (&optional surface-id)
  "Toggle fullscreen for SURFACE-ID or current surface."
  (interactive)
  (let ((sid (or surface-id
                 (and (derived-mode-p 'ewwm-mode) ewwm-surface-id))))
    (when sid
      (when-let ((buf (ewwm--get-buffer sid)))
        (with-current-buffer buf
          (setq ewwm-surface-state
                (if (eq ewwm-surface-state 'fullscreen) 'managed 'fullscreen))
          (when (derived-mode-p 'ewwm-mode)
            (ewwm--refresh-buffer-content))))
      (when (and (fboundp 'ewwm-ipc-send)
                 (fboundp 'ewwm-ipc-connected-p)
                 (funcall 'ewwm-ipc-connected-p))
        (funcall 'ewwm-ipc-send
                 `(:type :surface-fullscreen :surface-id ,sid))))))

(provide 'ewwm-manage)
;;; ewwm-manage.el ends here
