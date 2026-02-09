;;; ewwm-workspace.el --- Workspace management for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Workspace system for ewwm. Uses window-configuration save/restore
;; per workspace (option C from the design plan). Each workspace has
;; its own window configuration and surface list.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-workspace nil
  "EWWM workspace management."
  :group 'ewwm)

(defcustom ewwm-workspace-number 4
  "Number of workspaces."
  :type 'integer
  :group 'ewwm-workspace)

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-workspace-switch-hook nil
  "Hook run after switching workspaces.
Functions are called with two arguments: FROM-INDEX and TO-INDEX.")

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-workspace-current-index 0
  "Index of the current active workspace.")

(defvar ewwm-workspace--configs nil
  "Vector of saved window configurations, one per workspace.")

(defvar ewwm-workspace--names nil
  "Vector of workspace names.")

(defun ewwm-workspace--init ()
  "Initialize workspace data structures."
  (setq ewwm-workspace--configs (make-vector ewwm-workspace-number nil)
        ewwm-workspace--names (make-vector ewwm-workspace-number nil)
        ewwm-workspace-current-index 0)
  ;; Set default names
  (dotimes (i ewwm-workspace-number)
    (aset ewwm-workspace--names i (format "ws-%d" i)))
  ;; Save initial config for workspace 0
  (aset ewwm-workspace--configs 0 (current-window-configuration)))

;; ── Workspace switching ──────────────────────────────────────

(defun ewwm-workspace-switch (index)
  "Switch to workspace at INDEX.
Saves current window configuration, restores target workspace's config,
and sends workspace-switch IPC to compositor."
  (interactive "nWorkspace: ")
  (when (and (>= index 0)
             (< index ewwm-workspace-number)
             (/= index ewwm-workspace-current-index))
    (let ((from ewwm-workspace-current-index))
      ;; Save current window configuration
      (when ewwm-workspace--configs
        (aset ewwm-workspace--configs from (current-window-configuration)))
      ;; Switch
      (setq ewwm-workspace-current-index index)
      ;; Restore target config or build fresh
      (let ((config (aref ewwm-workspace--configs index)))
        (if config
            (condition-case nil
                (set-window-configuration config)
              (error (ewwm-workspace--build-fresh index)))
          (ewwm-workspace--build-fresh index)))
      ;; Send IPC to compositor
      (when (and (fboundp 'ewwm-ipc-send)
                 (fboundp 'ewwm-ipc-connected-p)
                 (funcall 'ewwm-ipc-connected-p))
        (funcall 'ewwm-ipc-send
                 `(:type :workspace-switch :workspace ,index)))
      ;; Run hooks
      (run-hook-with-args 'ewwm-workspace-switch-hook from index)
      (message "ewwm: workspace %d" index))))

(defun ewwm-workspace--build-fresh (index)
  "Build a fresh window layout for workspace INDEX."
  (delete-other-windows)
  (let ((bufs (ewwm--buffers-on-workspace index)))
    (if bufs
        (progn
          (switch-to-buffer (car bufs))
          ;; Apply layout for remaining buffers
          (when (fboundp 'ewwm-layout--apply-current)
            (funcall 'ewwm-layout--apply-current bufs)))
      ;; No surfaces on this workspace, show scratch
      (switch-to-buffer "*scratch*"))))

(defun ewwm-workspace-switch-next ()
  "Switch to the next workspace."
  (interactive)
  (ewwm-workspace-switch
   (mod (1+ ewwm-workspace-current-index) ewwm-workspace-number)))

(defun ewwm-workspace-switch-prev ()
  "Switch to the previous workspace."
  (interactive)
  (ewwm-workspace-switch
   (mod (1- ewwm-workspace-current-index) ewwm-workspace-number)))

;; ── Surface assignment ───────────────────────────────────────

(defun ewwm-workspace-move-surface (surface-id workspace)
  "Move SURFACE-ID to WORKSPACE.
Updates buffer-local variable and sends IPC."
  (interactive
   (list (or (and (derived-mode-p 'ewwm-mode) ewwm-surface-id)
             (read-number "Surface ID: "))
         (read-number "Target workspace: ")))
  (when-let ((buf (ewwm--get-buffer surface-id)))
    (with-current-buffer buf
      (setq ewwm-workspace workspace))
    ;; Send IPC
    (when (and (fboundp 'ewwm-ipc-send)
               (fboundp 'ewwm-ipc-connected-p)
               (funcall 'ewwm-ipc-connected-p))
      (funcall 'ewwm-ipc-send
               `(:type :workspace-move-surface
                       :surface-id ,surface-id
                       :workspace ,workspace)))
    ;; Re-layout current workspace if surface was moved away
    (when (and (/= workspace ewwm-workspace-current-index)
               (fboundp 'ewwm-layout--apply-current))
      (funcall 'ewwm-layout--apply-current
               (ewwm--buffers-on-workspace ewwm-workspace-current-index)))))

;; ── Query ────────────────────────────────────────────────────

(defun ewwm-workspace-list ()
  "Return workspace info as a list of plists."
  (let ((result nil))
    (dotimes (i ewwm-workspace-number)
      (push `(:index ,i
              :name ,(aref ewwm-workspace--names i)
              :current ,(= i ewwm-workspace-current-index)
              :surfaces ,(length (ewwm--buffers-on-workspace i)))
            result))
    (nreverse result)))

(defun ewwm-workspace-name (&optional index)
  "Return the name of workspace INDEX (default: current)."
  (let ((idx (or index ewwm-workspace-current-index)))
    (when (and ewwm-workspace--names
               (>= idx 0)
               (< idx ewwm-workspace-number))
      (aref ewwm-workspace--names idx))))

(defun ewwm-workspace-set-name (index name)
  "Set the name of workspace INDEX to NAME."
  (when (and ewwm-workspace--names
             (>= index 0)
             (< index ewwm-workspace-number))
    (aset ewwm-workspace--names index name)))

(provide 'ewwm-workspace)
;;; ewwm-workspace.el ends here
