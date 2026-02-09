;;; ewwm-floating.el --- Floating window support for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Floating window management for surfaces that should not be tiled
;; (dialogs, popups, KeePassXC). Floating surfaces are excluded from
;; the layout algorithm and positioned at compositor-reported geometry.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-floating nil
  "EWWM floating window management."
  :group 'ewwm)

(defcustom ewwm-floating-use-child-frame t
  "Non-nil to display floating surfaces in child frames (Emacs 29+).
When nil, use `pop-to-buffer' in a side window."
  :type 'boolean
  :group 'ewwm-floating)

;; ── Toggle floating ──────────────────────────────────────────

(defun ewwm-floating-toggle (&optional surface-id)
  "Toggle floating mode for SURFACE-ID or current surface.
Sends IPC to compositor and updates buffer state."
  (interactive)
  (let ((sid (or surface-id
                 (and (derived-mode-p 'ewwm-mode) ewwm-surface-id))))
    (when sid
      (when-let ((buf (ewwm--get-buffer sid)))
        (with-current-buffer buf
          (setq ewwm-surface-state
                (if (eq ewwm-surface-state 'floating) 'managed 'floating))
          (when (derived-mode-p 'ewwm-mode)
            (ewwm--refresh-buffer-content))))
      ;; Send IPC
      (when (and (fboundp 'ewwm-ipc-send)
                 (fboundp 'ewwm-ipc-connected-p)
                 (funcall 'ewwm-ipc-connected-p))
        (funcall 'ewwm-ipc-send
                 `(:type :surface-float :surface-id ,sid)))
      ;; Re-layout current workspace (floating surface removed/added to tiling)
      (when (and (boundp 'ewwm-workspace-current-index)
                 (fboundp 'ewwm-layout--apply-current))
        (funcall 'ewwm-layout--apply-current
                 (ewwm--buffers-on-workspace
                  ewwm-workspace-current-index))))))

;; ── Floating surface queries ─────────────────────────────────

(defun ewwm-floating-list ()
  "Return list of all floating surface buffers."
  (cl-remove-if-not
   (lambda (buf)
     (and (buffer-live-p buf)
          (eq (buffer-local-value 'ewwm-surface-state buf) 'floating)))
   (ewwm--all-buffers)))

(defun ewwm-floating-p (&optional buffer)
  "Return non-nil if BUFFER (default: current) is floating."
  (let ((buf (or buffer (current-buffer))))
    (and (buffer-live-p buf)
         (eq (buffer-local-value 'ewwm-surface-state buf) 'floating))))

;; ── Display floating buffers ─────────────────────────────────

(defun ewwm-floating--display (buffer)
  "Display floating BUFFER appropriately.
Uses child frame if available and enabled, otherwise side window."
  (if (and ewwm-floating-use-child-frame
           (fboundp 'make-frame)
           (display-graphic-p))
      (ewwm-floating--display-child-frame buffer)
    (ewwm-floating--display-side-window buffer)))

(defun ewwm-floating--display-child-frame (buffer)
  "Display floating BUFFER in a child frame."
  (let* ((geom (buffer-local-value 'ewwm-geometry buffer))
         (x (or (plist-get geom :x) 100))
         (y (or (plist-get geom :y) 100))
         (w (or (plist-get geom :w) 400))
         (h (or (plist-get geom :h) 300)))
    (display-buffer buffer
                    `(display-buffer-in-child-frame
                      (child-frame-parameters
                       (left . ,x)
                       (top . ,y)
                       (width . ,(/ w (frame-char-width)))
                       (height . ,(/ h (frame-char-height))))))))

(defun ewwm-floating--display-side-window (buffer)
  "Display floating BUFFER in a side window."
  (display-buffer buffer
                  '(display-buffer-in-side-window
                    (side . right)
                    (slot . 0)
                    (window-width . 0.4))))

;; ── Interactive move/resize ──────────────────────────────────

(defun ewwm-floating-move ()
  "Enter interactive move mode for the current floating surface.
Compositor tracks pointer and moves surface."
  (interactive)
  (when (and ewwm-surface-id (eq ewwm-surface-state 'floating))
    (when (and (fboundp 'ewwm-ipc-send)
               (fboundp 'ewwm-ipc-connected-p)
               (funcall 'ewwm-ipc-connected-p))
      (funcall 'ewwm-ipc-send
               `(:type :surface-move-interactive :surface-id ,ewwm-surface-id)))
    (message "ewwm: move mode (ESC or click to confirm)")))

(defun ewwm-floating-resize ()
  "Enter interactive resize mode for the current floating surface."
  (interactive)
  (when (and ewwm-surface-id (eq ewwm-surface-state 'floating))
    (when (and (fboundp 'ewwm-ipc-send)
               (fboundp 'ewwm-ipc-connected-p)
               (funcall 'ewwm-ipc-connected-p))
      (funcall 'ewwm-ipc-send
               `(:type :surface-resize-interactive :surface-id ,ewwm-surface-id)))
    (message "ewwm: resize mode (ESC or click to confirm)")))

(provide 'ewwm-floating)
;;; ewwm-floating.el ends here
