;;; ewwm-core.el --- Core definitions for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Core data structures for the Emacs Wayland Window Manager.
;; Central abstraction: surface-as-buffer model where each Wayland surface
;; is represented as an Emacs buffer with buffer-local metadata.
;; Mirrors exwm-core.el but uses Wayland surface IDs instead of X window IDs.

;;; Code:

(require 'cl-lib)

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm nil
  "Emacs Wayland Window Manager."
  :group 'environment)

;; ── Global state ─────────────────────────────────────────────

(defvar ewwm--surface-buffer-alist nil
  "Alist of (<Wayland surface ID> . <Emacs buffer>).
Analogous to `exwm--id-buffer-alist' in EXWM.")

(defvar ewwm--compositor-process nil
  "Process object for the compositor subprocess.")

(defvar ewwm--ipc-connection nil
  "Network process for IPC with the compositor.")

(defvar ewwm--compositor-socket nil
  "Path to the compositor IPC Unix domain socket.")

;; ── Buffer-local variables ───────────────────────────────────
;; These are set on each ewwm-managed buffer.

(defvar-local ewwm-surface-id nil
  "Wayland surface ID for this buffer (integer).")
(put 'ewwm-surface-id 'permanent-local t)

(defvar-local ewwm-app-id nil
  "Application ID (string, from xdg_toplevel set_app_id).
For example: \"org.qutebrowser.qutebrowser\" or \"foot\".")
(put 'ewwm-app-id 'permanent-local t)

(defvar-local ewwm-title ""
  "Surface title (string, from xdg_toplevel set_title).")
(put 'ewwm-title 'permanent-local t)

(defvar-local ewwm-class-name nil
  "Window class name (alias for `ewwm-app-id', EXWM compatibility).")
(put 'ewwm-class-name 'permanent-local t)

(defvar-local ewwm-instance-name nil
  "Window instance name (EXWM compatibility).")
(put 'ewwm-instance-name 'permanent-local t)

(defvar-local ewwm-surface-state 'managed
  "Surface state: `managed', `floating', or `fullscreen'.")
(put 'ewwm-surface-state 'permanent-local t)

(defvar-local ewwm-geometry nil
  "Surface geometry plist (:x N :y N :w N :h N).")
(put 'ewwm-geometry 'permanent-local t)

(defvar-local ewwm-workspace 0
  "Workspace index this surface belongs to.")
(put 'ewwm-workspace 'permanent-local t)

(defvar-local ewwm-x11-p nil
  "Non-nil if this surface is an XWayland (X11) surface.")
(put 'ewwm-x11-p 'permanent-local t)

(defvar-local ewwm-x11-class nil
  "X11 WM_CLASS for XWayland surfaces.")
(put 'ewwm-x11-class 'permanent-local t)

(defvar-local ewwm-x11-instance nil
  "X11 WM_CLASS instance for XWayland surfaces.")
(put 'ewwm-x11-instance 'permanent-local t)

;; ── Surface-buffer alist CRUD ────────────────────────────────

(defun ewwm--get-buffer (surface-id)
  "Get the Emacs buffer for SURFACE-ID."
  (alist-get surface-id ewwm--surface-buffer-alist))

(defun ewwm--set-buffer (surface-id buffer)
  "Associate SURFACE-ID with BUFFER."
  (setf (alist-get surface-id ewwm--surface-buffer-alist) buffer))

(defun ewwm--remove-buffer (surface-id)
  "Remove the association for SURFACE-ID."
  (setf (alist-get surface-id ewwm--surface-buffer-alist nil 'remove) nil))

(defun ewwm--all-surfaces ()
  "Return list of all managed surface IDs."
  (mapcar #'car ewwm--surface-buffer-alist))

(defun ewwm--all-buffers ()
  "Return list of all managed buffers."
  (mapcar #'cdr ewwm--surface-buffer-alist))

(defun ewwm--surface-count ()
  "Return the number of managed surfaces."
  (length ewwm--surface-buffer-alist))

(defun ewwm--clear-surfaces ()
  "Remove all surface-buffer associations."
  (setq ewwm--surface-buffer-alist nil))

;; ── Surface buffer creation ──────────────────────────────────

(defun ewwm--create-surface-buffer (surface-id app-id title)
  "Create a buffer for SURFACE-ID with APP-ID and TITLE.
Sets buffer-local variables and returns the buffer."
  (let* ((name (format "*ewwm:%s*" (or title app-id
                                        (format "surface-%d" surface-id))))
         (buf (generate-new-buffer name)))
    (with-current-buffer buf
      (setq ewwm-surface-id surface-id
            ewwm-app-id (or app-id "")
            ewwm-title (or title "")
            ewwm-class-name (or app-id "")
            ewwm-instance-name (or app-id "")
            ewwm-surface-state 'managed
            ewwm-geometry nil
            ewwm-workspace 0))
    ;; Register in global alist
    (ewwm--set-buffer surface-id buf)
    buf))

(defun ewwm--destroy-surface-buffer (surface-id)
  "Destroy the buffer for SURFACE-ID.
Removes from alist and kills the buffer."
  (when-let ((buf (ewwm--get-buffer surface-id)))
    (ewwm--remove-buffer surface-id)
    (when (buffer-live-p buf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(defun ewwm--update-surface-title (surface-id title)
  "Update the title of SURFACE-ID to TITLE."
  (when-let ((buf (ewwm--get-buffer surface-id)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq ewwm-title (or title ""))
        ;; Rename buffer
        (let ((new-name (format "*ewwm:%s*" (or title ewwm-app-id
                                                 (format "surface-%d" surface-id)))))
          (ignore-errors
            (rename-buffer new-name t)))
        ;; Update buffer content if ewwm-mode is active
        (when (derived-mode-p 'ewwm-mode)
          (ewwm--refresh-buffer-content))))))

(defun ewwm--update-surface-geometry (surface-id geometry)
  "Update the geometry of SURFACE-ID to GEOMETRY plist."
  (when-let ((buf (ewwm--get-buffer surface-id)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq ewwm-geometry geometry)))))

(defun ewwm--refresh-buffer-content ()
  "Refresh the informational content of the current ewwm buffer.
Should be called in the context of an ewwm-mode buffer."
  (when (and (derived-mode-p 'ewwm-mode) ewwm-surface-id)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Wayland surface: %s\n" ewwm-title))
      (insert (format "App ID: %s\n" ewwm-app-id))
      (insert (format "Surface ID: %d\n" ewwm-surface-id))
      (insert (format "Workspace: %d\n" ewwm-workspace))
      (insert (format "State: %s\n" ewwm-surface-state))
      (when ewwm-geometry
        (insert (format "Geometry: %dx%d+%d+%d\n"
                        (plist-get ewwm-geometry :w)
                        (plist-get ewwm-geometry :h)
                        (plist-get ewwm-geometry :x)
                        (plist-get ewwm-geometry :y))))
      (when ewwm-x11-p
        (insert (format "X11 class: %s\n" (or ewwm-x11-class "")))
        (insert (format "X11 instance: %s\n" (or ewwm-x11-instance ""))))
      (insert "\nThis buffer represents a Wayland surface managed by ewwm.\n")
      (insert "Use standard Emacs buffer/window commands to manage it.\n"))))

;; ── Workspace query helpers ──────────────────────────────────

(defun ewwm--buffers-on-workspace (workspace)
  "Return list of ewwm buffers on WORKSPACE."
  (cl-remove-if-not
   (lambda (buf)
     (and (buffer-live-p buf)
          (with-current-buffer buf
            (and ewwm-surface-id
                 (eql ewwm-workspace workspace)))))
   (ewwm--all-buffers)))

(defun ewwm--surface-buffer-p (buffer)
  "Return non-nil if BUFFER is an ewwm surface buffer."
  (and (buffer-live-p buffer)
       (buffer-local-value 'ewwm-surface-id buffer)))

;; ── ewwm-mode: major mode for managed surfaces ──────────────

(defvar ewwm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'ewwm-mode-close-surface)
    (define-key map "f" #'ewwm-mode-toggle-floating)
    (define-key map "F" #'ewwm-mode-toggle-fullscreen)
    (define-key map "m" #'ewwm-mode-move-to-workspace)
    (define-key map "i" #'ewwm-mode-surface-info)
    map)
  "Keymap for `ewwm-mode'.")

(define-derived-mode ewwm-mode special-mode "EWWM"
  "Major mode for Wayland surfaces managed by EWWM.
Each buffer in this mode represents a Wayland surface.
\\{ewwm-mode-map}"
  :group 'ewwm
  (setq mode-line-format
        '(" " ewwm-title
          " [" ewwm-app-id "]"
          " ws:" (:eval (number-to-string ewwm-workspace))
          " " (:eval (symbol-name ewwm-surface-state))
          (:eval (if ewwm-x11-p " [X11]" ""))
          " "
          mode-line-misc-info))
  (ewwm--refresh-buffer-content))

(defvar ewwm-mode-hook nil
  "Hook run after entering `ewwm-mode'.")

(defun ewwm-mode-close-surface ()
  "Close the Wayland surface for the current buffer."
  (interactive)
  (when ewwm-surface-id
    ;; ewwm-ipc.el provides ewwm-surface-close
    (when (fboundp 'ewwm-surface-close)
      (funcall 'ewwm-surface-close ewwm-surface-id))))

(defun ewwm-mode-toggle-floating ()
  "Toggle floating mode for the current surface."
  (interactive)
  (when ewwm-surface-id
    (when (fboundp 'ewwm-floating-toggle)
      (funcall 'ewwm-floating-toggle ewwm-surface-id))))

(defun ewwm-mode-toggle-fullscreen ()
  "Toggle fullscreen mode for the current surface."
  (interactive)
  (when ewwm-surface-id
    (when (fboundp 'ewwm-fullscreen-toggle)
      (funcall 'ewwm-fullscreen-toggle ewwm-surface-id))))

(defun ewwm-mode-move-to-workspace (n)
  "Move the current surface to workspace N."
  (interactive "nMove to workspace: ")
  (when ewwm-surface-id
    (when (fboundp 'ewwm-workspace-move-surface)
      (funcall 'ewwm-workspace-move-surface ewwm-surface-id n))))

(defun ewwm-mode-surface-info ()
  "Display detailed info about the current surface."
  (interactive)
  (when ewwm-surface-id
    (message "Surface %d: %s [%s] ws:%d state:%s%s"
             ewwm-surface-id ewwm-title ewwm-app-id
             ewwm-workspace ewwm-surface-state
             (if ewwm-x11-p " (X11)" ""))))

(provide 'ewwm-core)
;;; ewwm-core.el ends here
