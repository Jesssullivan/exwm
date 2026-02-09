;;; ewwm-core.el --- Core definitions for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Core data structures for the Emacs Wayland Window Manager.
;; Mirrors exwm-core.el but uses Wayland surface IDs instead of X window IDs.

;;; Code:

(require 'cl-lib)

(defgroup ewwm nil
  "Emacs Wayland Window Manager."
  :group 'environment)

(defvar ewwm--surface-buffer-alist nil
  "Alist of (<Wayland surface ID> . <Emacs buffer>).
Analogous to `exwm--id-buffer-alist' in EXWM.")

(defvar ewwm--compositor-process nil
  "Process object for the compositor subprocess.")

(defvar ewwm--ipc-connection nil
  "Network process for IPC with the compositor.")

(defvar ewwm--compositor-socket nil
  "Path to the compositor IPC Unix domain socket.")

;; Surface-buffer alist CRUD operations

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

(provide 'ewwm-core)
;;; ewwm-core.el ends here
