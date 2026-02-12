;;; ewwm-qutebrowser.el --- Qutebrowser integration for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Core qutebrowser integration for the Emacs Wayland Window Manager.
;; Provides surface identification, navigation commands, and launch
;; support.  Communicates with qutebrowser via the IPC layer in
;; ewwm-qutebrowser-ipc.el.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-send-sync "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")
(declare-function ewwm-launch "ewwm-launch")
(declare-function ewwm-qutebrowser-ipc-send "ewwm-qutebrowser-ipc")
(declare-function ewwm-qutebrowser-ipc-connected-p
                  "ewwm-qutebrowser-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-qutebrowser nil
  "Qutebrowser integration for EWWM."
  :group 'ewwm)

(defcustom ewwm-qutebrowser-app-id
  "org.qutebrowser.qutebrowser"
  "Wayland app_id used by qutebrowser."
  :type 'string
  :group 'ewwm-qutebrowser)

(defcustom ewwm-qutebrowser-command "qutebrowser"
  "Shell command to launch qutebrowser."
  :type 'string
  :group 'ewwm-qutebrowser)

(defcustom ewwm-qutebrowser-default-url "about:blank"
  "Default URL for new qutebrowser windows."
  :type 'string
  :group 'ewwm-qutebrowser)

(defcustom ewwm-qutebrowser-ipc-method 'fifo
  "IPC method for communicating with qutebrowser.
`fifo': write commands to the qutebrowser FIFO.
`socket': connect via Unix domain socket."
  :type '(choice (const fifo)
                 (const socket))
  :group 'ewwm-qutebrowser)

(defcustom ewwm-qutebrowser-fifo-dir
  (or (getenv "XDG_RUNTIME_DIR") "/tmp")
  "Directory containing the qutebrowser command FIFO.
Defaults to XDG_RUNTIME_DIR or /tmp."
  :type 'string
  :group 'ewwm-qutebrowser)

(defcustom ewwm-qutebrowser-config-dir
  "~/.config/qutebrowser"
  "Path to the qutebrowser configuration directory."
  :type 'string
  :group 'ewwm-qutebrowser)

(defcustom ewwm-qutebrowser-data-dir
  "~/.local/share/qutebrowser"
  "Path to the qutebrowser data directory."
  :type 'string
  :group 'ewwm-qutebrowser)

(defcustom ewwm-qutebrowser-auto-sync-theme t
  "Non-nil to auto-sync Emacs theme to qutebrowser."
  :type 'boolean
  :group 'ewwm-qutebrowser)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-qutebrowser--surfaces nil
  "Alist mapping surface-id to qutebrowser info plist.
Each entry is (SURFACE-ID . (:url URL :title TITLE)).")

(defvar ewwm-qutebrowser--active-surface nil
  "Surface ID of the currently focused qutebrowser window.")

(defvar ewwm-qutebrowser--ipc-connections nil
  "Alist of (SURFACE-ID . IPC-CONNECTION).")

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-qutebrowser-surface-create-hook nil
  "Hook run when a new qutebrowser surface is detected.
Functions receive the surface-id as argument.")

(defvar ewwm-qutebrowser-surface-destroy-hook nil
  "Hook run when a qutebrowser surface is destroyed.
Functions receive the surface-id as argument.")

;; ── Surface management ───────────────────────────────────────

(defun ewwm-qutebrowser--surface-p (surface-data)
  "Return non-nil if SURFACE-DATA belongs to qutebrowser.
SURFACE-DATA is a buffer or plist with an app-id field."
  (cond
   ((bufferp surface-data)
    (and (buffer-live-p surface-data)
         (string= (or (buffer-local-value
                       'ewwm-app-id surface-data)
                      "")
                  ewwm-qutebrowser-app-id)))
   ((and (listp surface-data) (keywordp (car-safe surface-data)))
    (string= (or (plist-get surface-data :app-id) "")
             ewwm-qutebrowser-app-id))
   (t nil)))

(defun ewwm-qutebrowser--surfaces ()
  "Return list of all qutebrowser surface IDs."
  (cl-loop for (sid . buf) in ewwm--surface-buffer-alist
           when (and (buffer-live-p buf)
                     (ewwm-qutebrowser--surface-p buf))
           collect sid))

(defun ewwm-qutebrowser-current-surface ()
  "Return the surface ID of the focused qutebrowser window.
Returns nil if no qutebrowser window is focused."
  (or ewwm-qutebrowser--active-surface
      (when (and (boundp 'ewwm-surface-id)
                 ewwm-surface-id
                 (ewwm-qutebrowser--surface-p
                  (current-buffer)))
        ewwm-surface-id)))

(defun ewwm-qutebrowser--on-surface-create (surface-data)
  "Handle creation of a qutebrowser surface.
SURFACE-DATA is a plist with :surface-id, :app-id, :title."
  (when (ewwm-qutebrowser--surface-p surface-data)
    (let ((sid (plist-get surface-data :surface-id)))
      (setf (alist-get sid ewwm-qutebrowser--surfaces)
            (list :url "" :title
                  (or (plist-get surface-data :title) "")))
      (run-hook-with-args
       'ewwm-qutebrowser-surface-create-hook sid))))

(defun ewwm-qutebrowser--on-surface-destroy (surface-id)
  "Handle destruction of qutebrowser SURFACE-ID."
  (when (alist-get surface-id ewwm-qutebrowser--surfaces)
    (setf (alist-get surface-id ewwm-qutebrowser--surfaces
                     nil 'remove)
          nil)
    (setf (alist-get surface-id ewwm-qutebrowser--ipc-connections
                     nil 'remove)
          nil)
    (when (eql ewwm-qutebrowser--active-surface surface-id)
      (setq ewwm-qutebrowser--active-surface nil))
    (run-hook-with-args
     'ewwm-qutebrowser-surface-destroy-hook surface-id)))

;; ── Command dispatch ─────────────────────────────────────────

(defun ewwm-qutebrowser--send-command (surface-id cmd &rest args)
  "Send CMD with ARGS to qutebrowser for SURFACE-ID.
Dispatches via the configured IPC method."
  (ignore surface-id)
  (let ((full-cmd (if args
                      (mapconcat #'identity (cons cmd args) " ")
                    cmd)))
    (ewwm-qutebrowser-ipc-send full-cmd)))

;; ── Interactive commands ─────────────────────────────────────

(defun ewwm-qutebrowser-open-url (url)
  "Open URL in the focused qutebrowser window."
  (interactive "sURL: ")
  (let ((sid (ewwm-qutebrowser-current-surface)))
    (unless sid
      (user-error "No qutebrowser surface focused"))
    (ewwm-qutebrowser--send-command
     sid ":open" url)))

(defun ewwm-qutebrowser-open-url-new-tab (url)
  "Open URL in a new tab of the focused qutebrowser."
  (interactive "sURL (new tab): ")
  (let ((sid (ewwm-qutebrowser-current-surface)))
    (unless sid
      (user-error "No qutebrowser surface focused"))
    (ewwm-qutebrowser--send-command
     sid ":open" "-t" url)))

(defun ewwm-qutebrowser-back ()
  "Navigate back in the focused qutebrowser window."
  (interactive)
  (let ((sid (ewwm-qutebrowser-current-surface)))
    (unless sid
      (user-error "No qutebrowser surface focused"))
    (ewwm-qutebrowser--send-command sid ":back")))

(defun ewwm-qutebrowser-forward ()
  "Navigate forward in the focused qutebrowser."
  (interactive)
  (let ((sid (ewwm-qutebrowser-current-surface)))
    (unless sid
      (user-error "No qutebrowser surface focused"))
    (ewwm-qutebrowser--send-command sid ":forward")))

(defun ewwm-qutebrowser-reload ()
  "Reload the focused qutebrowser page."
  (interactive)
  (let ((sid (ewwm-qutebrowser-current-surface)))
    (unless sid
      (user-error "No qutebrowser surface focused"))
    (ewwm-qutebrowser--send-command sid ":reload")))

(defun ewwm-qutebrowser-zoom-in ()
  "Zoom in on the focused qutebrowser page."
  (interactive)
  (let ((sid (ewwm-qutebrowser-current-surface)))
    (unless sid
      (user-error "No qutebrowser surface focused"))
    (ewwm-qutebrowser--send-command sid ":zoom-in")))

(defun ewwm-qutebrowser-zoom-out ()
  "Zoom out on the focused qutebrowser page."
  (interactive)
  (let ((sid (ewwm-qutebrowser-current-surface)))
    (unless sid
      (user-error "No qutebrowser surface focused"))
    (ewwm-qutebrowser--send-command sid ":zoom-out")))

(defun ewwm-qutebrowser-zoom-reset ()
  "Reset zoom on the focused qutebrowser page."
  (interactive)
  (let ((sid (ewwm-qutebrowser-current-surface)))
    (unless sid
      (user-error "No qutebrowser surface focused"))
    (ewwm-qutebrowser--send-command sid ":zoom")))

(defun ewwm-qutebrowser-yank-url ()
  "Copy the current URL from focused qutebrowser to kill ring."
  (interactive)
  (let ((sid (ewwm-qutebrowser-current-surface)))
    (unless sid
      (user-error "No qutebrowser surface focused"))
    (let ((info (alist-get sid ewwm-qutebrowser--surfaces)))
      (if info
          (let ((url (plist-get info :url)))
            (when (and url (not (string-empty-p url)))
              (kill-new url)
              (message "Copied: %s" url)))
        (message "No URL known for surface %s" sid)))))

(defun ewwm-qutebrowser-launch (&optional url)
  "Start a new qutebrowser instance.
Optional URL defaults to `ewwm-qutebrowser-default-url'."
  (interactive)
  (let ((target (or url ewwm-qutebrowser-default-url)))
    (if (fboundp 'ewwm-launch)
        (ewwm-launch (format "%s %s"
                             ewwm-qutebrowser-command
                             (shell-quote-argument target)))
      (start-process "qutebrowser" nil
                     ewwm-qutebrowser-command target))))

(provide 'ewwm-qutebrowser)
;;; ewwm-qutebrowser.el ends here
