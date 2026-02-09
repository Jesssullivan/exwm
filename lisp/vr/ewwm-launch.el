;;; ewwm-launch.el --- Application launcher for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Application launcher for ewwm. Starts Wayland clients with correct
;; environment variables and tracks launched processes.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-launch nil
  "EWWM application launcher."
  :group 'ewwm)

(defcustom ewwm-launch-favorites
  '("foot" "qutebrowser" "keepassxc" "emacsclient -c" "firefox")
  "List of favorite application commands for quick launch."
  :type '(repeat string)
  :group 'ewwm-launch)

(defcustom ewwm-launch-env-vars
  '(("GDK_BACKEND" . "wayland")
    ("QT_QPA_PLATFORM" . "wayland")
    ("MOZ_ENABLE_WAYLAND" . "1"))
  "Extra environment variables set when launching Wayland clients."
  :type '(alist :key-type string :value-type string)
  :group 'ewwm-launch)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-launch--processes nil
  "Alist of (COMMAND . PROCESS) for tracked launched processes.")

;; ── Launch functions ─────────────────────────────────────────

(defun ewwm-launch (command)
  "Launch COMMAND as a Wayland client.
Sets WAYLAND_DISPLAY, XDG_RUNTIME_DIR, and any extra env vars."
  (interactive "sLaunch: ")
  (let* ((process-environment (copy-sequence process-environment))
         (display (getenv "WAYLAND_DISPLAY")))
    ;; Ensure Wayland env is set
    (when display
      (setenv "WAYLAND_DISPLAY" display))
    ;; Set extra env vars
    (dolist (pair ewwm-launch-env-vars)
      (setenv (car pair) (cdr pair)))
    ;; Launch
    (let* ((name (format "ewwm-app:%s" (car (split-string command))))
           (buf-name (format " *%s*" name))
           (proc (start-process-shell-command name buf-name command)))
      ;; Track
      (push (cons command proc) ewwm-launch--processes)
      ;; Set up sentinel for cleanup
      (set-process-sentinel proc #'ewwm-launch--sentinel)
      (message "ewwm: launched %s" command)
      proc)))

(defun ewwm-launch--sentinel (proc event)
  "Handle process PROC exit EVENT."
  (let ((status (string-trim event)))
    (when (string-match-p "\\(finished\\|exited\\|killed\\)" status)
      ;; Remove from tracking
      (setq ewwm-launch--processes
            (cl-remove-if (lambda (entry) (eq (cdr entry) proc))
                          ewwm-launch--processes)))))

;; ── Interactive launchers ────────────────────────────────────

(defun ewwm-launch-interactively ()
  "Prompt for a command and launch it as a Wayland client."
  (interactive)
  (let ((cmd (read-shell-command "ewwm launch: ")))
    (ewwm-launch cmd)))

(defun ewwm-launch-favorite ()
  "Launch a favorite application from `ewwm-launch-favorites'."
  (interactive)
  (let ((cmd (completing-read "Launch favorite: " ewwm-launch-favorites nil t)))
    (ewwm-launch cmd)))

;; ── Process management ───────────────────────────────────────

(defun ewwm-launch-list ()
  "Return list of currently running launched processes."
  (cl-remove-if-not
   (lambda (entry)
     (process-live-p (cdr entry)))
   ewwm-launch--processes))

(defun ewwm-launch-kill-all ()
  "Kill all launched application processes."
  (interactive)
  (dolist (entry ewwm-launch--processes)
    (when (process-live-p (cdr entry))
      (kill-process (cdr entry))))
  (setq ewwm-launch--processes nil)
  (message "ewwm: killed all launched apps"))

(provide 'ewwm-launch)
;;; ewwm-launch.el ends here
