;;; ewwm-ipc.el --- IPC client for EWWM compositor  -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(defun ewwm-ipc-connect ()
  "Connect to the compositor IPC socket."
  (when ewwm--compositor-socket
    (setq ewwm--ipc-connection
          (make-network-process
           :name "ewwm-ipc"
           :family 'local
           :service ewwm--compositor-socket
           :noquery t))))

(defun ewwm-ipc-send (msg)
  "Send MSG to the compositor via IPC."
  (when (and ewwm--ipc-connection
             (process-live-p ewwm--ipc-connection))
    (process-send-string ewwm--ipc-connection
                         (format "%S\n" msg))))

(defun ewwm-ipc-disconnect ()
  "Disconnect from the compositor IPC socket."
  (when (and ewwm--ipc-connection
             (process-live-p ewwm--ipc-connection))
    (delete-process ewwm--ipc-connection)
    (setq ewwm--ipc-connection nil)))

(provide 'ewwm-ipc)
;;; ewwm-ipc.el ends here
