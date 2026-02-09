;;; ewwm-manage-test.el --- Tests for ewwm-manage  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-manage)

(ert-deftest ewwm-manage-test/surface-create ()
  "Surface create produces a buffer and registers it."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm-manage--on-surface-create 1 "Firefox" "firefox")))
      (unwind-protect
          (progn
            (should (buffer-live-p buf))
            (should (eq buf (ewwm--get-buffer 1))))
        (ewwm-manage--on-surface-destroy 1)))))

(ert-deftest ewwm-manage-test/surface-destroy ()
  "Surface destroy removes buffer and cleans alist."
  (let ((ewwm--surface-buffer-alist nil))
    (ewwm-manage--on-surface-create 1 "test")
    (ewwm-manage--on-surface-destroy 1)
    (should (null (ewwm--get-buffer 1)))))

;;; ewwm-manage-test.el ends here
