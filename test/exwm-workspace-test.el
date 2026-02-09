;;; exwm-workspace-test.el --- Tests for exwm-workspace  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(defvar exwm-workspace-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Project root.")

(defun exwm-workspace-test--file ()
  (expand-file-name "lisp/core/exwm-workspace.el" exwm-workspace-test--root))

(ert-deftest exwm-workspace-test/file-exists ()
  (should (file-exists-p (exwm-workspace-test--file))))

(ert-deftest exwm-workspace-test/defines-workspace-number ()
  (with-temp-buffer
    (insert-file-contents (exwm-workspace-test--file))
    (should (string-match-p "defcustom exwm-workspace-number" (buffer-string)))))

(ert-deftest exwm-workspace-test/defines-workspace-list ()
  (with-temp-buffer
    (insert-file-contents (exwm-workspace-test--file))
    (should (string-match-p "defvar exwm-workspace--list" (buffer-string)))))

(ert-deftest exwm-workspace-test/defines-switch ()
  (with-temp-buffer
    (insert-file-contents (exwm-workspace-test--file))
    (should (string-match-p "defun exwm-workspace-switch " (buffer-string)))))

;;; exwm-workspace-test.el ends here
