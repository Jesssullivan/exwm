;;; exwm-input-test.el --- Tests for exwm-input  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(defvar exwm-input-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Project root.")

(defun exwm-input-test--file ()
  (expand-file-name "lisp/core/exwm-input.el" exwm-input-test--root))

(ert-deftest exwm-input-test/file-exists ()
  (should (file-exists-p (exwm-input-test--file))))

(ert-deftest exwm-input-test/defines-global-keys ()
  (with-temp-buffer
    (insert-file-contents (exwm-input-test--file))
    (should (string-match-p "defvar exwm-input--global-keys" (buffer-string)))))

(ert-deftest exwm-input-test/defines-set-key ()
  (with-temp-buffer
    (insert-file-contents (exwm-input-test--file))
    (should (string-match-p "defun exwm-input-set-key" (buffer-string)))))

(ert-deftest exwm-input-test/defines-prefix-keys ()
  (with-temp-buffer
    (insert-file-contents (exwm-input-test--file))
    (should (string-match-p "defcustom exwm-input-prefix-keys" (buffer-string)))))

;;; exwm-input-test.el ends here
