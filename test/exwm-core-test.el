;;; exwm-core-test.el --- Tests for exwm-core  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(defvar exwm-core-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Project root.")

(defun exwm-core-test--file ()
  "Return path to exwm-core.el."
  (expand-file-name "lisp/core/exwm-core.el" exwm-core-test--root))

(ert-deftest exwm-core-test/file-exists ()
  (should (file-exists-p (exwm-core-test--file))))

(ert-deftest exwm-core-test/file-has-lexical-binding ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file) nil 0 200)
    (should (string-match-p "lexical-binding: t" (buffer-string)))))

(ert-deftest exwm-core-test/provides-feature ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file))
    (should (string-match-p "(provide 'exwm-core)" (buffer-string)))))

(ert-deftest exwm-core-test/defines-id-buffer-alist ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file))
    (should (string-match-p "defvar exwm--id-buffer-alist" (buffer-string)))))

(ert-deftest exwm-core-test/defines-exwm-mode ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file))
    (should (string-match-p "define-derived-mode exwm-mode" (buffer-string)))))

(ert-deftest exwm-core-test/defines-connection-var ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file))
    (should (string-match-p "defvar exwm--connection" (buffer-string)))))

(ert-deftest exwm-core-test/defines-root-var ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file))
    (should (string-match-p "defvar exwm--root" (buffer-string)))))

(ert-deftest exwm-core-test/defines-class-name ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file))
    (should (string-match-p "defvar-local exwm-class-name" (buffer-string)))))

(ert-deftest exwm-core-test/defines-title ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file))
    (should (string-match-p "defvar-local exwm-title" (buffer-string)))))

(ert-deftest exwm-core-test/defines-mode-map ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file))
    (should (string-match-p "defvar-keymap exwm-mode-map" (buffer-string)))))

(ert-deftest exwm-core-test/requires-xcb ()
  (with-temp-buffer
    (insert-file-contents (exwm-core-test--file))
    (should (string-match-p "(require 'xcb)" (buffer-string)))))

;;; exwm-core-test.el ends here
