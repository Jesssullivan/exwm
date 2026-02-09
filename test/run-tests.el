;;; run-tests.el --- Test runner for EXWM-VR  -*- lexical-binding: t -*-

;;; Commentary:
;; Run with: emacs --batch -L lisp/core -L lisp/vr -L lisp/ext -l test/run-tests.el

;;; Code:

(require 'ert)

;; Set up load path
(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory
                       (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path project-root)
  (add-to-list 'load-path (expand-file-name "lisp/core" project-root))
  (add-to-list 'load-path (expand-file-name "lisp/vr" project-root))
  (add-to-list 'load-path (expand-file-name "lisp/ext" project-root))
  (add-to-list 'load-path (expand-file-name "test" project-root)))

;; Load test files
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (file (directory-files test-dir t "-test\\.el$"))
    (load file nil t)))

;; Run
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
