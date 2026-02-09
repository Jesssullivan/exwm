;;; ewwm-core-test.el --- Tests for ewwm-core  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)

(ert-deftest ewwm-core-test/initial-alist-nil ()
  "Surface-buffer alist is initially nil."
  (let ((ewwm--surface-buffer-alist nil))
    (should (null ewwm--surface-buffer-alist))))

(ert-deftest ewwm-core-test/set-and-get ()
  "Set and get a surface-buffer association."
  (let ((ewwm--surface-buffer-alist nil)
        (buf (generate-new-buffer " *test-surface*")))
    (unwind-protect
        (progn
          (ewwm--set-buffer 42 buf)
          (should (eq buf (ewwm--get-buffer 42))))
      (kill-buffer buf))))

(ert-deftest ewwm-core-test/remove ()
  "Remove a surface-buffer association."
  (let ((ewwm--surface-buffer-alist nil)
        (buf (generate-new-buffer " *test-surface*")))
    (unwind-protect
        (progn
          (ewwm--set-buffer 42 buf)
          (ewwm--remove-buffer 42)
          (should (null (ewwm--get-buffer 42))))
      (kill-buffer buf))))

(ert-deftest ewwm-core-test/all-surfaces ()
  "List all managed surface IDs."
  (let ((ewwm--surface-buffer-alist nil)
        (b1 (generate-new-buffer " *test1*"))
        (b2 (generate-new-buffer " *test2*")))
    (unwind-protect
        (progn
          (ewwm--set-buffer 1 b1)
          (ewwm--set-buffer 2 b2)
          (should (equal (sort (ewwm--all-surfaces) #'<) '(1 2))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest ewwm-core-test/all-buffers ()
  "List all managed buffers."
  (let ((ewwm--surface-buffer-alist nil)
        (b1 (generate-new-buffer " *test1*"))
        (b2 (generate-new-buffer " *test2*")))
    (unwind-protect
        (progn
          (ewwm--set-buffer 1 b1)
          (ewwm--set-buffer 2 b2)
          (should (= 2 (length (ewwm--all-buffers)))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest ewwm-core-test/surface-count ()
  "Count managed surfaces."
  (let ((ewwm--surface-buffer-alist nil))
    (should (= 0 (ewwm--surface-count)))
    (ewwm--set-buffer 1 (current-buffer))
    (should (= 1 (ewwm--surface-count)))))

(ert-deftest ewwm-core-test/clear-surfaces ()
  "Clear all surface associations."
  (let ((ewwm--surface-buffer-alist nil))
    (ewwm--set-buffer 1 (current-buffer))
    (ewwm--set-buffer 2 (current-buffer))
    (ewwm--clear-surfaces)
    (should (= 0 (ewwm--surface-count)))))

(ert-deftest ewwm-core-test/get-nonexistent ()
  "Get returns nil for unknown surface."
  (let ((ewwm--surface-buffer-alist nil))
    (should (null (ewwm--get-buffer 999)))))

;;; ewwm-core-test.el ends here
