;;; ewwm-manage-test.el --- Tests for ewwm-manage  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-manage)

(ert-deftest ewwm-manage-test/surface-create ()
  "Surface create produces a buffer and registers it."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "foot" :title "foot terminal"))))
      (unwind-protect
          (progn
            (should (buffer-live-p buf))
            (should (eq buf (ewwm--get-buffer 1)))
            (should (= (buffer-local-value 'ewwm-surface-id buf) 1))
            (should (equal (buffer-local-value 'ewwm-app-id buf) "foot")))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-manage-test/surface-destroy ()
  "Surface destroy removes buffer and cleans alist."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (ewwm-manage--on-create '(:id 1 :app-id "test" :title "test"))
    (ewwm-manage--on-destroy '(:id 1))
    (should (null (ewwm--get-buffer 1)))))

(ert-deftest ewwm-manage-test/manage-rule-match ()
  "Manage rules classify surfaces correctly."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules
         `((,(lambda (s) (equal (plist-get s :app-id) "keepassxc"))
            . (:floating t :workspace 3)))))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "keepassxc" :title "KeePassXC"))))
      (unwind-protect
          (progn
            (should (eq (buffer-local-value 'ewwm-surface-state buf) 'floating))
            (should (= (buffer-local-value 'ewwm-workspace buf) 3)))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-manage-test/title-changed ()
  "Title change updates buffer-local variable."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "foot" :title "foot"))))
      (unwind-protect
          (progn
            (ewwm-manage--on-title-changed '(:id 1 :title "new title"))
            (should (equal (buffer-local-value 'ewwm-title buf) "new title")))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-manage-test/fullscreen-toggle ()
  "Fullscreen toggle changes surface state."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "test" :title "test"))))
      (unwind-protect
          (progn
            (should (eq (buffer-local-value 'ewwm-surface-state buf) 'managed))
            (ewwm-fullscreen-toggle 1)
            (should (eq (buffer-local-value 'ewwm-surface-state buf) 'fullscreen))
            (ewwm-fullscreen-toggle 1)
            (should (eq (buffer-local-value 'ewwm-surface-state buf) 'managed)))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-manage-test/manage-finish-hook ()
  "ewwm-manage-finish-hook fires on surface creation."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil)
        (hook-fired nil))
    (let ((ewwm-manage-finish-hook (list (lambda () (setq hook-fired t)))))
      (ewwm-manage--on-create '(:id 1 :app-id "test" :title "test"))
      (unwind-protect
          (should hook-fired)
        (ewwm--destroy-surface-buffer 1)))))

;;; ewwm-manage-test.el ends here
