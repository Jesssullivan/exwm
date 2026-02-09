;;; week5-integration-test.el --- Week 5 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-workspace)
(require 'ewwm-layout)
(require 'ewwm-input)
(require 'ewwm-manage)
(require 'ewwm-floating)
(require 'ewwm-launch)

(defvar week5-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Project root.")

;; ── Module structure ─────────────────────────────────────────

(ert-deftest week5-integration/ewwm-modules-exist ()
  "All Week 5 Elisp modules exist."
  (dolist (file '("lisp/vr/ewwm-core.el"
                  "lisp/vr/ewwm-workspace.el"
                  "lisp/vr/ewwm-layout.el"
                  "lisp/vr/ewwm-input.el"
                  "lisp/vr/ewwm-manage.el"
                  "lisp/vr/ewwm-floating.el"
                  "lisp/vr/ewwm-launch.el"
                  "lisp/vr/ewwm.el"))
    (should (file-exists-p (expand-file-name file week5-test--root)))))

;; ── Core APIs ────────────────────────────────────────────────

(ert-deftest week5-integration/core-apis ()
  "Core module provides all required APIs."
  ;; Buffer-local variables
  (dolist (var '(ewwm-surface-id ewwm-app-id ewwm-title
                 ewwm-class-name ewwm-instance-name
                 ewwm-surface-state ewwm-geometry ewwm-workspace
                 ewwm-x11-p ewwm-x11-class ewwm-x11-instance))
    (should (boundp var)))
  ;; Functions
  (dolist (fn '(ewwm--get-buffer ewwm--set-buffer ewwm--remove-buffer
                ewwm--all-surfaces ewwm--all-buffers ewwm--surface-count
                ewwm--clear-surfaces ewwm--create-surface-buffer
                ewwm--destroy-surface-buffer ewwm--update-surface-title
                ewwm--update-surface-geometry ewwm--refresh-buffer-content
                ewwm--buffers-on-workspace ewwm--surface-buffer-p))
    (should (fboundp fn))))

(ert-deftest week5-integration/ewwm-mode-defined ()
  "ewwm-mode is a derived major mode."
  (should (fboundp 'ewwm-mode))
  (should (keymapp ewwm-mode-map)))

;; ── Workspace APIs ───────────────────────────────────────────

(ert-deftest week5-integration/workspace-apis ()
  "Workspace module provides all required APIs."
  (dolist (fn '(ewwm-workspace-switch ewwm-workspace-switch-next
                ewwm-workspace-switch-prev ewwm-workspace-move-surface
                ewwm-workspace-list ewwm-workspace-name
                ewwm-workspace-set-name ewwm-workspace--init))
    (should (fboundp fn)))
  (should (boundp 'ewwm-workspace-number))
  (should (boundp 'ewwm-workspace-current-index))
  (should (boundp 'ewwm-workspace-switch-hook)))

;; ── Layout APIs ──────────────────────────────────────────────

(ert-deftest week5-integration/layout-apis ()
  "Layout module provides all required APIs."
  (dolist (fn '(ewwm-layout-set ewwm-layout-cycle ewwm-layout-current
                ewwm-layout--apply-current ewwm-layout--sync-geometry
                ewwm-layout--tiling ewwm-layout--monocle ewwm-layout--grid
                ewwm-layout--set-usable-area))
    (should (fboundp fn)))
  (should (boundp 'ewwm-layout-default))
  (should (boundp 'ewwm-layout-master-ratio))
  (should (boundp 'ewwm-layout-change-hook)))

;; ── Input APIs ───────────────────────────────────────────────

(ert-deftest week5-integration/input-apis ()
  "Input module provides all required APIs."
  (dolist (fn '(ewwm-input-set-key ewwm-input-unset-key
                ewwm-input--handle-key-event ewwm-input--register-all-grabs
                ewwm-input--setup-defaults ewwm-input-toggle-intercept))
    (should (fboundp fn)))
  (should (boundp 'ewwm-input-intercept-mode)))

;; ── Manage APIs ──────────────────────────────────────────────

(ert-deftest week5-integration/manage-apis ()
  "Manage module provides all required APIs."
  (dolist (fn '(ewwm-manage--on-create ewwm-manage--on-destroy
                ewwm-manage--on-title-changed ewwm-manage--on-geometry-changed
                ewwm-manage--match-rules ewwm-fullscreen-toggle))
    (should (fboundp fn)))
  (should (boundp 'ewwm-manage-rules))
  (should (boundp 'ewwm-manage-finish-hook))
  (should (boundp 'ewwm-manage-close-hook)))

;; ── Floating APIs ────────────────────────────────────────────

(ert-deftest week5-integration/floating-apis ()
  "Floating module provides all required APIs."
  (dolist (fn '(ewwm-floating-toggle ewwm-floating-list ewwm-floating-p
                ewwm-floating-move ewwm-floating-resize))
    (should (fboundp fn))))

;; ── Launch APIs ──────────────────────────────────────────────

(ert-deftest week5-integration/launch-apis ()
  "Launch module provides all required APIs."
  (dolist (fn '(ewwm-launch ewwm-launch-interactively ewwm-launch-favorite
                ewwm-launch-list ewwm-launch-kill-all))
    (should (fboundp fn)))
  (should (boundp 'ewwm-launch-favorites))
  (should (boundp 'ewwm-launch-env-vars)))

;; ── Orchestration ────────────────────────────────────────────

(ert-deftest week5-integration/ewwm-orchestration-apis ()
  "Main ewwm module provides init/exit/reset."
  (require 'ewwm)
  (dolist (fn '(ewwm-init ewwm-exit ewwm-restart ewwm-reset
                ewwm-global-mode))
    (should (fboundp fn)))
  (should (boundp 'ewwm-compositor-command))
  (should (boundp 'ewwm-compositor-args))
  (should (boundp 'ewwm-focus-policy))
  (should (boundp 'ewwm-compositor-startup-timeout)))

;; ── End-to-end surface management ────────────────────────────

(ert-deftest week5-integration/e2e-surface-management ()
  "End-to-end: create surface, manage it, switch workspace, destroy."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-workspace-number 4)
        (ewwm-workspace--configs nil)
        (ewwm-workspace--names nil)
        (ewwm-manage-rules nil)
        (ewwm-workspace-switch-hook nil))
    (ewwm-workspace--init)
    ;; Create surface on workspace 0
    (let ((buf (ewwm-manage--on-create '(:id 1 :app-id "foot" :title "term"))))
      (should (= (buffer-local-value 'ewwm-workspace buf) 0))
      ;; Move to workspace 1
      (with-current-buffer buf (setq ewwm-workspace 1))
      (should (= (length (ewwm--buffers-on-workspace 0)) 0))
      (should (= (length (ewwm--buffers-on-workspace 1)) 1))
      ;; Destroy
      (ewwm-manage--on-destroy '(:id 1))
      (should (= (ewwm--surface-count) 0)))))

;;; week5-integration-test.el ends here
