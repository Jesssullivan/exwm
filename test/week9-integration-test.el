;;; week9-integration-test.el --- Week 9 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-display)

(defvar ewwm-ipc--event-handlers)

;; ── Rust module structure ───────────────────────────────────

(ert-deftest week9/drm-lease-module-exists ()
  "drm_lease.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/vr/drm_lease.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week9/mod-rs-exports-drm-lease ()
  "vr/mod.rs declares drm_lease module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (mod-rs (expand-file-name "compositor/src/vr/mod.rs" root)))
    (with-temp-buffer
      (insert-file-contents mod-rs)
      (should (search-forward "pub mod drm_lease;" nil t)))))

;; ── IPC dispatch ────────────────────────────────────────────

(ert-deftest week9/dispatch-has-display-info ()
  "dispatch.rs handles vr-display-info."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-info\"" nil t)))))

(ert-deftest week9/dispatch-has-display-set-mode ()
  "dispatch.rs handles vr-display-set-mode."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-set-mode\"" nil t)))))

(ert-deftest week9/dispatch-has-display-select-hmd ()
  "dispatch.rs handles vr-display-select-hmd."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-select-hmd\"" nil t)))))

(ert-deftest week9/dispatch-has-display-refresh-rate ()
  "dispatch.rs handles vr-display-set-refresh-rate."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-set-refresh-rate\"" nil t)))))

(ert-deftest week9/dispatch-has-display-auto-detect ()
  "dispatch.rs handles vr-display-auto-detect."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-auto-detect\"" nil t)))))

(ert-deftest week9/dispatch-has-display-list-connectors ()
  "dispatch.rs handles vr-display-list-connectors."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-list-connectors\"" nil t)))))

;; ── Emacs integration ───────────────────────────────────────

(ert-deftest week9/ewwm-requires-vr-display ()
  "ewwm.el requires ewwm-vr-display."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(require 'ewwm-vr-display)" nil t)))))

(ert-deftest week9/ewwm-init-calls-display-init ()
  "ewwm.el init calls ewwm-vr-display-init."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(ewwm-vr-display-init)" nil t)))))

(ert-deftest week9/ewwm-exit-calls-display-teardown ()
  "ewwm.el exit calls ewwm-vr-display-teardown."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(ewwm-vr-display-teardown)" nil t)))))

;; ── Rust VrState integration ────────────────────────────────

(ert-deftest week9/openxr-state-has-hmd-manager ()
  "openxr_state.rs has hmd_manager field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ostate (expand-file-name "compositor/src/vr/openxr_state.rs" root)))
    (with-temp-buffer
      (insert-file-contents ostate)
      (should (search-forward "pub hmd_manager: HmdManager" nil t)))))

(ert-deftest week9/stub-has-hmd-manager ()
  "stub.rs has hmd_manager field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (stub (expand-file-name "compositor/src/vr/stub.rs" root)))
    (with-temp-buffer
      (insert-file-contents stub)
      (should (search-forward "pub hmd_manager: HmdManager" nil t)))))

;; ── Rust drm_lease structure ────────────────────────────────

(ert-deftest week9/drm-lease-has-connector-type ()
  "drm_lease.rs defines ConnectorType."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum ConnectorType" nil t)))))

(ert-deftest week9/drm-lease-has-hmd-manager ()
  "drm_lease.rs defines HmdManager."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct HmdManager" nil t)))))

(ert-deftest week9/drm-lease-has-lease-state ()
  "drm_lease.rs defines LeaseState."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct LeaseState" nil t)))))

(ert-deftest week9/drm-lease-has-display-mode ()
  "drm_lease.rs defines VrDisplayMode."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum VrDisplayMode" nil t)))))

(ert-deftest week9/drm-lease-has-rust-tests ()
  "drm_lease.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

;; ── Cross-module consistency ────────────────────────────────

(ert-deftest week9/display-modes-consistent ()
  "Elisp display modes match Rust VrDisplayMode."
  (let ((elisp-modes '(headset preview headless off)))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (m elisp-modes)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" (symbol-name m)) nil t)))))))

;;; week9-integration-test.el ends here
