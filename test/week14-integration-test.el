;;; week14-integration-test.el --- Week 14 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-secrets)
(require 'ewwm-keepassxc-browser)
(require 'ewwm-secrets-ydotool)
(require 'ewwm-secrets-compositor)
(require 'ewwm-vr-secure-input)
(require 'ewwm-secrets-gaze-away)
(require 'ewwm-secrets-totp)
(require 'ewwm-secrets-passkey)
(require 'ewwm-secrets-autotype)

;; ── Rust module structure ───────────────────────────────────

(ert-deftest week14/autotype-module-exists ()
  "autotype.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/autotype.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week14/secure-input-module-exists ()
  "secure_input.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/secure_input.rs"
                             (locate-dominating-file default-directory ".git")))))

;; ── Rust modules declared in lib.rs ───────────────────────

(ert-deftest week14/main-declares-autotype ()
  "lib.rs declares autotype module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (lib-rs (expand-file-name "compositor/src/lib.rs" root)))
    (with-temp-buffer
      (insert-file-contents lib-rs)
      (should (search-forward "pub mod autotype;" nil t)))))

(ert-deftest week14/main-declares-secure-input ()
  "lib.rs declares secure_input module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (lib-rs (expand-file-name "compositor/src/lib.rs" root)))
    (with-temp-buffer
      (insert-file-contents lib-rs)
      (should (search-forward "pub mod secure_input;" nil t)))))

;; ── State has new fields ───────────────────────────────────

(ert-deftest week14/state-has-autotype ()
  "state.rs has autotype field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/state.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub autotype: AutoTypeManager" nil t)))))

(ert-deftest week14/state-has-secure-input ()
  "state.rs has secure_input field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/state.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub secure_input: SecureInputState" nil t)))))

;; ── IPC dispatch ────────────────────────────────────────────

(ert-deftest week14/dispatch-has-autotype ()
  "dispatch.rs handles autotype."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"autotype\"" nil t)))))

(ert-deftest week14/dispatch-has-autotype-status ()
  "dispatch.rs handles autotype-status."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"autotype-status\"" nil t)))))

(ert-deftest week14/dispatch-has-autotype-abort ()
  "dispatch.rs handles autotype-abort."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"autotype-abort\"" nil t)))))

(ert-deftest week14/dispatch-has-autotype-pause ()
  "dispatch.rs handles autotype-pause."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"autotype-pause\"" nil t)))))

(ert-deftest week14/dispatch-has-autotype-resume ()
  "dispatch.rs handles autotype-resume."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"autotype-resume\"" nil t)))))

(ert-deftest week14/dispatch-has-secure-input-mode ()
  "dispatch.rs handles secure-input-mode."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"secure-input-mode\"" nil t)))))

(ert-deftest week14/dispatch-has-secure-input-status ()
  "dispatch.rs handles secure-input-status."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"secure-input-status\"" nil t)))))

(ert-deftest week14/dispatch-has-gaze-away-monitor ()
  "dispatch.rs handles gaze-away-monitor."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-away-monitor\"" nil t)))))

;; ── Rust type definitions ──────────────────────────────────

(ert-deftest week14/autotype-has-manager-struct ()
  "autotype.rs defines AutoTypeManager."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/autotype.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct AutoTypeManager" nil t)))))

(ert-deftest week14/autotype-has-phase-enum ()
  "autotype.rs defines AutoTypePhase."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/autotype.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum AutoTypePhase" nil t)))))

(ert-deftest week14/autotype-has-event-enum ()
  "autotype.rs defines AutoTypeEvent."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/autotype.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum AutoTypeEvent" nil t)))))

(ert-deftest week14/secure-input-has-state-struct ()
  "secure_input.rs defines SecureInputState."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/secure_input.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct SecureInputState" nil t)))))

(ert-deftest week14/secure-input-has-event-enum ()
  "secure_input.rs defines SecureInputEvent."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/secure_input.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum SecureInputEvent" nil t)))))

;; ── Rust unit tests ────────────────────────────────────────

(ert-deftest week14/autotype-has-rust-tests ()
  "autotype.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/autotype.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

(ert-deftest week14/secure-input-has-rust-tests ()
  "secure_input.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/secure_input.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

;; ── Elisp modules exist ────────────────────────────────────

(ert-deftest week14/ewwm-secrets-provides ()
  "ewwm-secrets provides its feature."
  (should (featurep 'ewwm-secrets)))

(ert-deftest week14/ewwm-keepassxc-browser-provides ()
  "ewwm-keepassxc-browser provides its feature."
  (should (featurep 'ewwm-keepassxc-browser)))

(ert-deftest week14/ewwm-secrets-ydotool-provides ()
  "ewwm-secrets-ydotool provides its feature."
  (should (featurep 'ewwm-secrets-ydotool)))

(ert-deftest week14/ewwm-secrets-compositor-provides ()
  "ewwm-secrets-compositor provides its feature."
  (should (featurep 'ewwm-secrets-compositor)))

(ert-deftest week14/ewwm-vr-secure-input-provides ()
  "ewwm-vr-secure-input provides its feature."
  (should (featurep 'ewwm-vr-secure-input)))

(ert-deftest week14/ewwm-secrets-gaze-away-provides ()
  "ewwm-secrets-gaze-away provides its feature."
  (should (featurep 'ewwm-secrets-gaze-away)))

(ert-deftest week14/ewwm-secrets-totp-provides ()
  "ewwm-secrets-totp provides its feature."
  (should (featurep 'ewwm-secrets-totp)))

(ert-deftest week14/ewwm-secrets-passkey-provides ()
  "ewwm-secrets-passkey provides its feature."
  (should (featurep 'ewwm-secrets-passkey)))

(ert-deftest week14/ewwm-secrets-autotype-provides ()
  "ewwm-secrets-autotype provides its feature."
  (should (featurep 'ewwm-secrets-autotype)))

;; ── Cross-module: all IPC commands in dispatch ─────────────

(ert-deftest week14/ipc-commands-match-dispatch ()
  "All Week 14 IPC command strings in dispatch."
  (let ((commands '("autotype" "autotype-status" "autotype-abort"
                    "autotype-pause" "autotype-resume"
                    "secure-input-mode" "secure-input-status"
                    "gaze-away-monitor")))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
      (with-temp-buffer
        (insert-file-contents dispatch)
        (dolist (cmd commands)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" cmd) nil t)))))))

;;; week14-integration-test.el ends here
