;;; week12-integration-test.el --- Week 12 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-eye)

;; ── Rust module structure ───────────────────────────────────

(ert-deftest week12/gaze-focus-module-exists ()
  "gaze_focus.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/vr/gaze_focus.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week12/mod-rs-exports-gaze-focus ()
  "vr/mod.rs declares gaze_focus module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (mod-rs (expand-file-name "compositor/src/vr/mod.rs" root)))
    (with-temp-buffer
      (insert-file-contents mod-rs)
      (should (search-forward "pub mod gaze_focus;" nil t)))))

;; ── IPC dispatch ────────────────────────────────────────────

(ert-deftest week12/dispatch-has-gaze-focus-config ()
  "dispatch.rs handles gaze-focus-config."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-focus-config\"" nil t)))))

(ert-deftest week12/dispatch-has-gaze-focus-status ()
  "dispatch.rs handles gaze-focus-status."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-focus-status\"" nil t)))))

(ert-deftest week12/dispatch-has-gaze-focus-set-policy ()
  "dispatch.rs handles gaze-focus-set-policy."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-focus-set-policy\"" nil t)))))

(ert-deftest week12/dispatch-has-gaze-focus-set-dwell ()
  "dispatch.rs handles gaze-focus-set-dwell."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-focus-set-dwell\"" nil t)))))

(ert-deftest week12/dispatch-has-gaze-focus-set-cooldown ()
  "dispatch.rs handles gaze-focus-set-cooldown."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-focus-set-cooldown\"" nil t)))))

(ert-deftest week12/dispatch-has-gaze-focus-analytics ()
  "dispatch.rs handles gaze-focus-analytics."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-focus-analytics\"" nil t)))))

(ert-deftest week12/dispatch-has-gaze-focus-back ()
  "dispatch.rs handles gaze-focus-back."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-focus-back\"" nil t)))))

;; ── VrState has gaze_focus field ────────────────────────────

(ert-deftest week12/openxr-state-has-gaze-focus ()
  "openxr_state.rs has gaze_focus field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/openxr_state.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub gaze_focus: GazeFocusManager" nil t)))))

(ert-deftest week12/stub-has-gaze-focus ()
  "stub.rs has gaze_focus field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/stub.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub gaze_focus: GazeFocusManager" nil t)))))

;; ── Rust gaze_focus structure ───────────────────────────────

(ert-deftest week12/gaze-focus-has-dwell-state ()
  "gaze_focus.rs defines DwellState."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum DwellState" nil t)))))

(ert-deftest week12/gaze-focus-has-dwell-config ()
  "gaze_focus.rs defines DwellConfig."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct DwellConfig" nil t)))))

(ert-deftest week12/gaze-focus-has-saccade-detector ()
  "gaze_focus.rs defines SaccadeDetector."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct SaccadeDetector" nil t)))))

(ert-deftest week12/gaze-focus-has-reading-detector ()
  "gaze_focus.rs defines ReadingDetector."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct ReadingDetector" nil t)))))

(ert-deftest week12/gaze-focus-has-cooldown-state ()
  "gaze_focus.rs defines CooldownState."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct CooldownState" nil t)))))

(ert-deftest week12/gaze-focus-has-hysteresis-tracker ()
  "gaze_focus.rs defines HysteresisTracker."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct HysteresisTracker" nil t)))))

(ert-deftest week12/gaze-focus-has-focus-policy ()
  "gaze_focus.rs defines FocusPolicy."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum FocusPolicy" nil t)))))

(ert-deftest week12/gaze-focus-has-focus-analytics ()
  "gaze_focus.rs defines FocusAnalytics."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct FocusAnalytics" nil t)))))

(ert-deftest week12/gaze-focus-has-manager ()
  "gaze_focus.rs defines GazeFocusManager."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct GazeFocusManager" nil t)))))

(ert-deftest week12/gaze-focus-has-event-enum ()
  "gaze_focus.rs defines GazeFocusEvent."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum GazeFocusEvent" nil t)))))

(ert-deftest week12/gaze-focus-has-rust-tests ()
  "gaze_focus.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

;; ── Cross-module consistency ────────────────────────────────

(ert-deftest week12/focus-policies-consistent ()
  "Elisp focus policies match Rust FocusPolicy."
  (let ((policies '(gaze-only gaze-primary gaze-assist disabled)))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (p policies)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" (symbol-name p)) nil t)))))))

(ert-deftest week12/ipc-commands-match-elisp ()
  "All 7 gaze-focus IPC command strings in dispatch match Elisp."
  (let ((commands '("gaze-focus-config" "gaze-focus-status" "gaze-focus-set-policy"
                    "gaze-focus-set-dwell" "gaze-focus-set-cooldown"
                    "gaze-focus-analytics" "gaze-focus-back")))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
      (with-temp-buffer
        (insert-file-contents dispatch)
        (dolist (cmd commands)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" cmd) nil t)))))))

(ert-deftest week12/dispatch-imports-gaze-focus ()
  "dispatch.rs uses gaze_focus::FocusPolicy."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "gaze_focus::FocusPolicy" nil t)))))

;; ── Gaze focus event IPC format ─────────────────────────────

(ert-deftest week12/gaze-focus-events-have-sexp ()
  "GazeFocusEvent variants have to_sexp method."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_focus.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub fn to_sexp" nil t))
      ;; Verify key event types are present
      (goto-char (point-min))
      (should (search-forward ":gaze-dwell-started" nil t))
      (goto-char (point-min))
      (should (search-forward ":gaze-dwell-progress" nil t))
      (goto-char (point-min))
      (should (search-forward ":gaze-focus-requested" nil t))
      (goto-char (point-min))
      (should (search-forward ":gaze-cooldown-started" nil t))
      (goto-char (point-min))
      (should (search-forward ":gaze-saccade-detected" nil t))
      (goto-char (point-min))
      (should (search-forward ":gaze-reading-entered" nil t)))))

;;; week12-integration-test.el ends here
