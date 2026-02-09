;;; week13-integration-test.el --- Week 13 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-wink)
(require 'ewwm-vr-gaze-zone)
(require 'ewwm-vr-fatigue)

;; ── Rust module structure ───────────────────────────────────

(ert-deftest week13/blink-wink-module-exists ()
  "blink_wink.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/vr/blink_wink.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week13/gaze-zone-module-exists ()
  "gaze_zone.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/vr/gaze_zone.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week13/fatigue-module-exists ()
  "fatigue.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/vr/fatigue.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week13/mod-rs-exports-blink-wink ()
  "vr/mod.rs declares blink_wink module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (mod-rs (expand-file-name "compositor/src/vr/mod.rs" root)))
    (with-temp-buffer
      (insert-file-contents mod-rs)
      (should (search-forward "pub mod blink_wink;" nil t)))))

(ert-deftest week13/mod-rs-exports-gaze-zone ()
  "vr/mod.rs declares gaze_zone module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (mod-rs (expand-file-name "compositor/src/vr/mod.rs" root)))
    (with-temp-buffer
      (insert-file-contents mod-rs)
      (should (search-forward "pub mod gaze_zone;" nil t)))))

(ert-deftest week13/mod-rs-exports-fatigue ()
  "vr/mod.rs declares fatigue module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (mod-rs (expand-file-name "compositor/src/vr/mod.rs" root)))
    (with-temp-buffer
      (insert-file-contents mod-rs)
      (should (search-forward "pub mod fatigue;" nil t)))))

;; ── IPC dispatch ────────────────────────────────────────────

(ert-deftest week13/dispatch-has-wink-status ()
  "dispatch.rs handles wink-status."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"wink-status\"" nil t)))))

(ert-deftest week13/dispatch-has-wink-config ()
  "dispatch.rs handles wink-config."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"wink-config\"" nil t)))))

(ert-deftest week13/dispatch-has-wink-calibrate-start ()
  "dispatch.rs handles wink-calibrate-start."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"wink-calibrate-start\"" nil t)))))

(ert-deftest week13/dispatch-has-wink-set-confidence ()
  "dispatch.rs handles wink-set-confidence."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"wink-set-confidence\"" nil t)))))

(ert-deftest week13/dispatch-has-gaze-zone-status ()
  "dispatch.rs handles gaze-zone-status."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-zone-status\"" nil t)))))

(ert-deftest week13/dispatch-has-gaze-zone-config ()
  "dispatch.rs handles gaze-zone-config."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-zone-config\"" nil t)))))

(ert-deftest week13/dispatch-has-gaze-zone-set-dwell ()
  "dispatch.rs handles gaze-zone-set-dwell."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-zone-set-dwell\"" nil t)))))

(ert-deftest week13/dispatch-has-fatigue-status ()
  "dispatch.rs handles fatigue-status."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"fatigue-status\"" nil t)))))

(ert-deftest week13/dispatch-has-fatigue-config ()
  "dispatch.rs handles fatigue-config."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"fatigue-config\"" nil t)))))

(ert-deftest week13/dispatch-has-fatigue-metrics ()
  "dispatch.rs handles fatigue-metrics."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"fatigue-metrics\"" nil t)))))

(ert-deftest week13/dispatch-has-fatigue-reset ()
  "dispatch.rs handles fatigue-reset."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"fatigue-reset\"" nil t)))))

;; ── VrState has new fields ──────────────────────────────────

(ert-deftest week13/openxr-state-has-blink-wink ()
  "openxr_state.rs has blink_wink field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/openxr_state.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub blink_wink: BlinkWinkManager" nil t)))))

(ert-deftest week13/openxr-state-has-zone-detector ()
  "openxr_state.rs has zone_detector field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/openxr_state.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub zone_detector: ZoneDetector" nil t)))))

(ert-deftest week13/openxr-state-has-fatigue-monitor ()
  "openxr_state.rs has fatigue_monitor field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/openxr_state.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub fatigue_monitor: FatigueMonitor" nil t)))))

(ert-deftest week13/stub-has-blink-wink ()
  "stub.rs has blink_wink field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/stub.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub blink_wink: BlinkWinkManager" nil t)))))

(ert-deftest week13/stub-has-zone-detector ()
  "stub.rs has zone_detector field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/stub.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub zone_detector: ZoneDetector" nil t)))))

(ert-deftest week13/stub-has-fatigue-monitor ()
  "stub.rs has fatigue_monitor field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/stub.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub fatigue_monitor: FatigueMonitor" nil t)))))

;; ── Rust type definitions ───────────────────────────────────

(ert-deftest week13/blink-wink-has-blink-detector ()
  "blink_wink.rs defines BlinkDetector."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/blink_wink.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct BlinkDetector" nil t)))))

(ert-deftest week13/blink-wink-has-wink-classifier ()
  "blink_wink.rs defines WinkClassifier."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/blink_wink.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct WinkClassifier" nil t)))))

(ert-deftest week13/blink-wink-has-manager ()
  "blink_wink.rs defines BlinkWinkManager."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/blink_wink.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct BlinkWinkManager" nil t)))))

(ert-deftest week13/blink-wink-has-wink-event ()
  "blink_wink.rs defines WinkEvent."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/blink_wink.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum WinkEvent" nil t)))))

(ert-deftest week13/gaze-zone-has-zone-enum ()
  "gaze_zone.rs defines GazeZone."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_zone.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum GazeZone" nil t)))))

(ert-deftest week13/gaze-zone-has-detector ()
  "gaze_zone.rs defines ZoneDetector."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_zone.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct ZoneDetector" nil t)))))

(ert-deftest week13/gaze-zone-has-zone-event ()
  "gaze_zone.rs defines ZoneEvent."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_zone.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum ZoneEvent" nil t)))))

(ert-deftest week13/fatigue-has-monitor ()
  "fatigue.rs defines FatigueMonitor."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/fatigue.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct FatigueMonitor" nil t)))))

(ert-deftest week13/fatigue-has-level-enum ()
  "fatigue.rs defines FatigueLevel."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/fatigue.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum FatigueLevel" nil t)))))

(ert-deftest week13/fatigue-has-event-enum ()
  "fatigue.rs defines FatigueEvent."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/fatigue.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum FatigueEvent" nil t)))))

;; ── Rust unit tests ─────────────────────────────────────────

(ert-deftest week13/blink-wink-has-rust-tests ()
  "blink_wink.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/blink_wink.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

(ert-deftest week13/gaze-zone-has-rust-tests ()
  "gaze_zone.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/gaze_zone.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

(ert-deftest week13/fatigue-has-rust-tests ()
  "fatigue.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/fatigue.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

;; ── Cross-module consistency ────────────────────────────────

(ert-deftest week13/ipc-commands-match-dispatch ()
  "All Week 13 IPC command strings in dispatch."
  (let ((commands '("wink-status" "wink-config" "wink-calibrate-start"
                    "wink-set-confidence" "gaze-zone-status" "gaze-zone-config"
                    "gaze-zone-set-dwell" "fatigue-status" "fatigue-config"
                    "fatigue-metrics" "fatigue-reset")))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
      (with-temp-buffer
        (insert-file-contents dispatch)
        (dolist (cmd commands)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" cmd) nil t)))))))

(ert-deftest week13/dispatch-imports-blink-wink ()
  "dispatch.rs uses blink_wink::BlinkWinkManager."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "blink_wink" nil t)))))

;;; week13-integration-test.el ends here
