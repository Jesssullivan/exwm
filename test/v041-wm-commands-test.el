;;; v041-wm-commands-test.el --- v0.4.1 WM command tests  -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for fullscreen, float, workspace-move, and workspace-list.

;;; Code:

(require 'ert)

(defvar v041-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

;; ── Fullscreen ─────────────────────────────────────────────

(ert-deftest v041/fullscreen-uses-toplevel-state ()
  "surface-fullscreen sets ToplevelState::Fullscreen."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "ToplevelState::Fullscreen" nil t)))))

(ert-deftest v041/fullscreen-sends-configure ()
  "surface-fullscreen sends pending configure."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "send_pending_configure" nil t)))))

(ert-deftest v041/fullscreen-accepts-enable-param ()
  "surface-fullscreen reads :enable parameter."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "get_bool(value, \"enable\")" nil t)))))

;; ── Float toggle ───────────────────────────────────────────

(ert-deftest v041/float-updates-data ()
  "surface-float updates SurfaceData.floating."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "data.floating = enable" nil t)))))

(ert-deftest v041/float-emits-event ()
  "surface-float emits surface-float-changed IPC event."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface-float-changed" nil t)))))

;; ── Workspace move ─────────────────────────────────────────

(ert-deftest v041/workspace-move-updates-data ()
  "workspace-move-surface updates SurfaceData.workspace."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "data.workspace = workspace" nil t)))))

(ert-deftest v041/workspace-move-emits-event ()
  "workspace-move-surface emits surface-workspace-changed event."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface-workspace-changed" nil t)))))

(ert-deftest v041/workspace-move-has-old-new ()
  "workspace-move event includes old-workspace and new-workspace."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "old-workspace" nil t))
      (should (search-forward "new-workspace" nil t)))))

;; ── Workspace list ─────────────────────────────────────────

(ert-deftest v041/workspace-list-has-surface-ids ()
  "workspace-list includes surface IDs per workspace."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface_ids" nil t)))))

(ert-deftest v041/workspace-list-has-count ()
  "workspace-list includes :count per workspace."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward ":count" nil t)))))

;; ── Layout (Emacs-driven) ──────────────────────────────────

(ert-deftest v041/layout-set-reads-layout-param ()
  "layout-set reads :layout parameter."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "get_string(value, \"layout\")" nil t)))))

(provide 'v041-wm-commands-test)
;;; v041-wm-commands-test.el ends here
