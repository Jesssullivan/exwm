;;; week1-integration-test.el --- Week 1 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(defvar week1-test--project-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Project root directory.")

;; ── scaffold tests ──────────────────────────────────────

(ert-deftest week1-integration/flake-exists ()
  (should (file-exists-p (expand-file-name "flake.nix" week1-test--project-root))))

(ert-deftest week1-integration/justfile-exists ()
  (should (file-exists-p (expand-file-name "justfile" week1-test--project-root))))

(ert-deftest week1-integration/cliff-toml-exists ()
  (should (file-exists-p (expand-file-name "cliff.toml" week1-test--project-root))))

(ert-deftest week1-integration/editorconfig-exists ()
  (should (file-exists-p (expand-file-name ".editorconfig" week1-test--project-root))))

(ert-deftest week1-integration/ci-pipeline-exists ()
  (should (file-exists-p (expand-file-name ".github/workflows/ci.yml" week1-test--project-root))))

(ert-deftest week1-integration/pre-commit-hook-exists ()
  (let ((hook (expand-file-name ".githooks/pre-commit" week1-test--project-root)))
    (should (file-exists-p hook))
    (should (file-executable-p hook))))

(ert-deftest week1-integration/commit-msg-hook-exists ()
  (let ((hook (expand-file-name ".githooks/commit-msg" week1-test--project-root)))
    (should (file-exists-p hook))
    (should (file-executable-p hook))))

;; ── module completeness (now in lisp/core/) ─────────────

(ert-deftest week1-integration/all-exwm-modules-exist ()
  (let ((modules '("exwm-core.el" "exwm-workspace.el" "exwm-input.el"
                    "exwm.el" "exwm-manage.el" "exwm-floating.el"
                    "exwm-layout.el" "exwm-xim.el" "exwm-systemtray.el"
                    "exwm-randr.el" "exwm-xsettings.el" "exwm-background.el")))
    (dolist (mod modules)
      (should (file-exists-p
               (expand-file-name (concat "lisp/core/" mod) week1-test--project-root))))))

(ert-deftest week1-integration/all-modules-lexical-binding ()
  (let ((modules '("exwm-core.el" "exwm-workspace.el" "exwm-input.el"
                    "exwm.el" "exwm-manage.el" "exwm-floating.el"
                    "exwm-layout.el" "exwm-xim.el" "exwm-systemtray.el"
                    "exwm-randr.el" "exwm-xsettings.el" "exwm-background.el")))
    (dolist (mod modules)
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name (concat "lisp/core/" mod) week1-test--project-root)
         nil 0 200)
        (should (string-match-p "lexical-binding: t" (buffer-string)))))))

(ert-deftest week1-integration/all-modules-provide-feature ()
  (let ((modules '("exwm-core" "exwm-workspace" "exwm-input"
                    "exwm" "exwm-manage" "exwm-floating"
                    "exwm-layout" "exwm-xim" "exwm-systemtray"
                    "exwm-randr" "exwm-xsettings" "exwm-background")))
    (dolist (mod modules)
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name (concat "lisp/core/" mod ".el") week1-test--project-root))
        (should (string-match-p
                 (format "(provide '%s)" mod) (buffer-string)))))))

;; ── meta-tests ──────────────────────────────────────────

(ert-deftest week1-integration/ert-available ()
  (should (featurep 'ert)))

(ert-deftest week1-integration/test-dir-exists ()
  (should (file-directory-p (expand-file-name "test" week1-test--project-root))))

(ert-deftest week1-integration/run-tests-exists ()
  (should (file-exists-p (expand-file-name "test/run-tests.el" week1-test--project-root))))

;; ── directory structure tests (Week 2) ──────────────────

(ert-deftest week2-integration/lisp-core-dir-exists ()
  (should (file-directory-p (expand-file-name "lisp/core" week1-test--project-root))))

(ert-deftest week2-integration/lisp-vr-dir-exists ()
  (should (file-directory-p (expand-file-name "lisp/vr" week1-test--project-root))))

(ert-deftest week2-integration/lisp-ext-dir-exists ()
  (should (file-directory-p (expand-file-name "lisp/ext" week1-test--project-root))))

;;; week1-integration-test.el ends here
