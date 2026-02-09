;;; ewwm-week5-test.el --- Week 5 tests for ewwm modules  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-workspace)
(require 'ewwm-layout)
(require 'ewwm-input)
(require 'ewwm-manage)
(require 'ewwm-floating)
(require 'ewwm-launch)

;; ── ewwm-core: surface-as-buffer model ──────────────────────

(ert-deftest ewwm-week5/create-surface-buffer ()
  "Creating a surface buffer sets buffer-local variables."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm--create-surface-buffer 42 "foot" "foot terminal")))
      (unwind-protect
          (progn
            (should (buffer-live-p buf))
            (should (string-prefix-p "*ewwm:" (buffer-name buf)))
            (should (= (buffer-local-value 'ewwm-surface-id buf) 42))
            (should (equal (buffer-local-value 'ewwm-app-id buf) "foot"))
            (should (equal (buffer-local-value 'ewwm-title buf) "foot terminal"))
            (should (eq (buffer-local-value 'ewwm-surface-state buf) 'managed)))
        (ewwm--destroy-surface-buffer 42)))))

(ert-deftest ewwm-week5/destroy-surface-buffer ()
  "Destroying a surface buffer removes it from alist and kills buffer."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm--create-surface-buffer 42 "test" "test")))
      (ewwm--destroy-surface-buffer 42)
      (should (null (ewwm--get-buffer 42)))
      (should-not (buffer-live-p buf)))))

(ert-deftest ewwm-week5/update-surface-title ()
  "Updating title changes buffer-local variable and renames buffer."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm--create-surface-buffer 42 "foot" "old title")))
      (unwind-protect
          (progn
            (ewwm--update-surface-title 42 "new title")
            (should (equal (buffer-local-value 'ewwm-title buf) "new title"))
            (should (string-match-p "new title" (buffer-name buf))))
        (ewwm--destroy-surface-buffer 42)))))

(ert-deftest ewwm-week5/update-surface-geometry ()
  "Updating geometry sets buffer-local geometry plist."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm--create-surface-buffer 42 "test" "test")))
      (unwind-protect
          (progn
            (ewwm--update-surface-geometry 42 '(:x 10 :y 20 :w 800 :h 600))
            (let ((geom (buffer-local-value 'ewwm-geometry buf)))
              (should (= (plist-get geom :x) 10))
              (should (= (plist-get geom :w) 800))))
        (ewwm--destroy-surface-buffer 42)))))

(ert-deftest ewwm-week5/buffers-on-workspace ()
  "Query buffers on a specific workspace."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((b1 (ewwm--create-surface-buffer 1 "app1" "app1"))
          (b2 (ewwm--create-surface-buffer 2 "app2" "app2")))
      (with-current-buffer b1 (setq ewwm-workspace 0))
      (with-current-buffer b2 (setq ewwm-workspace 1))
      (unwind-protect
          (progn
            (should (= 1 (length (ewwm--buffers-on-workspace 0))))
            (should (= 1 (length (ewwm--buffers-on-workspace 1))))
            (should (= 0 (length (ewwm--buffers-on-workspace 2)))))
        (ewwm--destroy-surface-buffer 1)
        (ewwm--destroy-surface-buffer 2)))))

(ert-deftest ewwm-week5/surface-buffer-p ()
  "Predicate detects ewwm surface buffers."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm--create-surface-buffer 1 "test" "test")))
      (unwind-protect
          (progn
            (should (ewwm--surface-buffer-p buf))
            (should-not (ewwm--surface-buffer-p (get-buffer-create "*scratch*"))))
        (ewwm--destroy-surface-buffer 1)))))

;; ── ewwm-mode ────────────────────────────────────────────────

(ert-deftest ewwm-week5/ewwm-mode-derived ()
  "ewwm-mode is derived from special-mode."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm--create-surface-buffer 1 "test" "test")))
      (unwind-protect
          (with-current-buffer buf
            (ewwm-mode)
            (should (derived-mode-p 'ewwm-mode))
            (should (derived-mode-p 'special-mode)))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-week5/ewwm-mode-buffer-content ()
  "ewwm-mode buffer contains surface information."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm--create-surface-buffer 1 "foot" "foot terminal")))
      (unwind-protect
          (with-current-buffer buf
            (ewwm-mode)
            (should (string-match-p "foot terminal" (buffer-string)))
            (should (string-match-p "App ID: foot" (buffer-string)))
            (should (string-match-p "Surface ID: 1" (buffer-string))))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-week5/ewwm-mode-keymap ()
  "ewwm-mode keymap has expected bindings."
  (should (keymapp ewwm-mode-map))
  (should (eq (lookup-key ewwm-mode-map "q") 'ewwm-mode-close-surface))
  (should (eq (lookup-key ewwm-mode-map "f") 'ewwm-mode-toggle-floating))
  (should (eq (lookup-key ewwm-mode-map "F") 'ewwm-mode-toggle-fullscreen))
  (should (eq (lookup-key ewwm-mode-map "m") 'ewwm-mode-move-to-workspace))
  (should (eq (lookup-key ewwm-mode-map "i") 'ewwm-mode-surface-info)))

(ert-deftest ewwm-week5/ewwm-mode-modeline ()
  "ewwm-mode sets a custom mode-line."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm--create-surface-buffer 1 "foot" "foot terminal")))
      (unwind-protect
          (with-current-buffer buf
            (ewwm-mode)
            (should (listp mode-line-format))
            ;; Mode-line should reference ewwm-title symbol
            (should (member 'ewwm-title (flatten-tree mode-line-format))))
        (ewwm--destroy-surface-buffer 1)))))

;; ── ewwm-workspace ───────────────────────────────────────────

(ert-deftest ewwm-week5/workspace-init ()
  "Workspace initialization creates configs and names vectors."
  (let ((ewwm-workspace-number 4)
        (ewwm-workspace--configs nil)
        (ewwm-workspace--names nil)
        (ewwm-workspace-current-index 0))
    (ewwm-workspace--init)
    (should (vectorp ewwm-workspace--configs))
    (should (= (length ewwm-workspace--configs) 4))
    (should (vectorp ewwm-workspace--names))
    (should (equal (aref ewwm-workspace--names 0) "ws-0"))))

(ert-deftest ewwm-week5/workspace-switch ()
  "Workspace switch updates current index."
  (let ((ewwm-workspace-number 4)
        (ewwm-workspace--configs nil)
        (ewwm-workspace--names nil)
        (ewwm-workspace-current-index 0)
        (ewwm-workspace-switch-hook nil)
        (ewwm--surface-buffer-alist nil))
    (ewwm-workspace--init)
    (ewwm-workspace-switch 2)
    (should (= ewwm-workspace-current-index 2))))

(ert-deftest ewwm-week5/workspace-switch-bounds ()
  "Workspace switch rejects out-of-range indices."
  (let ((ewwm-workspace-number 4)
        (ewwm-workspace--configs nil)
        (ewwm-workspace--names nil)
        (ewwm-workspace-current-index 0)
        (ewwm-workspace-switch-hook nil)
        (ewwm--surface-buffer-alist nil))
    (ewwm-workspace--init)
    (ewwm-workspace-switch -1)
    (should (= ewwm-workspace-current-index 0))
    (ewwm-workspace-switch 99)
    (should (= ewwm-workspace-current-index 0))))

(ert-deftest ewwm-week5/workspace-switch-hook ()
  "Workspace switch hook fires with from/to indices."
  (let ((ewwm-workspace-number 4)
        (ewwm-workspace--configs nil)
        (ewwm-workspace--names nil)
        (ewwm-workspace-current-index 0)
        (ewwm--surface-buffer-alist nil)
        (hook-args nil))
    (let ((ewwm-workspace-switch-hook
           (list (lambda (from to) (setq hook-args (list from to))))))
      (ewwm-workspace--init)
      (ewwm-workspace-switch 2)
      (should (equal hook-args '(0 2))))))

(ert-deftest ewwm-week5/workspace-list ()
  "Workspace list returns plists for all workspaces."
  (let ((ewwm-workspace-number 4)
        (ewwm-workspace--configs nil)
        (ewwm-workspace--names nil)
        (ewwm-workspace-current-index 0)
        (ewwm--surface-buffer-alist nil))
    (ewwm-workspace--init)
    (let ((ws (ewwm-workspace-list)))
      (should (= (length ws) 4))
      (should (plist-get (car ws) :current)))))

(ert-deftest ewwm-week5/workspace-name ()
  "Workspace name get/set works."
  (let ((ewwm-workspace-number 4)
        (ewwm-workspace--configs nil)
        (ewwm-workspace--names nil)
        (ewwm-workspace-current-index 0))
    (ewwm-workspace--init)
    (should (equal (ewwm-workspace-name 0) "ws-0"))
    (ewwm-workspace-set-name 0 "main")
    (should (equal (ewwm-workspace-name 0) "main"))))

;; ── ewwm-layout ──────────────────────────────────────────────

(ert-deftest ewwm-week5/layout-default ()
  "Default layout is tiling."
  (should (eq ewwm-layout-default 'tiling)))

(ert-deftest ewwm-week5/layout-cycle ()
  "Layout cycle rotates through available layouts."
  (let ((ewwm-layout--current 'tiling)
        (ewwm-layout--cycle-list '(tiling monocle grid))
        (ewwm-layout-change-hook nil)
        (ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0))
    (ewwm-layout-cycle)
    (should (eq ewwm-layout--current 'monocle))
    (ewwm-layout-cycle)
    (should (eq ewwm-layout--current 'grid))
    (ewwm-layout-cycle)
    (should (eq ewwm-layout--current 'tiling))))

(ert-deftest ewwm-week5/layout-set ()
  "Layout set changes current layout."
  (let ((ewwm-layout--current 'tiling)
        (ewwm-layout--cycle-list '(tiling monocle grid))
        (ewwm-layout-change-hook nil)
        (ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0))
    (ewwm-layout-set 'monocle)
    (should (eq (ewwm-layout-current) 'monocle))))

(ert-deftest ewwm-week5/layout-usable-area ()
  "Usable area can be set and queried."
  (let ((ewwm-layout--usable-area nil))
    (ewwm-layout--set-usable-area '(:x 0 :y 30 :w 1920 :h 1050))
    (should (= (plist-get ewwm-layout--usable-area :y) 30))))

;; ── ewwm-input ───────────────────────────────────────────────

(ert-deftest ewwm-week5/input-set-key ()
  "Input set-key stores key binding."
  (let ((ewwm-input--global-keys nil))
    (ewwm-input-set-key "s-r" #'ignore)
    (should (eq (alist-get "s-r" ewwm-input--global-keys nil nil #'equal)
                #'ignore))))

(ert-deftest ewwm-week5/input-unset-key ()
  "Input unset-key removes key binding."
  (let ((ewwm-input--global-keys nil))
    (ewwm-input-set-key "s-r" #'ignore)
    (ewwm-input-unset-key "s-r")
    (should-not (alist-get "s-r" ewwm-input--global-keys nil nil #'equal))))

(ert-deftest ewwm-week5/input-handle-key-event ()
  "Key event dispatch calls the right command."
  (let ((ewwm-input--global-keys nil)
        (called nil))
    (ewwm-input-set-key "s-r" (lambda () (interactive) (setq called t)))
    (ewwm-input--handle-key-event '(:key "s-r"))
    (should called)))

(ert-deftest ewwm-week5/input-setup-defaults ()
  "Default key setup registers workspace and utility keys."
  (let ((ewwm-input--global-keys nil))
    (ewwm-input--setup-defaults)
    ;; Should have workspace keys s-1 through s-9
    (should (alist-get "s-1" ewwm-input--global-keys nil nil #'equal))
    (should (alist-get "s-9" ewwm-input--global-keys nil nil #'equal))
    ;; Should have move-to-workspace keys
    (should (alist-get "s-S-1" ewwm-input--global-keys nil nil #'equal))
    ;; Should have layout cycle
    (should (alist-get "s-SPC" ewwm-input--global-keys nil nil #'equal))
    ;; Should have app launcher
    (should (alist-get "s-&" ewwm-input--global-keys nil nil #'equal))))

;; ── ewwm-floating ────────────────────────────────────────────

(ert-deftest ewwm-week5/floating-toggle ()
  "Floating toggle changes surface state."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0))
    (let ((buf (ewwm--create-surface-buffer 1 "test" "test")))
      (unwind-protect
          (progn
            (should (eq (buffer-local-value 'ewwm-surface-state buf) 'managed))
            (ewwm-floating-toggle 1)
            (should (eq (buffer-local-value 'ewwm-surface-state buf) 'floating))
            (ewwm-floating-toggle 1)
            (should (eq (buffer-local-value 'ewwm-surface-state buf) 'managed)))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-week5/floating-list ()
  "Floating list returns only floating buffers."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0))
    (ewwm--create-surface-buffer 1 "app1" "app1")
    (let ((b2 (ewwm--create-surface-buffer 2 "app2" "app2")))
      (with-current-buffer b2 (setq ewwm-surface-state 'floating))
      (unwind-protect
          (progn
            (should (= (length (ewwm-floating-list)) 1))
            (should (eq (car (ewwm-floating-list)) b2)))
        (ewwm--destroy-surface-buffer 1)
        (ewwm--destroy-surface-buffer 2)))))

(ert-deftest ewwm-week5/floating-p ()
  "Floating predicate detects floating buffers."
  (let ((ewwm--surface-buffer-alist nil))
    (let ((buf (ewwm--create-surface-buffer 1 "test" "test")))
      (unwind-protect
          (progn
            (should-not (ewwm-floating-p buf))
            (with-current-buffer buf (setq ewwm-surface-state 'floating))
            (should (ewwm-floating-p buf)))
        (ewwm--destroy-surface-buffer 1)))))

;; ── ewwm-launch ──────────────────────────────────────────────

(ert-deftest ewwm-week5/launch-favorites-defined ()
  "Launch favorites has default entries."
  (should (listp ewwm-launch-favorites))
  (should (> (length ewwm-launch-favorites) 0)))

(ert-deftest ewwm-week5/launch-env-vars-defined ()
  "Launch env vars includes Wayland platform vars."
  (should (assoc "QT_QPA_PLATFORM" ewwm-launch-env-vars))
  (should (assoc "MOZ_ENABLE_WAYLAND" ewwm-launch-env-vars)))

;; ── ewwm.el orchestration ────────────────────────────────────

(ert-deftest ewwm-week5/features-loaded ()
  "All ewwm submodules are loadable."
  (should (featurep 'ewwm-core))
  (should (featurep 'ewwm-workspace))
  (should (featurep 'ewwm-layout))
  (should (featurep 'ewwm-input))
  (should (featurep 'ewwm-manage))
  (should (featurep 'ewwm-floating))
  (should (featurep 'ewwm-launch)))

(ert-deftest ewwm-week5/ewwm-init-function-exists ()
  "ewwm-init is defined."
  (require 'ewwm)
  (should (fboundp 'ewwm-init)))

(ert-deftest ewwm-week5/ewwm-exit-function-exists ()
  "ewwm-exit is defined."
  (require 'ewwm)
  (should (fboundp 'ewwm-exit)))

(ert-deftest ewwm-week5/ewwm-reset-function-exists ()
  "ewwm-reset is defined."
  (require 'ewwm)
  (should (fboundp 'ewwm-reset)))

(ert-deftest ewwm-week5/focus-policy-customizable ()
  "Focus policy is a defcustom."
  (require 'ewwm)
  (should (boundp 'ewwm-focus-policy))
  (should (memq (symbol-value 'ewwm-focus-policy)
                '(follow-emacs follow-compositor manual))))

;; ── Integration: surface lifecycle ───────────────────────────

(ert-deftest ewwm-week5/surface-lifecycle-create-destroy ()
  "Full surface lifecycle: create -> verify -> destroy -> verify."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 100 :app-id "foot" :title "foot terminal"))))
      ;; After create
      (should (= (ewwm--surface-count) 1))
      (should (buffer-live-p buf))
      (should (string-match-p "foot" (buffer-name buf)))
      ;; Destroy
      (ewwm-manage--on-destroy '(:id 100))
      ;; After destroy
      (should (= (ewwm--surface-count) 0))
      (should-not (buffer-live-p buf)))))

(ert-deftest ewwm-week5/surface-lifecycle-multiple ()
  "Managing multiple surfaces and tracking them."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (ewwm-manage--on-create '(:id 1 :app-id "foot" :title "term1"))
    (let ((b2 (ewwm-manage--on-create '(:id 2 :app-id "qutebrowser" :title "browser"))))
      (ewwm-manage--on-create '(:id 3 :app-id "keepassxc" :title "passwords"))
      (unwind-protect
          (progn
            (should (= (ewwm--surface-count) 3))
            (should (equal (sort (ewwm--all-surfaces) #'<) '(1 2 3)))
            ;; Destroy one
            (ewwm-manage--on-destroy '(:id 2))
            (should (= (ewwm--surface-count) 2))
            (should-not (buffer-live-p b2)))
        ;; Cleanup
        (dolist (id '(1 2 3))
          (ewwm--destroy-surface-buffer id))))))

;;; ewwm-week5-test.el ends here
