;;; ewwm-gesture-test.el --- Tests for gesture recognition module  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-vr-gesture)

;; Forward-declare functions from module under test
(declare-function ewwm-vr-gesture-default-bindings "ewwm-vr-gesture")
(declare-function ewwm-vr-gesture-bind "ewwm-vr-gesture")
(declare-function ewwm-vr-gesture-unbind "ewwm-vr-gesture")
(declare-function ewwm-vr-gesture--lookup "ewwm-vr-gesture")
(declare-function ewwm-vr-gesture--on-started "ewwm-vr-gesture")
(declare-function ewwm-vr-gesture--on-swipe "ewwm-vr-gesture")
(declare-function ewwm-vr-gesture-status "ewwm-vr-gesture")
(declare-function ewwm-vr-gesture-toggle "ewwm-vr-gesture")
(declare-function ewwm-vr-gesture-list-bindings "ewwm-vr-gesture")
(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-vr-gesture-enable)
(defvar ewwm-vr-gesture-pinch-threshold)
(defvar ewwm-vr-gesture-grab-threshold)
(defvar ewwm-vr-gesture-swipe-min-velocity)
(defvar ewwm-vr-gesture-debounce-ms)
(defvar ewwm-vr-gesture-verbose)
(defvar ewwm-vr-gesture--bindings)
(defvar ewwm-vr-gesture--last-gesture)
(defvar ewwm-vr-gesture--last-time)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest ewwm-gesture/provides-feature ()
  "ewwm-vr-gesture provides its feature."
  (should (featurep 'ewwm-vr-gesture)))

(ert-deftest ewwm-gesture/group-exists ()
  "ewwm-vr-gesture customization group exists."
  (should (get 'ewwm-vr-gesture 'custom-group)))

;; ── Defcustom defaults ──────────────────────────────────────

(ert-deftest ewwm-gesture/enable-default ()
  "Default enable is t."
  (should (eq (default-value 'ewwm-vr-gesture-enable) t)))

(ert-deftest ewwm-gesture/pinch-threshold-default ()
  "Default pinch-threshold is 0.02."
  (should (= (default-value 'ewwm-vr-gesture-pinch-threshold) 0.02)))

(ert-deftest ewwm-gesture/grab-threshold-default ()
  "Default grab-threshold is 0.04."
  (should (= (default-value 'ewwm-vr-gesture-grab-threshold) 0.04)))

(ert-deftest ewwm-gesture/swipe-min-velocity-default ()
  "Default swipe-min-velocity is 0.5."
  (should (= (default-value 'ewwm-vr-gesture-swipe-min-velocity) 0.5)))

(ert-deftest ewwm-gesture/debounce-ms-default ()
  "Default debounce-ms is 200."
  (should (= (default-value 'ewwm-vr-gesture-debounce-ms) 200)))

(ert-deftest ewwm-gesture/verbose-default ()
  "Default verbose is nil."
  (should-not (default-value 'ewwm-vr-gesture-verbose)))

;; ── Binding system ──────────────────────────────────────────

(ert-deftest ewwm-gesture/default-bindings-populated ()
  "Default bindings are populated."
  (let ((bindings (ewwm-vr-gesture-default-bindings)))
    (should (listp bindings))
    (should (> (length bindings) 0))))

(ert-deftest ewwm-gesture/bind-adds-to-alist ()
  "gesture-bind adds a binding to the alist."
  (let ((ewwm-vr-gesture--bindings nil))
    (ewwm-vr-gesture-bind 'right 'pinch #'ignore)
    (should (= (length ewwm-vr-gesture--bindings) 1))
    (should (eq (cdr (car ewwm-vr-gesture--bindings)) #'ignore))))

(ert-deftest ewwm-gesture/bind-replaces-existing ()
  "gesture-bind replaces an existing binding."
  (let ((ewwm-vr-gesture--bindings nil))
    (ewwm-vr-gesture-bind 'right 'pinch #'ignore)
    (ewwm-vr-gesture-bind 'right 'pinch #'identity)
    (should (= (length ewwm-vr-gesture--bindings) 1))
    (should (eq (cdr (car ewwm-vr-gesture--bindings)) #'identity))))

(ert-deftest ewwm-gesture/unbind-removes-from-alist ()
  "gesture-unbind removes a binding from the alist."
  (let ((ewwm-vr-gesture--bindings nil))
    (ewwm-vr-gesture-bind 'right 'pinch #'ignore)
    (ewwm-vr-gesture-bind 'left 'grab #'identity)
    (ewwm-vr-gesture-unbind 'right 'pinch)
    (should (= (length ewwm-vr-gesture--bindings) 1))
    (should (equal (caar ewwm-vr-gesture--bindings) '(left . grab)))))

(ert-deftest ewwm-gesture/lookup-finds-binding ()
  "Binding lookup finds the correct command."
  (let ((ewwm-vr-gesture--bindings nil))
    (ewwm-vr-gesture-bind 'right 'pinch #'ignore)
    (should (eq (ewwm-vr-gesture--lookup 'right 'pinch) #'ignore))))

(ert-deftest ewwm-gesture/lookup-returns-nil-unbound ()
  "Binding lookup returns nil for unbound gesture."
  (let ((ewwm-vr-gesture--bindings nil))
    (should-not (ewwm-vr-gesture--lookup 'right 'pinch))))

;; ── Event dispatch ──────────────────────────────────────────

(ert-deftest ewwm-gesture/on-started-calls-bound-command ()
  "on-started calls the bound command for a gesture."
  (let ((ewwm-vr-gesture-enable t)
        (ewwm-vr-gesture--bindings nil)
        (ewwm-vr-gesture--last-gesture nil)
        (ewwm-vr-gesture--last-time nil)
        (ewwm-vr-gesture-verbose nil)
        (called nil))
    (ewwm-vr-gesture-bind 'right 'pinch (lambda () (setq called t)))
    (ewwm-vr-gesture--on-started '(:hand right :gesture pinch))
    (should called)))

(ert-deftest ewwm-gesture/on-started-does-nothing-unbound ()
  "on-started does nothing for unbound gestures."
  (let ((ewwm-vr-gesture-enable t)
        (ewwm-vr-gesture--bindings nil)
        (ewwm-vr-gesture--last-gesture nil)
        (ewwm-vr-gesture--last-time nil)
        (ewwm-vr-gesture-verbose nil)
        (called nil))
    (ewwm-vr-gesture--on-started '(:hand left :gesture grab))
    (should-not called)))

(ert-deftest ewwm-gesture/on-started-disabled-no-dispatch ()
  "on-started does nothing when disabled."
  (let ((ewwm-vr-gesture-enable nil)
        (ewwm-vr-gesture--bindings nil)
        (ewwm-vr-gesture--last-gesture nil)
        (ewwm-vr-gesture--last-time nil)
        (ewwm-vr-gesture-verbose nil)
        (called nil))
    (ewwm-vr-gesture-bind 'right 'pinch (lambda () (setq called t)))
    (ewwm-vr-gesture--on-started '(:hand right :gesture pinch))
    (should-not called)))

(ert-deftest ewwm-gesture/on-swipe-dispatches ()
  "on-swipe dispatches swipe direction gestures."
  (let ((ewwm-vr-gesture-enable t)
        (ewwm-vr-gesture--bindings nil)
        (ewwm-vr-gesture--last-gesture nil)
        (ewwm-vr-gesture--last-time nil)
        (ewwm-vr-gesture-verbose nil)
        (called nil))
    (ewwm-vr-gesture-bind 'left 'swipe-left (lambda () (setq called t)))
    (ewwm-vr-gesture--on-swipe '(:hand left :direction left))
    (should called)))

(ert-deftest ewwm-gesture/on-swipe-unbound-no-dispatch ()
  "on-swipe does nothing for unbound direction."
  (let ((ewwm-vr-gesture-enable t)
        (ewwm-vr-gesture--bindings nil)
        (ewwm-vr-gesture--last-gesture nil)
        (ewwm-vr-gesture--last-time nil)
        (ewwm-vr-gesture-verbose nil)
        (called nil))
    (ewwm-vr-gesture--on-swipe '(:hand left :direction up))
    (should-not called)))

;; ── Verbose mode ────────────────────────────────────────────

(ert-deftest ewwm-gesture/verbose-mode-logs ()
  "Verbose mode logs gesture events."
  (let ((ewwm-vr-gesture-enable t)
        (ewwm-vr-gesture--bindings nil)
        (ewwm-vr-gesture--last-gesture nil)
        (ewwm-vr-gesture--last-time nil)
        (ewwm-vr-gesture-verbose t)
        (logged nil))
    (cl-letf (((symbol-function 'message)
               (lambda (_fmt &rest _args) (setq logged t))))
      (ewwm-vr-gesture--on-started '(:hand right :gesture pinch))
      (should logged))))

;; ── Interactive command checks ──────────────────────────────

(ert-deftest ewwm-gesture/status-interactive ()
  "ewwm-vr-gesture-status is interactive."
  (should (commandp 'ewwm-vr-gesture-status)))

(ert-deftest ewwm-gesture/toggle-interactive ()
  "ewwm-vr-gesture-toggle is interactive."
  (should (commandp 'ewwm-vr-gesture-toggle)))

(ert-deftest ewwm-gesture/list-bindings-interactive ()
  "ewwm-vr-gesture-list-bindings is interactive."
  (should (commandp 'ewwm-vr-gesture-list-bindings)))

;; ── Toggle behavior ─────────────────────────────────────────

(ert-deftest ewwm-gesture/toggle-flips-enable ()
  "Toggle flips the enable flag."
  (let ((ewwm-vr-gesture-enable t))
    (ewwm-vr-gesture-toggle)
    (should-not ewwm-vr-gesture-enable)
    (ewwm-vr-gesture-toggle)
    (should ewwm-vr-gesture-enable)))

;; ── Hook runs on dispatch ───────────────────────────────────

(ert-deftest ewwm-gesture/hook-runs-on-dispatch ()
  "Gesture hook runs after command dispatch."
  (let ((ewwm-vr-gesture-enable t)
        (ewwm-vr-gesture--bindings nil)
        (ewwm-vr-gesture--last-gesture nil)
        (ewwm-vr-gesture--last-time nil)
        (ewwm-vr-gesture-verbose nil)
        (ewwm-vr-gesture-hook nil)
        (hook-called nil))
    (ewwm-vr-gesture-bind 'right 'pinch #'ignore)
    (let ((ewwm-vr-gesture-hook
           (list (lambda (_hand _gesture _cmd) (setq hook-called t)))))
      (ewwm-vr-gesture--on-started '(:hand right :gesture pinch))
      (should hook-called))))

;;; ewwm-gesture-test.el ends here
