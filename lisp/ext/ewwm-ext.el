;;; ewwm-ext.el --- Extension framework for EWWM  -*- lexical-binding: t -*-

;;; Commentary:
;; Lightweight extension loading framework with lifecycle hooks and
;; dependency resolution.  Extensions register with `ewwm-ext-register'
;; and are enabled/disabled individually.

;;; Code:

(require 'cl-lib)

(defgroup ewwm-ext nil
  "EWWM extension framework."
  :group 'ewwm)

(defcustom ewwm-ext-enabled-list nil
  "List of extension names to auto-enable on startup."
  :type '(repeat symbol))

(defvar ewwm-ext--registry (make-hash-table :test 'eq)
  "Hash table of registered extensions.  Key: name, Value: plist.")

(defvar ewwm-ext--enabled nil
  "List of currently enabled extension names.")

(cl-defun ewwm-ext-register (name &key init-fn enable-fn disable-fn deps)
  "Register extension NAME with lifecycle hooks.
INIT-FN is called once on first enable.
ENABLE-FN is called each time the extension is enabled.
DISABLE-FN is called each time the extension is disabled.
DEPS is a list of extension names that must be enabled first."
  (puthash name
           (list :init-fn init-fn
                 :enable-fn enable-fn
                 :disable-fn disable-fn
                 :deps (or deps nil)
                 :initialized nil)
           ewwm-ext--registry))

(defun ewwm-ext--check-circular (name &optional visited)
  "Check for circular dependencies starting from NAME.
VISITED is the set of already-seen names."
  (let ((visited (or visited nil)))
    (when (memq name visited)
      (error "Circular dependency detected: %s" name))
    (let* ((ext (gethash name ewwm-ext--registry))
           (deps (plist-get ext :deps)))
      (dolist (dep deps)
        (ewwm-ext--check-circular dep (cons name visited))))))

(defun ewwm-ext-enable (name)
  "Enable extension NAME, resolving dependencies first."
  (unless (gethash name ewwm-ext--registry)
    (error "Unknown extension: %s" name))
  (unless (memq name ewwm-ext--enabled)
    (ewwm-ext--check-circular name)
    (let ((ext (gethash name ewwm-ext--registry)))
      ;; Enable dependencies first
      (dolist (dep (plist-get ext :deps))
        (ewwm-ext-enable dep))
      ;; Initialize if first time
      (unless (plist-get ext :initialized)
        (when-let ((init-fn (plist-get ext :init-fn)))
          (condition-case err
              (funcall init-fn)
            (error (message "EWWM ext: init failed for %s: %s" name err))))
        (plist-put ext :initialized t))
      ;; Enable
      (when-let ((enable-fn (plist-get ext :enable-fn)))
        (funcall enable-fn))
      (push name ewwm-ext--enabled))))

(defun ewwm-ext-disable (name)
  "Disable extension NAME."
  (when (memq name ewwm-ext--enabled)
    (let ((ext (gethash name ewwm-ext--registry)))
      (when-let ((disable-fn (plist-get ext :disable-fn)))
        (funcall disable-fn))
      (setq ewwm-ext--enabled (delq name ewwm-ext--enabled)))))

(defun ewwm-ext-list ()
  "Return list of registered extension names."
  (let (names)
    (maphash (lambda (k _v) (push k names)) ewwm-ext--registry)
    (nreverse names)))

(defun ewwm-ext-enabled-p (name)
  "Return non-nil if extension NAME is enabled."
  (memq name ewwm-ext--enabled))

(provide 'ewwm-ext)
;;; ewwm-ext.el ends here
