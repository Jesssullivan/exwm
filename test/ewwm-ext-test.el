;;; ewwm-ext-test.el --- Tests for extension framework  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-ext)

(defmacro ewwm-ext-test--with-clean-registry (&rest body)
  "Run BODY with a clean extension registry."
  `(let ((ewwm-ext--registry (make-hash-table :test 'eq))
         (ewwm-ext--enabled nil))
     ,@body))

(ert-deftest ewwm-ext-test/register ()
  "Register an extension."
  (ewwm-ext-test--with-clean-registry
   (ewwm-ext-register 'test-ext :init-fn #'ignore :enable-fn #'ignore)
   (should (memq 'test-ext (ewwm-ext-list)))))

(ert-deftest ewwm-ext-test/enable-disable ()
  "Enable and disable an extension."
  (ewwm-ext-test--with-clean-registry
   (ewwm-ext-register 'test-ext :enable-fn #'ignore :disable-fn #'ignore)
   (ewwm-ext-enable 'test-ext)
   (should (ewwm-ext-enabled-p 'test-ext))
   (ewwm-ext-disable 'test-ext)
   (should-not (ewwm-ext-enabled-p 'test-ext))))

(ert-deftest ewwm-ext-test/dependency-resolution ()
  "Enabling extension A auto-enables dependency B."
  (ewwm-ext-test--with-clean-registry
   (ewwm-ext-register 'dep-b :enable-fn #'ignore)
   (ewwm-ext-register 'dep-a :enable-fn #'ignore :deps '(dep-b))
   (ewwm-ext-enable 'dep-a)
   (should (ewwm-ext-enabled-p 'dep-a))
   (should (ewwm-ext-enabled-p 'dep-b))))

(ert-deftest ewwm-ext-test/circular-dependency-detection ()
  "Circular dependencies are detected."
  (ewwm-ext-test--with-clean-registry
   (ewwm-ext-register 'circ-a :enable-fn #'ignore :deps '(circ-b))
   (ewwm-ext-register 'circ-b :enable-fn #'ignore :deps '(circ-a))
   (should-error (ewwm-ext-enable 'circ-a))))

(ert-deftest ewwm-ext-test/unknown-extension-error ()
  "Enabling unknown extension signals error."
  (ewwm-ext-test--with-clean-registry
   (should-error (ewwm-ext-enable 'nonexistent))))

(ert-deftest ewwm-ext-test/init-called-once ()
  "Init function called only once even with multiple enable/disable cycles."
  (ewwm-ext-test--with-clean-registry
   (let ((init-count 0))
     (ewwm-ext-register 'once-init
                         :init-fn (lambda () (cl-incf init-count))
                         :enable-fn #'ignore
                         :disable-fn #'ignore)
     (ewwm-ext-enable 'once-init)
     (ewwm-ext-disable 'once-init)
     (ewwm-ext-enable 'once-init)
     (should (= 1 init-count)))))

(ert-deftest ewwm-ext-test/list-extensions ()
  "List registered extensions."
  (ewwm-ext-test--with-clean-registry
   (ewwm-ext-register 'ext-a :enable-fn #'ignore)
   (ewwm-ext-register 'ext-b :enable-fn #'ignore)
   (let ((exts (ewwm-ext-list)))
     (should (memq 'ext-a exts))
     (should (memq 'ext-b exts)))))

;;; ewwm-ext-test.el ends here
