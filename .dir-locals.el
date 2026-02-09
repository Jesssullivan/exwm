;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (indent-tabs-mode . nil)
  (fill-column . 80)
  (sentence-end-double-space . t)
  (eval . (progn
            (let ((root (locate-dominating-file default-directory ".dir-locals.el")))
              (when root
                (dolist (dir '("lisp/core" "lisp/vr" "lisp/ext"))
                  (add-to-list 'load-path (expand-file-name dir root))))))))
 (emacs-lisp-mode
  (indent-tabs-mode . nil)
  (fill-column . 80))
 (rust-mode
  (indent-tabs-mode . nil)
  (fill-column . 100)))
