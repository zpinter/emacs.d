(zconfig-add-lisp-path "lisp/contrib")
(require 'slime-autoloads)

(setq slime-lisp-implementations nil)
(add-to-list 'slime-lisp-implementations '(sbcl ("/opt/local/bin/sbcl")))
