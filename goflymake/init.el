(zconfig-add-lisp-path "lisp/goflymake")

(setenv "GOPATH" (expand-file-name "~/gocode"))
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/gocode/bin")))
(push (expand-file-name "~/gocode/bin") exec-path)

(setq goflymake-debug nil)

(require 'go-flycheck)

;; (setenv "PATH" (concat (getenv "PATH") (concat zconfig-current-module-dir "/lisp/goflymake")))
;; (setq exec-path (append exec-path '(concat zconfig-current-module-dir "/lisp/goflymake")))
