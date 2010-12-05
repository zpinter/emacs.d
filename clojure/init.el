(zconfig-add-lisp-path "lisp/clojure-mode")

(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))