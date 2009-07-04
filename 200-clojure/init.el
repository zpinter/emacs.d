(zconfig-add-lisp-path "lisp/clojure-mode")
(zconfig-add-lisp-path "lisp/slime")
(zconfig-add-lisp-path "lisp/slime/contrib")
(zconfig-add-lisp-path "lisp/swank-clojure")

(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;(setq swank-clojure-binary "/Users/zpinter/clojure/bin/clojure-emacs")

(setq swank-clojure-jar-path "~/clojure/clojure-core/clojure.jar")

(setq swank-clojure-extra-classpaths
		(list
		 "~/clojure/clojure-contrib/clojure-contrib.jar"
		 "~/clojure/compojure/compojure.jar"))

(require 'slime-autoloads)
(require 'swank-clojure-autoload)