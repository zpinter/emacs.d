(zconfig-add-lisp-path "lisp/clojure-mode")
(zconfig-add-lisp-path "lisp/swank-clojure")

(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(setq swank-clojure-jar-path "~/clojure/clojure-core/clojure.jar")

(setq swank-clojure-extra-classpaths
		(list
		 "~/clojure/clojure-contrib/clojure-contrib.jar"
		 "~/clojure/compojure/compojure.jar"))

(require 'swank-clojure-autoload)

(defun slime-clojure ()
  "Starts clojure in Slime"
  (interactive)
  (slime 'clojure))