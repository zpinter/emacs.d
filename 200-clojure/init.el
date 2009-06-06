(defvar clj-root (concat (expand-file-name "~") "/clojure/"))
(setq load-path (append (list (concat clj-root "slime")
			      (concat clj-root "slime/contrib")
			      (concat clj-root "clojure-mode")
			      (concat clj-root "swank-clojure"))
			load-path))


(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(require 'slime)
(slime-setup)

(setq swank-clojure-binary (concat clj-root "bin/clojure-emacs"))
(require 'swank-clojure-autoload)



