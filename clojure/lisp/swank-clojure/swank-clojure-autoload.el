
(autoload 'swank-clojure-init "swank-clojure" "Initialize clojure for swank")
(autoload 'swank-clojure-cmd "swank-clojure" "Command to start clojure")

(eval-after-load "slime"
  '(progn
     (require 'swank-clojure)
     (add-to-list 'slime-lisp-implementations `(clojure ,(swank-clojure-cmd) :init swank-clojure-init) t)
     (add-hook 'slime-indentation-update-hooks 'swank-clojure-update-indentation)
     (add-hook 'slime-repl-mode-hook 'swank-clojure-slime-repl-modify-syntax t)
     (add-hook 'clojure-mode-hook 'swank-clojure-slime-mode-hook t)))

(defmacro swank-clojure-config (&rest body)
  `(eval-after-load "swank-clojure"
     '(progn
        ,@body)))

(provide 'swank-clojure-autoload)