(require 'clojure-mode)

(defun turn-on-paredit () 
  (show-paren-mode 1)
  (paredit-mode 1))

(add-hook 'clojure-mode-hook 'turn-on-paredit)


(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))
