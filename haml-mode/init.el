(autoload 'haml-mode "haml-mode" "Major mode for editing haml files." t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;; (require 'haml-mode)
(add-hook 'haml-mode-hook
			 '(lambda ()
				 (setq indent-tabs-mode nil)
				 (define-key haml-mode-map "\C-m" 'newline-and-indent)))
