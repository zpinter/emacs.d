(require 'haml-mode)
(add-hook 'haml-mode-hook
			 '(lambda ()
				 (setq indent-tabs-mode nil)
				 (define-key haml-mode-map "\C-m" 'newline-and-indent)))