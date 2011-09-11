(require 'deft)

(setq deft-extension "txt")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/org")

(global-set-key [f5] 'deft)