(require 'ido)
(ido-mode t)

(setq ido-enable-flex-matching t) ; fuzzy matching is a must have

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
