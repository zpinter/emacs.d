(require 'expand-region)
(global-set-key (kbd "C-?") 'er/expand-region)
(er/enable-mode-expansions 'sgml-mode 'er/add-html-mode-expansions)
