(add-to-list 'auto-mode-alist '("\\.html\\'" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . sgml-mode))
(require 'handlebars-sgml-mode)
(handlebars-use-mode 'global)
