(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories (concat zconfig-current-module-dir "/lisp/dict"))
(ac-config-default)

(global-auto-complete-mode t)
