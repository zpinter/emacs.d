(zconfig-add-lisp-path "/lisp/vendor")

(custom-set-variables
 '(eclim-eclipse-dirs '("/Applications/eclipse-latest")))

(require 'eclim)
(setq eclim-auto-save t)
(global-eclim-mode)

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

(require 'ac-emacs-eclim-source)
(add-hook 'eclim-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim)))

(add-to-list 'ac-dictionary-directories (concat zconfig-current-module-dir "/lisp/dict"))
