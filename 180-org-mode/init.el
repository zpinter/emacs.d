(add-module-path "org-mode/lisp")
(require 'org-install)

(add-hook 'org-mode-hook (lambda ()
                           (local-set-key [(control return)] 'set-mark-command)))

