(add-module-path "org-mode/lisp")
(require 'org-install)

(add-hook 'org-mode-hook (lambda ()
                           (local-set-key [(control return)] 'set-mark-command)))

;; (setq font-lock-maximum-decoration
;;       '((org-mode . nil) (tex-mode . nil) (latex-mode . nil)))

; (eval-after-load "tex-mode" '(fset 'tex-font-lock-suscript 'ignore))

(setq org-export-with-sub-superscripts nil)