(zconfig-add-lisp-path "lisp/lib/ert")
(zconfig-add-lisp-path "lisp/lib/fuzzy")
(zconfig-add-lisp-path "lisp/lib/popup")

(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories (concat zconfig-current-module-dir "/lisp/dict"))

(ac-config-default)
;; equivalent to...
;; (defun ac-config-default ()
;;   (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;;   (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;;   (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))

(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(global-auto-complete-mode t)

