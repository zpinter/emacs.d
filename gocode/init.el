(zconfig-add-lisp-path "lisp/emacs")
(require 'go-autocomplete)


(defun setup-go-code ()
  (interactive)
  (auto-complete-mode 1)
  (setq ac-sources (append '(ac-source-go) ac-sources)))
