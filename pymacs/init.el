;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

(eval-after-load "python-mode"
  '(progn
     ;; Do whatever you need to do here, it will only get executed after python-mode.el has loaded
     (require 'pymacs)
     (pymacs-load "ropemacs" "rope-")
	  (setq ropemacs-enable-autoimport t)))
