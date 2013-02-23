 ;;core functions

; prevent tramp from messing up recentf
(require 'recentf)
    (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
	 (recentf-mode 1)

(setq tramp-debug-buffer t)

(defun add-path (p)
  (add-to-list 'load-path
               (expand-file-name (concat emacsd p))))

(add-to-list 'auto-mode-alist
             '("\\.emacs\\-[a-z\\-]" . emacs-lisp-mode))


;; bug workaround
(setq warning-suppress-types nil)
