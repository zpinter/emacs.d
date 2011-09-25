 ;;core functions

;; (eval-after-load "tramp"
;;   '(debug))

; prevent tramp from messing up recentf
(require 'recentf)
    (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
    (recentf-mode 1)

(defun add-path (p)
  (add-to-list 'load-path
               (expand-file-name (concat emacsd p))))

(add-to-list 'auto-mode-alist
             '("\\.emacs\\-[a-z\\-]" . emacs-lisp-mode))

(defun ismac ()
	 (or (eq system-type "darwin") (eq system-type 'darwin)))

;; bug workaround
(setq warning-suppress-types nil)