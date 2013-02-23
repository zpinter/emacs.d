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

(defun islinux ()
	 (or (eq system-type "gnu/linux") (eq system-type 'gnu/linux)))

(defun ismac ()
	 (or (eq system-type "darwin") (eq system-type 'darwin)))

(defun iswindows ()
  (or 
   (eq system-type "cygwin") 
   (eq system-type 'cygwin) 
   (eq system-type "windows-nt") 
   (eq system-type 'windows-nt)))

;; bug workaround
(setq warning-suppress-types nil)
