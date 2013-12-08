;;default to home directory
(setq default-directory "~/")

 ;;core functions
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

; prevent tramp from messing up recentf
(require 'recentf)
    (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
	 (recentf-mode 1)

(setq tramp-debug-buffer t)

;; (defun add-path (p)
;;   (add-to-list 'load-path
;;                (expand-file-name (concat emacsd p))))

(add-to-list 'auto-mode-alist
             '("\\.emacs\\-[a-z\\-]" . emacs-lisp-mode))


;; bug workaround
(setq warning-suppress-types nil)


                                        ;disable gui crap
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; show the tool bar on cocoa emacs
(if (and (ismac) window-system)
  (if (fboundp 'menu-bar-mode) (menu-bar-mode 1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq inhibit-splash-screen t)

(defcustom mf-display-padding-width 50
  "*Any extra display padding that you want to account for while
determining the maximize number of columns to fit on a display"
  :type 'integer
  :group 'maxframe)

;;; (when (and (ismac) window-system)
;;; ;;   (require 'maxframe)
;;; ;;   (setq mf-max-width 1600)
;;; ;;   (add-hook 'window-setup-hook 'maximize-frame t)

;;;   (require 'carbon-font)
;;;   (fixed-width-set-default-fontset
;;;    "-apple-inconsolata-medium-r-normal--14-*-*-*-*-*-iso10646-1"))

(if (and (islinux) window-system)
    (progn
		(require 'cua-base)
		(cua-selection-mode t)))		

(if (and (ismac) window-system)
    (progn

		(set-frame-font "-apple-inconsolata-medium-r-normal--14-*-*-*-*-*-iso10646-1")

		(require 'cua-base)
		(cua-mode t)

      ;; fix a mac-specific problem with ptys
      (setq process-connection-type nil)

		;; (mac-key-mode t)

      ;; Mac-style cut/copy/paste
      (setq mac-command-key-is-meta nil)
      (setq cua-enable-cua-keys nil)
		(global-set-key [(alt W)] 'delete-frame)
      (global-set-key [(alt x)] 'cua-cut-region)
      (global-set-key [(alt c)] 'cua-copy)
      (global-set-key [(alt v)] 'cua-paste)
      (global-set-key [(alt a)] 'mark-whole-buffer)
      (global-set-key [(alt s)] 'save-buffer)
      (global-set-key [(alt S)] 'write-file)
      (global-set-key [(alt p)] 'ps-print-buffer)
      (global-set-key [(alt o)] 'find-file)
      (global-set-key [(alt q)] 'save-buffers-kill-emacs)
      (global-set-key [(alt w)] 'kill-buffer-and-window)
      (global-set-key [(alt z)] 'undo)
      (global-set-key [(alt f)] 'isearch-forward)
      (global-set-key [(alt g)] 'query-replace)
      (global-set-key [(alt l)] 'goto-line)
      (global-set-key [(alt m)] 'iconify-frame)
      (global-set-key [(alt n)] 'new-frame)
      (global-set-key [kp-delete] 'delete-char)
      (global-set-key [(control kp-home)] 'beginning-of-buffer)
      (global-set-key [(control kp-end)] 'end-of-buffer)))

;; hippie-expand
(global-set-key (kbd "C-.") 'hippie-expand)

;default list
(setq hippie-expand-try-functions-list
  '(try-complete-file-name-partially
    try-complete-file-name
    try-expand-all-abbrevs
    try-expand-list
    try-expand-line
    try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol))

;;ido tweaks
(require 'ido)
(ido-mode t)
;; (ido-everywhere t)

(setq ido-enable-flex-matching t)      ; fuzzy matching is a must have
(setq ido-enable-tramp-completion t)
(setq tramp-default-method "ssh")

;; (setq ido-default-file-method 'selected-window)
;; (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)



;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq undo-tree-history-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; disable .#foo style symlinks
(setq create-lockfiles nil)

;; (defconst use-backup-dir t)

;; (defvar user-temporary-file-directory "/tmp/emacs")

;; (make-directory user-temporary-file-directory t)
;; (setq backup-directory-alist
;;       `(("." . ,user-temporary-file-directory) (,tramp-file-name-regexp nil))
;;       version-control t        ; Use version numbers for backups
;;       kept-new-versions 16     ; Number of newest versions to keep
;;       kept-old-versions 2      ; Number of oldest versions to keep
;;       delete-old-versions t    ; Ask to delete excess backup versions?
;;       backup-by-copying-when-linked t) ; Copy linked files, don't rename.
;; (setq auto-save-list-file-prefix
;;       (concat user-temporary-file-directory ".auto-saves-"))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,user-temporary-file-directory t)))

;; ;; (defvar user-temporary-file-directory-semantic (concat user-temporary-file-directory "/semantic-cache"))
;; ;; (make-directory user-temporary-file-directory-semantic t)
;; ;; (setq semanticdb-default-save-directory  user-temporary-file-directory-semantic)
