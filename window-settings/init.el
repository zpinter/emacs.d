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

