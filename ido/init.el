(require 'ido)
(ido-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t)      ; fuzzy matching is a must have
(setq ido-enable-tramp-completion t)
(setq tramp-default-method "ssh")

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; (icomplete-mode 1)
;; (setq icomplete-compute-delay 0)
;; (require 'icomplete+)

(defalias 'ido-list-directory 'ido-dired)

;; (add-hook 'ido-setup-hook
;;           (lambda ()
;;             (define-key ido-completion-map "\C-l" 'ido-open-shell)))
;;
;; ;; trying to get this to work, not much luck
;; (defun ido-open-shell-old ()
;;   (interactive)
;;   ;; (shell (concat ido-current-directory (or ido-text "")))
;;   (shell-current-directory)
;;   (setq ido-exit 'fallback)
;;   (exit-minibuffer))
;;
;; (defun ido-open-shell ()
;;   "Call `dired' the ido way.
;; The directory is selected interactively by typing a substring.
;; For details of keybindings, see `ido-find-file'."
;;   (interactive)
;;   (let ((ido-report-no-match nil)
;; 	(ido-auto-merge-work-directories-length -1))
;;     (ido-file-internal 'shell 'shell nil "Shell: " 'dir)))



;; (require 'bookmark+)

;; (setq enable-recursive-minibuffers t)
;; (global-set-key (kbd "C-x r b") 'ido-goto-bookmark)

;; (defun ido-goto-bookmark (bookmark)
;;   (interactive
;;    (list (bookmark-completing-read "Jump to bookmark"
;;                                    bookmark-current-bookmark)))
;;   (unless bookmark
;;     (error "No bookmark specified"))
;;   (let ((filename (bookmark-get-filename bookmark)))
;;     (ido-set-current-directory
;;      (if (file-directory-p filename)
;;          filename
;;          (file-name-directory filename)))
;;     (setq ido-exit        'refresh
;;           ido-text-init   ido-text
;;           ido-rotate-temp t)
;;     (exit-minibuffer)))