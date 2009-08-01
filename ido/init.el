(require 'ido)
(ido-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t)      ; fuzzy matching is a must have

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(icomplete-mode 1)
(setq icomplete-compute-delay 0)
(require 'icomplete+)

;; (require 'smex)
;; (smex-initialize)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

(defalias 'ido-list-directory 'ido-dired)

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