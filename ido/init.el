(require 'ido)
(ido-mode t)

(setq ido-enable-flex-matching t)      ; fuzzy matching is a must have

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(require 'smex)

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