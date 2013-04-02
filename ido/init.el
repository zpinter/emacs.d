(require 'ido)
(ido-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t)      ; fuzzy matching is a must have
(setq ido-enable-tramp-completion t)
(setq tramp-default-method "ssh")

(setq ido-default-file-method 'selected-window)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

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


;; imenu support
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
	 (require 'imenu nil t))
  (cond
	((not symbol-list)
	 (let ((ido-mode ido-mode)
			 (ido-enable-flex-matching
			  (if (boundp 'ido-enable-flex-matching)
					ido-enable-flex-matching t))
			 name-and-pos symbol-names position)
		(unless ido-mode
		  (ido-mode 1)
		  (setq ido-enable-flex-matching t))
		(while (progn
					(imenu--cleanup)
					(setq imenu--index-alist nil)
					(ido-goto-symbol (imenu--make-index-alist))
					(setq selected-symbol
							(ido-completing-read "Symbol? " symbol-names))
					(string= (car imenu--rescan-item) selected-symbol)))
		(unless (and (boundp 'mark-active) mark-active)
		  (push-mark nil t nil))
		(setq position (cdr (assoc selected-symbol name-and-pos)))
		(cond
		 ((overlayp position)
		  (goto-char (overlay-start position)))
		 (t
		  (goto-char position)))))
	((listp symbol-list)
	 (dolist (symbol symbol-list)
		(let (name position)
		  (cond
			((and (listp symbol) (imenu--subalist-p symbol))
			 (ido-goto-symbol symbol))
			((listp symbol)
			 (setq name (car symbol))
			 (setq position (cdr symbol)))
			((stringp symbol)
			 (setq name symbol)
			 (setq position
					 (get-text-property 1 'org-imenu-marker symbol))))
		  (unless (or (null position) (null name)
						  (string= (car imenu--rescan-item) name))
			 (add-to-list 'symbol-names name)
			 (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "C-x C-v") 'ido-goto-symbol)
