(fset 'yes-or-no-p 'y-or-n-p)

(setq mac-option-modifier 'meta)

; don't commit (save outside .emacs.d), save silently
(setq abbrev-file-name "~/.abbrev_defs")
(setq save-abbrevs 'silently)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(global-set-key (kbd "C-x C-r") 'bookmark-jump)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key [(meta up)] 'move-line-up)
(global-set-key [(meta down)] 'move-line-down)

(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (forward-char col)))

(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key [(control o)] 'vi-open-next-line)

(defun vi-open-next-line (arg)
  "Move to the next line (like vi) and then opens a line."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defconst use-backup-dir t)

(defvar user-temporary-file-directory
  (concat zconfig-emacsd "tmp/"))

(make-directory user-temporary-file-directory t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory) (,tramp-file-name-regexp nil))
      version-control t        ; Use version numbers for backups
      kept-new-versions 16     ; Number of newest versions to keep
      kept-old-versions 2      ; Number of oldest versions to keep
      delete-old-versions t    ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; (defvar user-temporary-file-directory-semantic (concat user-temporary-file-directory "/semantic-cache"))
;; (make-directory user-temporary-file-directory-semantic t)
;; (setq semanticdb-default-save-directory  user-temporary-file-directory-semantic)

(global-set-key (kbd "<f8>") 'mac-toggle-max-window)

(require 'misc) ;;needed for zap-up-to-char
(global-set-key "\M-z" 'zap-up-to-char)
(global-set-key "\M-Z" 'zap-to-char)

(defalias 'dtw 'delete-trailing-whitespace)
;(setq-default show-trailing-whitespace t)
;(defface trailing-whitespace
;  '((t (:background "pale green")))
;  "Used for tabs and such.")


;; (defvar ph/delete-whitespace-counter 0)
;; (defun ph/delete-whitespace (arg)
;;   (interactive "*p")
;;   (if (eq this-command last-command)
;;       (progn 
;;         (incf ph/delete-whitespace-counter)
;;         (if (= ph/delete-whitespace-counter 1)
;;             (delete-blank-lines)
;;           (join-line arg)))
;;     (setq ph/delete-whitespace-counter 0) 
;;     (just-one-space arg))) 
;; (global-set-key (kbd "M-SPC") 'ph/delete-whitespace)

;(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-faces
 '(flymake-errline ((t :underline "red")))
 '(flymake-warnline ((t :underline "green"))))

;(global-set-key [(control return)] 'set-mark-command) 

(defun reload-file ()
  (interactive)
  (find-file (buffer-file-name))
  )

(defalias 'rlf 'reload-file)

(transient-mark-mode t)

;(setq default-major-mode 'org-mode)

(add-to-list 'auto-mode-alist '("\\.el$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

(setq org-return-follows-link t)
(setq org-tab-follows-link t)
(setq tab-width 3)
(setq default-tab-width 3)

(setq truncate-partial-width-windows nil)
(setq default-truncate-lines nil)
(global-set-key (kbd "<f7>") 'toggle-truncate-lines)

;; If the *scratch* buffer is killed, recreate it automatically
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))

  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)

  ;; Since we killed it, don't let caller do that.
  nil)

(setq org-return-follows-link t)

(eval-after-load "em-term"
  '(add-to-list 'eshell-visual-commands "git"))

(eval-after-load "em-term"
  '(add-to-list 'eshell-visual-commands "ssh"))

;(setq eshell-visual-commands (cons "git" "ssh"))
;;(add-to-list 'eshell-visual-commands "git")
;;(add-to-list 'eshell-visual-commands "ssh")

;;dired tweaks
(defun dired-launch-command ()
  (interactive)
  (dired-do-shell-command
   (case system-type
     (gnu/linux "gnome-open") ;right for gnome (ubuntu), not for other systems
     (darwin "open"))
   nil
   (dired-get-marked-files t current-prefix-arg)))

(add-hook 'dired-mode-hook
			 '(lambda ()
				 (define-key dired-mode-map "o" 'dired-launch-command)
				 (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)				 
				 (define-key dired-mode-map "b" 'shell-current-directory)))

(defalias 'mkdir 'make-directory)

;; make sure dired listings have human sizes
(setq dired-listing-switches "-alh")

;; make it easier to copy between buffers
(setq dired-dwim-target t)

; add the function back to the latest carbon emacs
(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))

(setq ssl-certificate-directory "~/.certs")

;ansi-term
(defun visit-ansi-term ()
  "If we are in an *ansi-term*, rename it.
If there is no *ansi-term*, run it.
If there is one running, switch to that buffer."
  (interactive)
  (if (equal "*ansi-term*" (buffer-name))
      (call-interactively 'rename-buffer)
      (if (get-buffer "*ansi-term*")
   (switch-to-buffer "*ansi-term*")
   (ansi-term "/bin/bash"))))
(global-set-key (kbd "<f2>") 'visit-ansi-term)

;file encoding
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.org\\'" 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;sudo stuff
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer)))))
(global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)

;; remember my place
(setq save-place-file "~/.saveplace")
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

;; move regions up and down
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)


;; fix x copy/paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

; window movement
(global-set-key [s-left] 'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up] 'windmove-up)
(global-set-key [s-down] 'windmove-down)

; make sure to set this back to nil, in case anything is misbehaving
(setq debug-on-error nil)

; save and close buffer
(global-set-key [(control c) (k)]
		'(lambda ()
		   (interactive)
		   (if (y-or-n-p-with-timeout "Do you really want to save and close buffer ? " 4 nil)
				 (progn
					(save-buffer)
					(kill-buffer (buffer-name (current-buffer)))))))
