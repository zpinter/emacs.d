;; (eval-after-load "em-term"
;;   '(add-to-list 'eshell-visual-commands "git"))

(eval-after-load "em-term"
  '(add-to-list 'eshell-visual-commands "ssh"))


(require 'vc-git)
(defun fg/eshell-git-info ()
  (let* ((branch (vc-git-working-revision (eshell/pwd))))
    (if (not (string-equal "" branch))
        (concat branch " ")
      "")))


(defun fg/eshell-replace-prompt-prefixes ()
  (let ((absolute-path (eshell/pwd)))
    (cond ((string-match (getenv "HOME") absolute-path)
           (replace-match "~" nil nil absolute-path))
          ((string-match "/ssh:\\(.+\\):" absolute-path)
           (replace-match (concat "@" (match-string 1 absolute-path) " ")  nil nil absolute-path))
          (t
           absolute-path))))

(defun fg/eshell-prompt-function ()
  (concat
   (fg/eshell-git-info)
   (fg/eshell-replace-prompt-prefixes)
   "/ "))

(defun fg/eshell-with-prefix ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'eshell))


(setq eshell-prompt-function #'fg/eshell-prompt-function)
(setq eshell-prompt-regexp "^[^\n]*/ ")

(global-set-key (kbd "C-c e") 'eshell)

(defun eshell/git (&rest args)
     (apply 'eshell-exec-visual (cons "git" args)))
	
(add-hook 'eshell-preoutput-filter-functions
           'ansi-color-filter-apply)

;; ;; Eshell aliases
;; (defun eshell/git-log ()
;;   (magit-log))

;(setq eshell-visual-commands (cons "git" "ssh"))
;;(add-to-list 'eshell-visual-commands "git")
;;(add-to-list 'eshell-visual-commands "ssh")


;; (defun update-eshell-term-directory (buffer-name)
;;   (let ((current-dir (file-truename default-directory)))
;; 	 (save-excursion
;; 		(set-buffer (get-buffer-create buffer-name))
;; 		(message (string (eshell-parse-command (concat "cd \"" current-dir "\"")))))))

;; (defun visit-eshell-term ()
;;   "If we are in an *eshell-term*, rename it.
;; If there is no *eshell-term*, run it.
;; If there is one running, switch to that buffer."
;;   (interactive)
;;   (if (equal "*eshell*" (buffer-name))
;;       (call-interactively 'rename-buffer)
;; 		(if (get-buffer "*eshell*")
;; 			 (progn
;; 				(update-eshell-term-directory "*eshell*")
;; 				(switch-to-buffer "*eshell*"))
;; 		  (eshell))))
;; (global-set-key (kbd "C-c e") 'visit-eshell-term)



(defun ac-pcomplete ()
  ;; eshell uses `insert-and-inherit' to insert a \t if no completion
  ;; can be found, but this must not happen as auto-complete source
  (flet ((insert-and-inherit (&rest args)))
    ;; this code is stolen from `pcomplete' in pcomplete.el
    (let* (tramp-mode ;; do not automatically complete remote stuff
           (pcomplete-stub)
           (pcomplete-show-list t) ;; inhibit patterns like * being deleted
           pcomplete-seen pcomplete-norm-func
           pcomplete-args pcomplete-last pcomplete-index
           (pcomplete-autolist pcomplete-autolist)
           (pcomplete-suffix-list pcomplete-suffix-list)
           (candidates (pcomplete-completions))
           (beg (pcomplete-begin))
           ;; note, buffer text and completion argument may be
           ;; different because the buffer text may bet transformed
           ;; before being completed (e.g. variables like $HOME may be
           ;; expanded)
           (buftext (buffer-substring beg (point)))
           (arg (nth pcomplete-index pcomplete-args)))
      ;; we auto-complete only if the stub is non-empty and matches
      ;; the end of the buffer text
      (when (and (not (zerop (length pcomplete-stub)))
                 (or (string= pcomplete-stub ; Emacs 23
                              (substring buftext
                                         (max 0
                                              (- (length buftext)
                                                 (length pcomplete-stub)))))
                     (string= pcomplete-stub ; Emacs 24
                              (substring arg
                                         (max 0
                                              (- (length arg)
                                                 (length pcomplete-stub)))))))
        ;; Collect all possible completions for the stub. Note that
        ;; `candidates` may be a function, that's why we use
        ;; `all-completions`.
        (let* ((cnds (all-completions pcomplete-stub candidates))
               (bnds (completion-boundaries pcomplete-stub
                                            candidates
                                            nil
                                            ""))
               (skip (- (length pcomplete-stub) (car bnds))))
          ;; We replace the stub at the beginning of each candidate by
          ;; the real buffer content.
          (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                  cnds))))))

(defvar ac-source-pcomplete
  '((candidates . ac-pcomplete)))

(add-hook 'eshell-mode-hook #'(lambda () (setq ac-sources '(ac-source-pcomplete))))
(add-to-list 'ac-modes 'eshell-mode)
