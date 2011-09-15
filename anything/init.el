(require 'anything-config)

(setq recentf-max-saved-items 500)

;; http://www.emacswiki.org/emacs/AnythingSources#toc65
(defvar anything-c-source-git-project-files-cache nil "(path signature cached-buffer)")
    (defvar anything-c-source-git-project-files
      '((name . "Files from Current GIT Project")
	(init . (lambda ()
		  (let* ((top-dir (file-truename (magit-get-top-dir (if (buffer-file-name)
									(file-name-directory (buffer-file-name))
								      default-directory))))
			 (default-directory top-dir)
			 (signature (magit-shell (magit-format-git-command "rev-parse --verify HEAD" nil))))

		    (unless (and anything-c-source-git-project-files-cache
				 (third anything-c-source-git-project-files-cache)
				 (equal (first anything-c-source-git-project-files-cache) top-dir)
				 (equal (second anything-c-source-git-project-files-cache) signature))
		      (if (third anything-c-source-git-project-files-cache)
			  (kill-buffer (third anything-c-source-git-project-files-cache)))
		      (setq anything-c-source-git-project-files-cache
			    (list top-dir
				  signature
				  (anything-candidate-buffer 'global)))
		      (with-current-buffer (third anything-c-source-git-project-files-cache)
			(dolist (filename (mapcar (lambda (file) (concat default-directory file))
						  (magit-shell-lines (magit-format-git-command "ls-files" nil))))
			  (insert filename)
			  (newline))))
		    (anything-candidate-buffer (third anything-c-source-git-project-files-cache)))))

        (type . file)
	(candidates-in-buffer)))


(global-set-key [(super t)]
  (lambda() (interactive)
    (anything
     :prompt "Open file: "
     :candidate-number-limit 15                 ;; up to 10 of each 
     :sources
     '( 
        ;; anything-c-source-files-in-current-dir+ ;; current dir
        anything-c-source-git-project-files-cache))))


;; (setq anything-sources
;;       '(
;; 		  anything-c-source-elscreen
;; 		  anything-c-source-buffers
;;         anything-c-source-buffer-not-found
;;         anything-c-source-file-name-history
;; ;;         anything-c-source-recentf
;; 		  anything-c-source-cheat
;; ;;         anything-c-source-man-pages
;; ;;         anything-c-source-locate
;;         anything-c-source-bookmarks
;;         anything-c-source-org-headline
;; 		  anything-c-source-imenu
;; ;; 		  anything-c-source-google-suggest
;; 		  anything-c-source-calculation-result
;; ;; 		  anything-c-source-file-search
;; ;; 		  anything-c-source-complex-command-history
;;         ))

;; ;; elscreen
;; (defvar anything-c-source-elscreen
;;   '((name . "Elscreen")
;;     (candidates . (lambda ()
;;                     (if (cdr (elscreen-get-screen-to-name-alist))
;;                         (sort
;;                          (loop for sname in (elscreen-get-screen-to-name-alist)
;;                                append (list (format "[%d] %s" (car sname) (cdr sname))) into lst
;;                                finally (return lst))
;;                          '(lambda (a b) (compare-strings a nil nil b nil nil))))))
;;     (action . (("Change Screen".
;;                 (lambda (candidate)
;;                   (elscreen-goto (- (aref candidate 1) (aref "0" 0)))))
;;                ("Kill Screen".
;;                 (lambda (candidate)
;;                   (elscreen-kill (- (aref candidate 1) (aref "0" 0)))))
;;                ("Only Screen".
;;                 (lambda (candidate)
;;                   (elscreen-goto (- (aref candidate 1) (aref "0" 0)))
;;                   (elscreen-kill-others)))))))
;; ;; (anything 'anything-c-source-elscreen)

;; (defvar anything-c-source-cheat
;;   '((name . "Cheat Sheets")
;;     (init . (lambda ()
;;               (unless (anything-candidate-buffer)
;;                 (with-current-buffer (anything-candidate-buffer 'global)
;;                   (call-process-shell-command
;;                    "cheat sheets" nil  (current-buffer))
;;                   (goto-char (point-min))
;;                   (forward-line 1)
;;                   (delete-region (point-min) (point))
;;                   (indent-region (point) (point-max) -2)))))
;;     (candidates-in-buffer)
;;     (action . (lambda (entry)
;;                 (let ((buf (format "*cheat sheet:%s*" entry)))
;;                   (unless (get-buffer buf)
;;                     (call-process "cheat" nil (get-buffer-create buf) t entry))
;;                   (display-buffer buf)
;;                   (set-window-start (get-buffer-window buf) 1))))))

;; (defvar anything-c-source-file-search
;;   '((name . "File Search")
;;     (init . (lambda ()
;;               (setq anything-default-directory
;;                     default-directory)))
;;     (candidates . (lambda ()
;;                     (let ((args
;;                            (format "'%s' \\( -path \\*/.svn \\) -prune -o -iregex '.*%s.*' -print"
;;                                    anything-default-directory
;;                                    anything-pattern)))
;;                     (start-process-shell-command "file-search-process" nil
;;                                    "find" args))))
;;     (type . file)
;;     (requires-pattern . 4)
;;     (delayed))
;;   "Source for searching matching files recursively.")


;; (global-set-key (kbd "C-z C-x") 'anything)
