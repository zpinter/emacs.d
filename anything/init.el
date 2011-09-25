(defvar anything-command-map-prefix-key "C-;")

(require 'anything-config)

(setq recentf-max-saved-items 500)

;; http://www.emacswiki.org/emacs/AnythingSources#toc65

(defun anything-c-sources-git-project-for (pwd)
  (loop for elt in
        '(("Modified files (%s)" . "--modified")
          ("Untracked files (%s)" . "--others --exclude-standard")
          ("All controlled files in this project (%s)" . ""))
        collect
        `((name . ,(format (car elt) pwd))
          (init . (lambda ()
                    (unless (and ,(string= (cdr elt) "") ;update candidate buffer every time except for that of all project files
                                 (anything-candidate-buffer))
                      (with-current-buffer
                          (anything-candidate-buffer 'global)
                        (insert
                         (shell-command-to-string
                          ,(format "git ls-files $(git rev-parse --show-cdup) %s"
                                   (cdr elt))))))))
          (candidates-in-buffer)
          (type . file))))

(defun anything-git-project ()
  (interactive)
  (let* ((pwd (shell-command-to-string "echo -n `pwd`"))
         (sources (anything-c-sources-git-project-for pwd)))
    (anything-other-buffer sources
     (format "*Anything git project in %s*" pwd))))

(define-key global-map (kbd "s-t") 'anything-git-project)

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
