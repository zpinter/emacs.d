;;load the etags-select.el source code
(require 'etags-select)

;;binding the key
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

(require 'etags-table)

(defun etags-table-build-table-list (filename)
  "Build tags table list based on a filename"
  (let (tables)

	 ;; Check git repo
	 (when (file-exists-p (concat (helm-git-git-dir) "TAGS"))
		(setq tables (list (concat (helm-git-git-dir) "TAGS"))))

    ;; Search up
    (when etags-table-search-up-depth
      (let ((depth etags-table-search-up-depth)
            (dir (file-name-directory filename)))
        (while (and (>= depth 0) dir)
          (when (file-exists-p (concat dir "TAGS"))
            (setq tables (list (concat dir "TAGS")))
            (setq depth 0))
          (setq dir (file-name-directory (directory-file-name dir)))
          (setq depth (1- depth)))))

    ;; Go through mapping alist
    (mapc (lambda (mapping)
            (let ((key (car mapping))
                  (tag-files (cdr mapping)))
              (when (string-match key filename)
                (mapc (lambda (tag-file)
                        (add-to-list 'tables (file-truename (replace-match tag-file t nil filename)) t))
                      tag-files))))
          etags-table-alist)

    ;; Return result or the original list
    (setq etags-table-last-table-list
          (or tables tags-table-list etags-table-last-table-list))))

(setq etags-table-search-up-depth 10)
