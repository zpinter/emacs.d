;; stolen from https://github.com/magit/magit/blob/master/tests/magit-tests.el
(defmacro with-temp-git-repo (repo &rest body)
  (declare (indent 1) (debug t))
  `(let* ((,repo (make-temp-file "tmp_git" t))
          (default-directory (concat ,repo "/")))
     (unwind-protect
         (progn
           (magit-init repo)
           ,@body)
       (delete-directory ,repo t)
       )))

(defun create-file-in-repo (repo dummy-file)
  (with-temp-buffer
    (write-file (format "%s/%s" repo dummy-file))
    (magit-run-git "add" dummy-file)))
