(add-module-path "magit")
(require 'magit)

(defalias 'gs 'magit-status)
(defalias 'gsr 'magit-svn-rebase)
(defalias 'gsd 'magit-svn-rebase)

(defun magit-stage-all-even-untracked ()
  (interactive)
  (magit-run-git "add" "."))

(defalias 'ga. 'magit-stage-all-even-untracked)