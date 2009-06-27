(require 'magit)

(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))
(setq exec-path (append exec-path '("/opt/local/bin")))

(defalias 'gs 'magit-status)
(defalias 'gsr 'magit-svn-rebase)
(defalias 'gsd 'magit-svn-rebase)

(defun magit-stage-all-even-untracked ()
  (interactive)
  (magit-run-git "add" "."))

(defalias 'ga. 'magit-stage-all-even-untracked)