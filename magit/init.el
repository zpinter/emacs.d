
(require 'magit)

(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))
(setq exec-path (append exec-path '("/opt/local/bin")))

(defalias 'gs 'magit-status)

(defun magit-stage-all-even-untracked ()
  (interactive)
  (magit-run-git "add" "."))

(defalias 'ga. 'magit-stage-all-even-untracked)