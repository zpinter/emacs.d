(add-module-path "magit")
(require 'magit)

(defalias 'gs 'magit-status)

(defun magit-stage-all-even-untracked ()
  (interactive)
  (magit-run-git "add" "."))

(defalias 'ga. 'magit-stage-all-even-untracked)