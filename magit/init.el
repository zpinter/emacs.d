(autoload 'magit-status "magit" nil t)

(if (ismac)
	 (progn
		(setenv "PATH" (concat (getenv "PATH") ":~/homebrew/bin"))
		(setq exec-path (append exec-path '("~/homebrew/bin")))))

(defalias 'gs 'magit-status)

(defun magit-stage-all-even-untracked ()
  (interactive)
  (magit-run-git "add" "."))

(defalias 'ga. 'magit-stage-all-even-untracked)

(global-set-key (kbd "C-c C-g") 'magit-status)
