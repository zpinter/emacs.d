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

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))



(global-set-key (kbd "C-c C-g") 'magit-status)
