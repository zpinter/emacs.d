(let ((lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/emacs-color-theme-solarized"))

  (let ((build-cmd (concat "cd " update-dir " && git clone https://github.com/sellout/emacs-color-theme-solarized.git emacs-color-theme-solarized"
			  " && cd emacs-color-theme-solarized"
			  " && rm -rf .git"
			  " && cp -r ./* ../../lisp/")))

	 (message build-cmd)
	 (shell-command build-cmd))
)
