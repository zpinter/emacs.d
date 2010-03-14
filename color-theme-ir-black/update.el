(let ((lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "mkdir -p " lisp-dir))
  (shell-command (concat "mkdir -p " update-dir))

  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/rinari"))

  (let ((build-cmd (concat "cd " update-dir " && git clone git://github.com/burke/color-theme-ir-black.git"
			  " && cd color-theme-ir-black"
			  " && rm -rf .git"
			  " && mv ./* ../../lisp")))

	 (message build-cmd)
	 (shell-command build-cmd))
)
