(let ((lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/helm-git"))

  (let ((build-cmd (concat "cd " update-dir " && git clone git://github.com/maio/helm-git.git"
			  " && cd helm-git"
			  " && rm -rf .git"
			  " && cp -r ./* ../../lisp/")))
	 
	 (message build-cmd)
	 (shell-command build-cmd))
)
