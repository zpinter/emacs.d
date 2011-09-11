(let ((lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/deft"))

  (let ((build-cmd (concat "cd " update-dir " && git clone git://jblevins.org/git/deft.git deft"
			  " && cd deft"
			  " && rm -rf .git"
			  " && cp -r ./* ../../lisp/")))

	 (message build-cmd)
	 (shell-command build-cmd))
)
