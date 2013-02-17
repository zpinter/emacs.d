(let ((info-dir (concat zconfig-current-module-dir "/info"))
		(lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(icon-dir (concat zconfig-current-module-dir "/icons"))
		(update-dir (concat zconfig-current-module-dir "/update")))



  (shell-command (concat "rm -rf " info-dir "/*"))
  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " icon-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/emacs-w3m"))


  (shell-command (concat update-dir "/cvslogin.sh"))

  (let ((build-cmd (concat "cd " update-dir " && cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m"
			  " && cd emacs-w3m && autoconf && ./configure --with-lispdir=" lisp-dir " --with-icondir=" icon-dir
			  " --infodir=" info-dir " --with-emacs=" zconfig-emacs-cmd " && make && make install && make install-icons")))
	 (message build-cmd)
	 (shell-command build-cmd))

)
