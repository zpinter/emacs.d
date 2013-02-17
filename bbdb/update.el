(let ((info-dir (concat zconfig-current-module-dir "/info"))
		(lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(icon-dir (concat zconfig-current-module-dir "/icons"))
		(tex-dir (concat zconfig-current-module-dir "/tex"))
		(update-dir (concat zconfig-current-module-dir "/update"))
		(gnus-dir (concat (zconfig-get-module-dir "gnus") "/gnus-cvs")))

  (shell-command (concat "rm -rf " info-dir "/*"))
  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " icon-dir "/*"))
  (shell-command (concat "rm -rf " tex-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/bbdb"))

  (shell-command (concat update-dir "/cvslogin.sh"))

  (let ((build-cmd (concat "cd " update-dir " && cvs -d :pserver:anonymous@bbdb.cvs.sourceforge.net:/cvsroot/bbdb co bbdb"
			  " && cd bbdb && autoconf && ./configure "
			  " --infodir=" info-dir " --with-emacs=" zconfig-emacs-cmd " --with-gnus-dir=" gnus-dir
			  " && make autoloads && make "
			  " && cp -r lisp/*.el " lisp-dir "/ && cp -r lisp/*.elc " lisp-dir "/"
			  " && cp -r tex/*.tex " tex-dir "/ && cp -r texinfo/*.info " info-dir "/")))
	 (message build-cmd)
	 (shell-command build-cmd))

)
