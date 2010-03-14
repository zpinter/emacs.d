(let ((info-dir (concat zconfig-current-module-dir "/info"))
		(lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "rm -rf " info-dir "/*"))
  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/magit"))

  (let ((build-cmd (concat "cd " update-dir " && git clone git://gitorious.org/magit/mainline.git magit"
			  " && cd magit"
			  " && autoconf"
			  " && ./configure"
			  " && make"
			  " && rm -rf .git"
			  " && cp -r ./* ../../lisp/"
			  " && cp -r contrib/* ../../contrib/")))

	 (message build-cmd)
	 (shell-command build-cmd))

)
