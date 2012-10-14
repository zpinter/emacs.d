(let ((lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/expand-region"))

  (let ((build-cmd (concat "cd " update-dir " && git clone git://github.com/magnars/expand-region.el.git expand-region"
			  " && cd expand-region"
			  " && rm -rf .git"
			  " && cp -r ./* ../../lisp/")))
	 
	 (print build-cmd)
	 (shell-command build-cmd))
)

