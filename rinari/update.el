(let ((info-dir (concat zconfig-current-module-dir "/info"))
		(lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "mkdir -p " lisp-dir))
  (shell-command (concat "mkdir -p " update-dir))
  (shell-command (concat "mkdir -p " info-dir))

  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " info-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/rinari"))

  (let ((build-cmd (concat "cd " update-dir " && git clone git://github.com/eschulte/rinari.git"
			  " && cd rinari"
			  " && git submodule update --init --recursive"
			  " && find . -name \".git\" | xargs -I {} rm -rf {}"
			  " && cp doc/* ../../info"
			  " && mv ./* ../../lisp")))

	 (message build-cmd)
	 (shell-command build-cmd))

)
