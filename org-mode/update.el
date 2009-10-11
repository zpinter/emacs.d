(let ((info-dir (concat zconfig-current-module-dir "/info"))
		(lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(contrib-dir (concat zconfig-current-module-dir "/contrib"))
		(update-dir (concat zconfig-current-module-dir "/update"))
		(emacs-cmd (if (ismac) "/Applications/Emacs.app/Contents/MacOS/Emacs" "emacs")))

  (shell-command (concat "rm -rf " info-dir "/*"))
  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " contrib-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/org-mode"))

  (let ((build-cmd (concat "cd " update-dir " && git clone git://repo.or.cz/org-mode.git"
			  " && cd org-mode"
			  " && sed -i -e 's,EMACS=emacs,EMACS=" emacs-cmd ",' Makefile"
			  " && sed -i -e 's,lispdir = $(prefix)/share/emacs/site-lisp,lispdir=" lisp-dir ",' Makefile"
			  " && sed -i -e 's,lispdir = $(prefix)/share/emacs/site-lisp,lispdir=" lisp-dir ",' Makefile"
			  " && sed -i -e 's,infodir = $(prefix)/share/info,infodir=" info-dir ",' Makefile"
			  " && make && make doc && make install && make install-info"
			  " && cp -r contrib/* ../../contrib/")))

;; autoconf && ./configure --with-lispdir=" lisp-dir " --with-icondir=" icon-dir
;; 			  " --infodir=" info-dir " --with-emacs=" emacs-cmd " && make && make install && make install-icons")))
	 (message build-cmd)
	 (shell-command build-cmd))

)
