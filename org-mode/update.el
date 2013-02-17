(let ((info-dir (concat zconfig-current-module-dir "/info"))
		(lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(data-dir (concat zconfig-current-module-dir "/data"))
		(contrib-dir (concat zconfig-current-module-dir "/contrib"))
		(update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "rm -rf " info-dir "/*"))
  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " data-dir "/*"))
  (shell-command (concat "rm -rf " contrib-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/org-mode"))

  (let ((build-cmd (concat "cd " update-dir " && git clone git://repo.or.cz/org-mode.git"
			  " && cd org-mode"
			  " && cp ../../local.mk ."
			  " && sed -i -e 's,__emacspath__," zconfig-emacs-cmd ",' local.mk"
			  " && sed -i -e 's,__lispdir__," lisp-dir ",' local.mk"
			  " && sed -i -e 's,__datadir__," data-dir ",' local.mk"
			  " && sed -i -e 's,__infodir__," info-dir ",' local.mk"
			  " && make all && make install && make install-info"
			  " && cp -r contrib/* ../../contrib/")))

;; autoconf && ./configure --with-lispdir=" lisp-dir " --with-icondir=" icon-dir
;; 			  " --infodir=" info-dir " --with-emacs=" emacs-cmd " && make && make install && make install-icons")))
	 (message build-cmd)
	 (shell-command build-cmd))

)
