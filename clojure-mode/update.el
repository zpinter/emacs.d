(let ((lisp-dir (concat zconfig-current-module-dir "/lisp"))
		(update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/clojure-mode"))

  (let ((build-cmd (concat "cd " update-dir " && git clone https://github.com/technomancy/clojure-mode.git clojure-mode"
			  " && cd clojure-mode"
			  " && rm -rf .git"
			  " && cp -r ./* ../../lisp/")))

	 (message build-cmd)
	 (shell-command build-cmd))
)
