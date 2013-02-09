(let ((lisp-dir (concat zconfig-current-module-dir "/lisp"))
      (update-dir (concat zconfig-current-module-dir "/update")))

  (shell-command (concat "mkdir -p " lisp-dir))
  (shell-command (concat "mkdir -p " update-dir))
  (shell-command (concat "rm -rf " lisp-dir "/*"))
  (shell-command (concat "rm -rf " update-dir "/confluence-el"))

  (let ((build-cmd (concat "cd " update-dir " && svn export http://confluence-el.googlecode.com/svn/trunk confluence-el"
									" && cd confluence-el"
									" && cp -r ./* ../../lisp/")))

    (message build-cmd)
    (shell-command build-cmd)))
