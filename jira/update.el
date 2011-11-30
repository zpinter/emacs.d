(let ((lisp-dir (concat zconfig-current-module-dir "/lisp")))
  (shell-command (concat "rm " lisp-dir " jira.el"))

  (let ((build-cmd (concat "cd " lisp-dir " && curl -O http://www.emacswiki.org/emacs/download/jira.el")))
	 (message build-cmd)
	 (shell-command build-cmd))
)

