(zconfig-update-from-git-simple "js2-mode" "git://github.com/mooz/js2-mode.git")
(let ((lisp-dir (concat zconfig-current-module-dir "/lisp")))
  (shell-command (concat zconfig-emacs-cmd " --batch -f batch-byte-compile " lisp-dir "/js2-mode.el")))
