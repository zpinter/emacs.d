;;; 50magit.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (magit-status) "../../../../../../.emacs.d/magit/update/magit/magit"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/magit.el"
;;;;;;  (20761 15987))
;;; Generated autoloads from ../../../../../../.emacs.d/magit/update/magit/magit.el

(autoload 'magit-status "../../../../../../.emacs.d/magit/update/magit/magit" "\
Open a Magit status buffer for the Git repository containing DIR.
If DIR is not within a Git repository, offer to create a Git
repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git
control.  Two prefix arguments means to ignore `magit-repo-dirs'
when asking for user input.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (magit-blame-mode) "../../../../../../.emacs.d/magit/update/magit/magit-blame"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/magit-blame.el"
;;;;;;  (20761 15987))
;;; Generated autoloads from ../../../../../../.emacs.d/magit/update/magit/magit-blame.el

(autoload 'magit-blame-mode "../../../../../../.emacs.d/magit/update/magit/magit-blame" "\
Display blame information inline.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (turn-on-magit-flow magit-flow-mode) "../../../../../../.emacs.d/magit/update/magit/magit-flow"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/magit-flow.el"
;;;;;;  (20761 15987))
;;; Generated autoloads from ../../../../../../.emacs.d/magit/update/magit/magit-flow.el

(autoload 'magit-flow-mode "../../../../../../.emacs.d/magit/update/magit/magit-flow" "\
FLOW support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-flow "../../../../../../.emacs.d/magit/update/magit/magit-flow" "\
Unconditionally turn on `magit-flow-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-stgit magit-stgit-mode) "../../../../../../.emacs.d/magit/update/magit/magit-stgit"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/magit-stgit.el"
;;;;;;  (20761 15987))
;;; Generated autoloads from ../../../../../../.emacs.d/magit/update/magit/magit-stgit.el

(autoload 'magit-stgit-mode "../../../../../../.emacs.d/magit/update/magit/magit-stgit" "\
StGit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-stgit "../../../../../../.emacs.d/magit/update/magit/magit-stgit" "\
Unconditionally turn on `magit-stgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-svn magit-svn-mode) "../../../../../../.emacs.d/magit/update/magit/magit-svn"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/magit-svn.el"
;;;;;;  (20761 15987))
;;; Generated autoloads from ../../../../../../.emacs.d/magit/update/magit/magit-svn.el

(autoload 'magit-svn-mode "../../../../../../.emacs.d/magit/update/magit/magit-svn" "\
SVN support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-svn "../../../../../../.emacs.d/magit/update/magit/magit-svn" "\
Unconditionally turn on `magit-svn-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (turn-on-magit-topgit magit-topgit-mode) "../../../../../../.emacs.d/magit/update/magit/magit-topgit"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/magit-topgit.el"
;;;;;;  (20761 15987))
;;; Generated autoloads from ../../../../../../.emacs.d/magit/update/magit/magit-topgit.el

(autoload 'magit-topgit-mode "../../../../../../.emacs.d/magit/update/magit/magit-topgit" "\
Topgit support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-topgit "../../../../../../.emacs.d/magit/update/magit/magit-topgit" "\
Unconditionally turn on `magit-topgit-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (global-magit-wip-save-mode magit-wip-save-mode
;;;;;;  magit-wip-mode) "../../../../../../.emacs.d/magit/update/magit/magit-wip"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/magit-wip.el"
;;;;;;  (20761 15987))
;;; Generated autoloads from ../../../../../../.emacs.d/magit/update/magit/magit-wip.el

(defvar magit-wip-mode nil "\
Non-nil if Magit-Wip mode is enabled.
See the command `magit-wip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.")

(custom-autoload 'magit-wip-mode "../../../../../../.emacs.d/magit/update/magit/magit-wip" nil)

(autoload 'magit-wip-mode "../../../../../../.emacs.d/magit/update/magit/magit-wip" "\
In Magit log buffers; give wip refs a special appearance.

\(fn &optional ARG)" t nil)

(autoload 'magit-wip-save-mode "../../../../../../.emacs.d/magit/update/magit/magit-wip" "\
Magit support for committing to a work-in-progress ref.

When this minor mode is turned on and a file is saved inside a writable
git repository then it is also committed to a special work-in-progress
ref.

\(fn &optional ARG)" t nil)

(defvar global-magit-wip-save-mode nil "\
Non-nil if Global-Magit-Wip-Save mode is enabled.
See the command `global-magit-wip-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-wip-save-mode'.")

(custom-autoload 'global-magit-wip-save-mode "../../../../../../.emacs.d/magit/update/magit/magit-wip" nil)

(autoload 'global-magit-wip-save-mode "../../../../../../.emacs.d/magit/update/magit/magit-wip" "\
Toggle Magit-Wip-Save mode in all buffers.
With prefix ARG, enable Global-Magit-Wip-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-Save mode is enabled in all buffers where
`turn-on-magit-wip-save' would do it.
See `magit-wip-save-mode' for more information on Magit-Wip-Save mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (rebase-mode) "../../../../../../.emacs.d/magit/update/magit/rebase-mode"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/rebase-mode.el"
;;;;;;  (20761 15987))
;;; Generated autoloads from ../../../../../../.emacs.d/magit/update/magit/rebase-mode.el

(autoload 'rebase-mode "../../../../../../.emacs.d/magit/update/magit/rebase-mode" "\
Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("git-rebase-todo" . rebase-mode))

;;;***

;;;### (autoloads nil nil ("../../../../../../.emacs.d/magit/update/magit/magit-bisect.el"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/magit-key-mode.el"
;;;;;;  "../../../../../../.emacs.d/magit/update/magit/magit-pkg.el")
;;;;;;  (20761 15988 710064))

;;;***

(provide '50magit)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; 50magit.el ends here
