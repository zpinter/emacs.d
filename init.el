                                        ; -*- mode: emacs-lisp; -*-
(load "~/.emacs.d/zconfig.el")

(zconfig-load-modules (list
                       "core"
                       "customize"
                       "apel"
                       "elscreen"
                       "easypg"
                       "window-settings"
							  "solarized-theme"
							  ;; "color-theme"
							  ;; "color-theme-zenburn"
                       "ido"
                       "ibuffer"
                       "hippie-expand"
                       "nxml"
                       "ruby"
                       "auto-complete"
							  "python-mode"
                       ;; "pymacs"							  
                       "mirah"
                       "js2-mode"
                       "android-mode"							  
                       ;; "yasnippet"
							  "git-contrib"
							  "egit"
                       "magit"
                       "mo-git-blame"
                       "twitter"
                       "post-mode"
                       ;; "gnus"
                       "bbdb"
                       "puppet-mode"
                       "markdown-mode"
                       ;; "ess"
							  "lua-mode"
							  "xml-rpc" ;needed by trac-wiki and jira
                       "trac-wiki"
							  "jira"
                       "remember"
                       "org-mode"
                       "deft"
                       ;; "paredit"
                       "slime"
                       "clojure"
                       "cheat"
                       "shell-current-directory"
                       "erc"
                       "haml-mode"
                       "smali"
                       "anything"
                       ;; "w3m"
                       "flyspell"
                       "jump"
                       "rinari"
                       "flex"
                       "nav"
                       "open-resource"
                       "breadcrumb"
							  "xcscope"
                       ;; "company-mode"
                       ;; "cedet"
                       ;; "ecb"
                       ;; "jdee"
                       ;; "textmate"
                       "undo-tree"
                       "jabber"
                       "ack"
                       "csv-mode"
                       "scss-mode"
                       "tweaks"
                       "smex"
                       "server"
							  ;; "edit-server"
                       ))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
