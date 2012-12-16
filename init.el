                                        ; -*- mode: emacs-lisp; -*-
(load "~/.emacs.d/zconfig.el")

(zconfig-load-modules (list
                       "core"
                       "customize"
							  "org-mode"
							  "org-jira"
                       "apel"
                       "elscreen"
                       "easypg"
                       "window-settings"
							  "solarized-theme"
							  ;; "color-theme"
							  ;; "color-theme-zenburn"
                       "ibuffer"
                       "hippie-expand"
                       "nxml"
							  ;; "ruby"
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
                       "deft"
                       "paredit"
                       "slime"
                       "clojure-mode"
                       "cheat"
                       "shell-current-directory"
                       "erc"
                       "haml-mode"
                       "smali"
                       "helm"
                       "helm-git"
                       ;; "anything"
                       ;; "w3m"
                       "flyspell"
                       "rinari"
                       ;; "flex"
                       "nav"
                       "open-resource"
                       "breadcrumb"
                       "xcscope"
                       ;; "company-mode"
                       ;; "cedet"
                       ;; "ecb"
                       ;; "jdee"
                       ;; "textmate"
                       "csharp"
                       "undo-tree"
                       "jabber"
                       "ack"
                       "csv-mode"
                       "scss-mode"
                       "ace-jump-mode"
							  "erlang-mode"
                       ;; "emacs-eclim"
                       "expand-region"
                       "mark-multiple"
                       "smart-tab"
                       "tweaks"
                       "smex"
                       "server"
                       ;; "edit-server"
							  "ace-jump-mode"
							  "erlang-mode"							  
							  ;; "emacs-eclim"
							  "expand-region"
							  "mark-multiple"
							  "smart-tab"
							  "ws-trim"
							  "tweaks"
							  "mu4e"
							  "ido"
                       "smex"
							  ;; "server"
							  ;; "edit-server"
                       ))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
