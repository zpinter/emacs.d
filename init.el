; -*- mode: emacs-lisp; -*-
(load "~/.emacs.d/zconfig.el")

(zconfig-load-modules (list
                        "core"
								"customize"
								"apel"
								"elscreen"
                        "easypg"
                        "window-settings"
								"color-theme"
								"color-theme-ir-black"
                        "ido"
                        "ibuffer"								
                        "hippie-expand"
                        "nxml"
                        "ruby"
                        "auto-complete"																
                        "mirah"								
                        "js2-mode"
;;                         "yasnippet"
								"magit"
                        "twitter"
                        "post-mode"
                        "gnus"
								"bbdb"
                        "puppet-mode"
                        "markdown-mode"
;;                         "ess"
                        "trac-wiki"
                        "remember"
                        "org-mode"
								;; "paredit"								
								"slime"
                        "clojure"
                        "cheat"
                        "shell-current-directory"
                        "erc"
                        ;; "anything"
                        "w3m"
                        ;; "flyspell"
                        "jump"
                        "rinari"
                        "flex"
                        "nav"
                        "open-resource"
                        "breadcrumb"
;;                         "company-mode"
								"cedet"
;; 								"ecb"
;; 								"jdee"
;;								"textmate"
								"undo-tree"
								"jabber"
								"ack"
								"csv-mode"
                        "tweaks"
								"smex"
                        "server"
                        ))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
