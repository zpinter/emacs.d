(require 'erc-auto)

(setq erc-echo-notices-in-minibuffer-flag t)

;; (eval-after-load "erc"
;;   '(progn
;;     (setq erc-auto-query t
;;      erc-bbdb-auto-create-on-whois-p t
;;      erc-fill-column (- (window-width) 2))
;;     (require 'erc-imenu)
;;     (require 'erc-menu)
;;     (require 'erc-notify)
;;     (require 'erc-ring)
;;     (erc-button-mode 1)
;;     (erc-completion-mode 1)
;;     (erc-fill-mode 1)
;;     (erc-match-mode 1)
;;     (erc-netsplit-mode 1)
;;     (erc-services-mode 1)
;;     (erc-timestamp-mode 1)
;;     (erc-track-mode 1)
;;     (add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)
;;     (add-to-list 'erc-nick-popup-alist
;;      '("DebianDB" .
;;        (shell-command
;;         (format
;;          "ldapsearch -x -P 2 -h db.debian.org -b dc=debian,dc=org ircnick=%s"
;;          nick))))))