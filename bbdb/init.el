(require 'bbdb)

(load "bbdb-com" t)
(bbdb-initialize 'gnus) ;; 'message 'reportmail 'w3)
;; (bbdb-insinuate-reportmail)
;; (bbdb-insinuate-message)
;; (bbdb-insinuate-sc)
;; (bbdb-insinuate-w3)
(setq bbdb-north-american-phone-numbers t)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; (setq bbdb-auto-notes-alist
;;       (quote (("To"
;;                ("w3o" . "w3o")
;;                ("plug" . "plug")
;;                ("linux" . "linux")
;;                ("emacs-commit" . "emacs commit")
;;                ("emacs" . "emacs")
;;                ("sacha" . "personal mail"))
;;               ("From"
;;                ("Organization" (".*" company 0 nil))
;;                ))))
;; (setq bbdb-auto-notes-ignore (quote (("Organization" . "^Gatewayed from\\\\|^Source only"))))
;; (setq bbdb-auto-notes-ignore-all nil)
;; (setq bbdb-check-zip-codes-p nil)
(setq bbdb-default-area-code 303)
(setq bbdb-default-country "USA")
;; (setq bbdb-notice-hook (quote (bbdb-auto-notes-hook)))
(setq bbdb/mail-auto-create-p t)
(setq bbdb/news-auto-create-p nil)