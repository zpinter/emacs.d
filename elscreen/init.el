;; (setq elscreen-display-tab t)

;; (load "elscreen"); "ElScreen" t)
;; (require 'elscreen-server)
;; (require 'elscreen-speedbar)

;; ;; mac elscreen play nice with carbon emacs
;; (defun create-new-tab-and-switch-to ()
;;   (message "New tab")
;;   (elscreen-create))

;; (defadvice mac-ae-open-documents (before mac-ae-open-documents-advice activate)
;;   "Create new tab before" (create-new-tab-and-switch-to))
;; (ad-activate 'mac-ae-open-documents)


(require 'elscreen)

(defadvice elscreen-e21-tab-update (before elscreen-e21-tab-update-advice activate)
  "Only show elscreen tabs if more than 1 tab active"
  (if (> (length (elscreen-get-screen-list)) 1)
		(setq elscreen-display-tab t)
	 (setq elscreen-display-tab nil)))

(elscreen-start)
