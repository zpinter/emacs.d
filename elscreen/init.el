
(setq elscreen-display-tab nil)

(load "elscreen"); "ElScreen" t)
(require 'elscreen-server)
(require 'elscreen-speedbar)

;; mac elscreen play nice with carbon emacs
(defun create-new-tab-and-switch-to ()
  (message "New tab")
  (elscreen-create))

(defadvice mac-ae-open-documents (before mac-ae-open-documents-advice activate)
  "Create new tab before" (create-new-tab-and-switch-to))
(ad-activate 'mac-ae-open-documents)