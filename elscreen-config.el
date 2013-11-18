(defadvice elscreen-e21-tab-update (before elscreen-e21-tab-update-advice activate)
  "Only show elscreen tabs if more than 1 tab active"
  (if (> (length (elscreen-get-screen-list)) 1)
		(setq elscreen-display-tab t)
	 (setq elscreen-display-tab nil)))

(elscreen-start)
