                                        ;(autoload 'company-mode "company" nil t)
(setq company-idle-delay nil)
(require 'company)

(setq company-backends '(company-elisp company-nxml company-css
                              company-dabbrev-code company-keywords
                              company-files company-dabbrev))

(defun smart-tab ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

(global-set-key [(tab)] 'smart-tab)

;; (defun smart-tab ()
;;   "This smart tab is minibuffer compliant: it acts as usual in
;;      the minibuffer. Else, if mark is active, indents region. Else if
;;      point is at the end of a symbol, expands it. Else indents the
;;      current line."
;;   (interactive)
;;   (if (minibufferp)
;;       (unless (minibuffer-complete)
;; 		  (company-complete-common))
;;     (if mark-active
;;         (indent-region (region-beginning)
;;                        (region-end))
;;       (if (looking-at "\\_>")
;;           (company-complete-common)
;;         (indent-for-tab-command)))))

