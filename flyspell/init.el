(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; (dolist (hook '(text-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(setq flyspell-issue-message-flag nil)