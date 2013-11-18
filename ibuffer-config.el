(setq ibuffer-default-sorting-mode 'major-mode)

(global-set-key (kbd "C-x b") 'ibuffer)
(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))
