;; mirah
(autoload 'mirah-mode "mirah-mode"
  "Mode for editing mirah source files")
(add-to-list 'auto-mode-alist '("\\.mirah$" . mirah-mode))

(require 'mirah-complete-config)
(add-hook 'mirah-mode-hook 'mirah-complete-mode)
