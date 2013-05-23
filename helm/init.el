(require 'helm-config)

(global-set-key (kbd "C-c C-h") 'helm-mini)
(global-set-key (kbd "C-c d") 'helm-google-suggest)

(eval-after-load "helm"
  '(set-face-background 'helm-selection "Black"))
