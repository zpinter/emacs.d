(require 'helm-config)

(global-set-key (kbd "C-c C-h") 'helm-mini)

(eval-after-load "helm"
  '(set-face-background 'helm-selection "Black"))
