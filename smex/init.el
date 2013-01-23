(require 'smex)
(smex-initialize)

(defadvice zconfig-load-module (after zconfig-load-module-advice activate)
  "Advise zconfig-load-module-by-name to refresh smex"
  (smex-rebuild-cache))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
