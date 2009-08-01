(require 'yasnippet-bundle)
(yas/initialize)
(yas/load-directory (concat zconfig-current-module-dir "/snippets"))
(yas/load-directory (concat zconfig-current-module-dir "/rails-snippets"))


;; (if (boundp 'mumamo:version)
;;     ((setq mumamo-map
;;           (let ((map (make-sparse-keymap)))
;;             (define-key map [(control meta prior)] 'mumamo-backward-chunk)
;;             (define-key map [(control meta next)]  'mumamo-forward-chunk)
;;             (define-key map [tab] 'yas/expand)
;;             map))
;;      (mumamo-add-multi-keymap 'mumamo-multi-major-mode mumamo-map)))

