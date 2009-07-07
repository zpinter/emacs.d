(setq custom-file load-file-name)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;; (when
;; 	 (load
;; 	  (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/gnus.txt" "~/org/emacs.txt" "~/org/inbox.txt" "~/org/simplesoon.txt" "~/org/zigimus.txt" "~/org/star.txt" "~/org/natgeo.txt" "~/org/eui.txt" "~/org/personal.txt"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((t :underline "red")))
 '(flymake-warnline ((t :underline "green")))
 '(twit-author-face ((t (:height 0.8))))
 '(twit-message-face ((default (:height 1.1)) (nil nil)))
 '(twit-title-face ((((background light)) (:background "PowderBlue" :underline "DeepSkyBlue" :box (:line-width 2 :color "Black" :style 0))) (((background dark)) (:background "Black" :underline "DeepSkyBlue" :box (:line-width 2 :color "Black" :style 0))) (t (:underline "white"))))
 '(twit-zebra-1-face ((((class color) (background light)) (:foreground "black" :background "gray89" :box (:line-width 2 :color "gray89" :style 0))) (((class color) (background dark)) nil) (t (:inverse nil)))))

