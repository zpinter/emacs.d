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
 '(coffee-tab-width 2)
 '(css-indent-offset 2)
 '(ecb-options-version "2.40")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(expand-region-guess-python-mode nil)
 '(expand-region-preferred-python-mode (quote python))
 '(indent-tabs-mode nil)
 '(jde-global-classpath (quote ("/Users/zpinter/repos/emacs.d/jdee/classes")))
 '(jde-jdk (quote ("1.6.0")))
 '(jde-jdk-registry
   (quote
    (("1.6.0" . "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0"))))
 '(jira-url "https://jira.hulu.com/rpc/xmlrpc")
 '(js-indent-level 2)
 '(nav-quickjump-show t)
 '(ns-antialias-text t)
 '(ns-pop-up-frames nil)
 '(nxml-child-indent 4)
 '(python-indent-guess-indent-offset nil)
 '(smart-tab-disabled-major-modes (quote (org-mode term-mode mu4e-compose-mode)))
 '(sp-autoescape-string-quote nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t :underline "red")))
 '(flymake-warnline ((t :underline "green")))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode ((((class color) (min-colors 88) (background dark)) (:background "#222222"))))
 '(twit-author-face ((t (:height 0.8))))
 '(twit-message-face ((default (:height 1.1)) (nil nil)))
 '(twit-title-face ((((background light)) (:background "PowderBlue" :underline "DeepSkyBlue" :box (:line-width 2 :color "Black" :style 0))) (((background dark)) (:background "Black" :underline "DeepSkyBlue" :box (:line-width 2 :color "Black" :style 0))) (t (:underline "white"))))
 '(twit-zebra-1-face ((((class color) (background light)) (:foreground "black" :background "gray89" :box (:line-width 2 :color "gray89" :style 0))) (((class color) (background dark)) nil) (t (:inverse nil))))
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))

