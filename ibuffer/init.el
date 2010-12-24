(require 'ibuffer)

(setq ibuffer-default-sorting-mode 'major-mode)

;; (setq ibuffer-saved-filter-groups
;;   (quote (("default"      
;;             ("Org" ;; all org-related buffers
;;               (mode . org-mode))  
;;             ("Term"
;; 				 (or
;;               (mode . term-mode)
;;               (mode . term-char-mode)))
;;             ("Zinio"
;; 				 (or
;;               (filename . "org/zinio/")				  
;;               (filename . "repos/zinio/")
;;               (filename . "projects/zinio/")))
;;             ("Zebra"
;; 				 (or
;;               (filename . "org/zebra/")				  				  
;;               (filename . "repos/zebra")
;;               (filename . "projects/zebra")))
;;             ("Programming" ;; prog stuff not already in MyProjectX
;;               (or
;;                 (mode . c-mode)
;;                 (mode . perl-mode)
;;                 (mode . python-mode)
;;                 (mode . emacs-lisp-mode)
;;                 (mode . js2-mode)					 
;;                 ;; etc
;;                 )) 
;;             ("Git"   (mode . magit-mode))))))

(global-set-key (kbd "C-x b") 'ibuffer)
(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))
