(setq emacsd "~/emacs.d/")

(defun zconfig-load-all-modules ()
  (let (
        (module-directories (directory-files emacsd nil "^[0-9]+\-")))

    (dolist (element module-directories value)
      (setq value nil)
      (zconfig-load-module element)
    ))
)

(defun zconfig-load-module (module-name)
  (setq zconfig-current-module module-name)
  (setq zconfig-current-module-dir (concat emacsd module-name))
  (setq zconfig-current-module-init-file (concat zconfig-current-module-dir "/init.el"))
  (setq zconfig-current-module-private-file (concat zconfig-current-module-dir "/private.el"))  

;;   (print (concat "module is " zconfig-current-module))
;;   (print (concat "module-dir is " zconfig-current-module-dir))
;;   (print (concat "module-init-file is " zconfig-current-module-init-file))

  (if (file-exists-p zconfig-current-module-private-file)
      (load-file zconfig-current-module-private-file))  
  
  (if (file-exists-p zconfig-current-module-init-file)
      (load-file zconfig-current-module-init-file))
)

(zconfig-load-all-modules)


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
 '(org-agenda-files (quote ("~/org/inbox.txt" "~/org/simplesoon.txt" "~/org/zigimus.txt" "~/org/star.txt" "~/org/natgeo.txt" "~/org/eui.txt" "~/org/personal.txt"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((t :underline "red")))
 '(flymake-warnline ((t :underline "green"))))
