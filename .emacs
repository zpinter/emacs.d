(setq emacsd "~/emacs.d/")

(setq zconfig-errors nil)

(defmacro zconfig-module-error-wrap (fn module-name)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception in %s: [%s]" ,module-name ex))
				(add-to-list 'zconfig-errors (list ,module-name ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     nil))

(defun zconfig-load-all-modules ()
  (let (
        (module-directories (directory-files emacsd nil "^[0-9]+\-")))

    (dolist (element module-directories value)
      (setq value nil)
		(zconfig-module-error-wrap (zconfig-load-module element) element)
    ))
  (if zconfig-errors
		(display-warning :error (concat "There were errors loading modules! " (prin1-to-string zconfig-errors))))
)

(defun zconfig-add-lisp-path (p)
  (add-to-list 'load-path (expand-file-name (concat zconfig-current-module-dir "/" p)))
)

(defun zconfig-add-info-path (p)
  (add-to-list 'Info-default-directory-list (expand-file-name (concat zconfig-current-module-dir "/" p)))
)

(defun zconfig-add-icons-path (p)
  (add-to-list 'image-load-path (expand-file-name (concat zconfig-current-module-dir "/" p)))
)

(defun zconfig-load-module (module-name)
  (setq zconfig-current-module module-name)
  (setq zconfig-current-module-dir (concat emacsd module-name))
  (setq zconfig-current-module-init-file (concat zconfig-current-module-dir "/init.el"))
  (setq zconfig-current-module-private-file (concat zconfig-current-module-dir "/private.el"))

;;   (print (concat "module is " zconfig-current-module))
;;   (print (concat "module-dir is " zconfig-current-module-dir))
;;   (print (concat "module-init-file is " zconfig-current-module-init-file))

  (if (file-exists-p (concat zconfig-current-module-dir "/lisp"))
		(zconfig-add-lisp-path "lisp"))

  (if (file-exists-p (concat zconfig-current-module-dir "/info"))
		(zconfig-add-info-path "info"))

  (if (file-exists-p (concat zconfig-current-module-dir "/icons"))
		(zconfig-add-icons-path "icons"))

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
 '(org-agenda-files (quote ("~/org/gnus.txt" "~/org/emacs.txt" "~/org/inbox.txt" "~/org/simplesoon.txt" "~/org/zigimus.txt" "~/org/star.txt" "~/org/natgeo.txt" "~/org/eui.txt" "~/org/personal.txt"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(flymake-errline ((t :underline "red")))
 '(flymake-warnline ((t :underline "green"))))
