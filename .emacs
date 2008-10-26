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

;;   (print (concat "module is " zconfig-current-module))
;;   (print (concat "module-dir is " zconfig-current-module-dir))
;;   (print (concat "module-init-file is " zconfig-current-module-init-file))

  (if (file-exists-p zconfig-current-module-init-file)
      (load-file zconfig-current-module-init-file))
)

(zconfig-load-all-modules)
