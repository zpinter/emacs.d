(setq zconfig-emacsd (file-name-directory load-file-name))

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

;; old way of loading modules
;; (defun zconfig-load-all-modules ()
;;   (let (
;;         (module-directories (directory-files zconfig-emacsd nil "^[0-9]+\-")))

;;     (dolist (element module-directories value)
;;       (setq value nil)
;;       (zconfig-module-error-wrap (zconfig-load-module element) element)
;;       ))
;;   (if zconfig-errors
;;       (display-warning :error (concat "There were errors loading modules! " (prin1-to-string zconfig-errors))))
;;   )

(defun zconfig-load-modules (module-names)
  (dolist (element module-names value)
    (setq value nil)
    (zconfig-module-error-wrap (zconfig-load-module element) element)
    )
  (if zconfig-errors
      (display-warning :error (concat "There were errors loading modules! " (prin1-to-string zconfig-errors)))))

(defun zconfig-add-lisp-path (p)
  (add-to-list 'load-path (expand-file-name (concat zconfig-current-module-dir "/" p)))
  )

(defun zconfig-add-info-path (p)
  (add-to-list 'Info-default-directory-list (expand-file-name (concat zconfig-current-module-dir "/" p)))
  )

(defun zconfig-add-icons-path (p)
  (add-to-list 'image-load-path (expand-file-name (concat zconfig-current-module-dir "/" p)))
  )

(defun zconfig-get-module-dir (module-name)
  (concat zconfig-emacsd module-name))

(defun zconfig-load-module (module-name)
  (zconfig-run-module module-name "load"))

(defun zconfig-update-module (module-name)
  (zconfig-run-module module-name "update"))

(defun zconfig-run-module (module-name operation)
  (setq zconfig-current-module module-name)
  (setq zconfig-current-module-dir (concat zconfig-emacsd module-name))
  (setq zconfig-current-module-init-file (concat zconfig-current-module-dir "/init.el"))
  (setq zconfig-current-module-private-file (concat zconfig-current-module-dir "/private.el"))
  (setq zconfig-current-module-update-file (concat zconfig-current-module-dir "/update.el"))

  ;;   (print (concat "module is " zconfig-current-module))
  ;;   (print (concat "module-dir is " zconfig-current-module-dir))
  ;;   (print (concat "module-init-file is " zconfig-current-module-init-file))

  (if (file-exists-p (concat zconfig-current-module-dir "/lisp"))
      (zconfig-add-lisp-path "lisp"))

  (if (file-exists-p (concat zconfig-current-module-dir "/tex"))
      (zconfig-add-lisp-path "tex"))

  (if (file-exists-p (concat zconfig-current-module-dir "/info"))
      (zconfig-add-info-path "info"))

  (if (file-exists-p (concat zconfig-current-module-dir "/icons"))
      (zconfig-add-icons-path "icons"))

  (when (equal operation "load")
    (if (file-exists-p zconfig-current-module-private-file)
        (load-file zconfig-current-module-private-file))

    (if (file-exists-p zconfig-current-module-init-file)
        (load-file zconfig-current-module-init-file)))

  (when (equal operation "update")
    (if (file-exists-p zconfig-current-module-update-file)
        (load-file zconfig-current-module-update-file)))

)

