(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

(setq zconfig-emacsd (file-name-directory load-file-name))

(setq zconfig-errors nil)

(defun write-string-to-file (string file)
   (interactive "sEnter the string: \nFFile to save to: ")
   (with-temp-buffer
     (insert string)
     (when (file-writable-p file)
       (write-region (point-min)
                     (point-max)
                     file))))

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

(defun zconfig-create-module ()
  "Create a folder for a module and set it up with update script"
  (interactive)
  (let ((module-name (read-from-minibuffer "Module Name? "))
		  (module-repo (read-from-minibuffer "Git repo? ")))
	 (zconfig-run-module module-name "create" module-repo)))


(defun zconfig-load-modules (module-names)
  (let ((benchmarks '()))
	 (dolist (element module-names value)
		(setq value nil)
		(setq benchmarks (cons 
								(prin1-to-string
								 (list
								  (benchmark-run 1
										(zconfig-load-module-by-name element))
								  element))
								benchmarks)))
	 
	 (message "Benchmarks results")
	 ;; (message benchmarks)
	 (print (reverse (sort benchmarks 'string<)))
	 )
    
    ;; (zconfig-module-error-wrap (zconfig-load-module-by-name element) element)
  (if zconfig-errors
      (display-warning :error (concat "There were errors loading modules! " (prin1-to-string zconfig-errors)))))

(defun zconfig-add-lisp-path (p)
  (add-to-list 'load-path (expand-file-name (concat zconfig-current-module-dir "/" p)))
  )

(defun zconfig-add-lisp-path-end (p)
  (add-to-list 'load-path (expand-file-name (concat zconfig-current-module-dir "/" p)) t)
  )

(defun zconfig-add-info-path (p)
  (add-to-list 'Info-default-directory-list (expand-file-name (concat zconfig-current-module-dir "/" p)))
  )

(defun zconfig-add-icons-path (p)
  (add-to-list 'image-load-path (expand-file-name (concat zconfig-current-module-dir "/" p)))
  )

(defun zconfig-get-module-dir (module-name)
  (concat zconfig-emacsd module-name))

(defun zconfig-load-module-by-name (module-name)
  (zconfig-run-module module-name "load"))

(defun zconfig-load-module ()
  "Update a module via its update.el"
  (interactive)
  (let ((module-name (read-from-minibuffer "Module? ")))
	 (zconfig-load-module-by-name module-name)))

(defadvice shell-command 
  (before zconfig-update-shell-command (&rest params) disable)
  "Capture the output to a different buffer"
  (ad-set-args 0 (list (car params) "*zconfig-update*" "*zconfig-update*"))
  ;; (ad-set-arg 0 (car params))
  ;; (ad-set-arg 1 '"*scratch*")
  ;; (ad-set-arg 2 '"*scratch*")
  ;; (ad-set-arg 0 "echo foo")
  ;; (ad-set-arg 1 (get-buffer-create "*scratch*"))
  ;; (ad-set-arg 2 (get-buffer-create "*zconfig-update*"))  
  )

(defun zconfig-update-from-git-simple
  (repo-name repo)
  (let ((lisp-dir (concat zconfig-current-module-dir "/lisp"))
		  (update-dir (concat zconfig-current-module-dir "/update")))

	 (shell-command (concat "mkdir -p " lisp-dir))
	 (shell-command (concat "mkdir -p " update-dir))
	 (shell-command (concat "rm -rf " lisp-dir "/*"))
	 (shell-command (concat "rm -rf " update-dir "/" repo-name))

	 (let ((build-cmd (concat "cd " update-dir " && git clone " repo " " repo-name
									  " && cd " repo-name
									  " && rm -rf .git"
									  " && cp -r ./* ../../lisp/")))
		
		(message build-cmd)
		(shell-command build-cmd))))

(defun zconfig-update-module ()
  "Update a module via its update.el"
  (interactive)
  (let ((module-name (read-from-minibuffer "Module? ")))
	 (with-output-to-temp-buffer "*zconfig-update*"
		(ad-enable-advice 'shell-command 'before 'zconfig-update-shell-command)
		(ad-activate 'shell-command)
		(zconfig-run-module module-name "update")
		(ad-disable-advice 'shell-command 'before 'zconfig-update-shell-command)
		(ad-activate 'shell-command)
		)))

(defun zconfig-run-module (module-name operation &optional arg)
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

  (when (equal operation "create")
	 (if (file-directory-p zconfig-current-module-dir)
		  (message "Module already exists!")
		(progn
		  (shell-command (concat "mkdir -p" zconfig-current-module-dir "lisp"))
		  (shell-command (concat "mkdir -p" zconfig-current-module-dir "update"))
		  (shell-command (concat "touch " zconfig-current-module-init-file))
		  (write-string-to-file
			(concat zconfig-current-module-dir "update.el")
			(concat "(zconfig-update-from-git-simple \"" module-name "\" \"" arg "\")"))))))

