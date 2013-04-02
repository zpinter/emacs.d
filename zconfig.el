(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(push "/usr/local/bin" exec-path)

(setq zconfig-emacsd (file-name-directory load-file-name))
(setq zconfig-emacs-cmd (concat invocation-directory invocation-name))

(setq zconfig-errors nil)


(defun islinux ()
  (or (eq system-type "gnu/linux") (eq system-type 'gnu/linux)))

(defun ismac ()
  (or (eq system-type "darwin") (eq system-type 'darwin)))

(defun iswindows ()
  (or
   (eq system-type "cygwin")
   (eq system-type 'cygwin)
   (eq system-type "windows-nt")
   (eq system-type 'windows-nt)))

(defmacro safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defun write-string-to-file (string file)
  (interactive "sEnter the string: \nFFile to save to: ")
  (with-temp-buffer
    (insert string)
    (when (file-writable-p file)
      (write-region (point-min)
                    (point-max)
                    file))))

(defun zconfig-create-module ()
  "Create a folder for a module and set it up with update script"
  (interactive)
  (let ((module-name (read-from-minibuffer "Module Name? "))
        (module-repo (read-from-minibuffer "Git repo? ")))
	 (zconfig-run-module module-name "create" module-repo)
	 (if (y-or-n-p "Update module now?")
		  (zconfig-update-module-internal module-name))))

(setq zconfig-benchmarks 'undefined)
(setq zconfig-errors '())

(defun zconfig-start-benchmark ()
  (setq zconfig-benchmarks '())
  )

(defun zconfig-benchmark-sort (x1 x2)
  (< (car (car (cdr x2))) (car (car (cdr x1)))))

(defun zconfig-finish-benchmark ()
	 (message "Benchmarks results")

	 (loop for x in (sort zconfig-benchmarks 'zconfig-benchmark-sort) do
		(message (concat "Benchmark " (car x) " in " (prin1-to-string (car (cdr x))))))
)

(defun zconfig-load (&optional module-name)
  "Load/update a module via its init.el"
  (interactive)
  (if module-name
		(zconfig-load-internal module-name)
	 (zconfig-load-internal (read-from-minibuffer "Module? "))))

(defun zconfig-protect (body-fn)
  (unwind-protect
		(condition-case e
			 (funcall body-fn)
		  (error (add-to-list 'zconfig-errors e)))))

(defun zconfig-load-internal (module-to-load)
  (let ((debug-on-error t))
	 (if (eq zconfig-benchmarks 'undefined)
		  (let ((debug-on-error t))
			 (zconfig-run-module module-to-load "load"))
		(zconfig-load-module-by-name-with-benchmark module-to-load))))

(defun zconfig-load-module-by-name-with-benchmark (module-name)
	(add-to-list 'zconfig-benchmarks
					  (list
						module-name
						(benchmark-run 1
						  (unwind-protect
								(condition-case e
									 (zconfig-run-module module-name "load")
								  (error (add-to-list 'zconfig-errors (list e (backtrace))))))))))


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
  (repo-name repo &optional copy-whole-dir)
  (let ((lisp-dir (concat zconfig-current-module-dir "/lisp"))
		  (update-dir (concat zconfig-current-module-dir "/update")))

    (shell-command (concat "mkdir -p " lisp-dir))
    (shell-command (concat "mkdir -p " update-dir))
    (shell-command (concat "rm -rf " lisp-dir "/*"))
    (shell-command (concat "rm -rf " update-dir "/" repo-name))

	 (let ((build-cmd (concat "cd " update-dir " && git clone --depth 1 --recursive " repo " " repo-name
                             " && cd " repo-name
									  " && rm -rf ./**/.git"
									  (if copy-whole-dir (concat " && cp -r ../" repo-name " ../../lisp/") " && cp -r ./* ../../lisp/")
										 )))

      (message build-cmd)
		(shell-command build-cmd))))

(defun zconfig-update-module ()
  "Update a module via its update.el"
  (interactive)
  (let ((module-name (read-from-minibuffer "Module? ")))
	 (zconfig-update-module-internal module-name)))

(defun zconfig-update-module-internal (module-name)
  (with-output-to-temp-buffer "*zconfig-update*"
	 (ad-enable-advice 'shell-command 'before 'zconfig-update-shell-command)
	 (ad-activate 'shell-command)
	 (zconfig-run-module module-name "update")
	 (ad-disable-advice 'shell-command 'before 'zconfig-update-shell-command)
	 (ad-activate 'shell-command)
	 (princ (concat "\nModule " module-name " updated!"))
	 ))

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

  (if (file-exists-p (concat zconfig-current-module-dir "/lisp/tex"))
      (zconfig-add-lisp-path "lisp/tex"))

  (if (file-exists-p (concat zconfig-current-module-dir "/info"))
      (zconfig-add-info-path "info"))

  (if (file-exists-p (concat zconfig-current-module-dir "/lisp/info"))
      (zconfig-add-info-path "lisp/info"))

  (if (file-exists-p (concat zconfig-current-module-dir "/icons"))
      (zconfig-add-icons-path "icons"))

  (if (file-exists-p (concat zconfig-current-module-dir "/lisp/icons"))
      (zconfig-add-icons-path "lisp/icons"))

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
        (shell-command (concat "mkdir -p " zconfig-current-module-dir "/lisp"))
        (shell-command (concat "mkdir -p " zconfig-current-module-dir "/update"))
        (shell-command (concat "touch " zconfig-current-module-init-file))
        (write-string-to-file
         (concat "(zconfig-update-from-git-simple \"" module-name "\" \"" arg "\")")
         (concat zconfig-current-module-update-file))))))
