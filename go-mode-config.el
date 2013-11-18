(if (file-exists-p "/usr/local/bin/go")
    (progn
      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/go"))
      (add-to-list 'exec-path "/usr/local/bin/go" t)))

(if (file-exists-p "/usr/local/go/bin")
    (progn
      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/go/bin"))
      (add-to-list 'exec-path "/usr/local/go/bin" t)))

(let ((gopath (concat (getenv "HOME") "/gocode/bin")))
  (if (file-exists-p gopath)
    (progn
      (setenv "PATH" (concat (getenv "PATH") ":" gopath))
      (add-to-list 'exec-path (concat (getenv "PATH") ":" gopath) t))))
