(defun z:find-parent-dir (dir-to-find)
  (progn
      (defun find-parent-dir-r (dir-to-find path)
         "find the dir from the parent directories"
         (let* ((parent (file-name-directory path))
                (possible-dir (concat parent dir-to-find)))
           (cond
             ((file-exists-p possible-dir) (throw 'found-it possible-dir))
             ((string= (concat "/" dir-to-find) possible-dir) nil)
             (t (find-parent-dir-r dir-to-find (directory-file-name parent))))))

    (if (buffer-file-name)
        (catch 'found-it
          (find-parent-dir-r dir-to-find (buffer-file-name)))
        nil)))

(defun z:jedi:setup ()
  (let ((venv-dir (z:find-parent-dir "venv")))
    (if venv-dir
        (setq jedi:server-args `("--virtual-env" ,venv-dir "--sys-path" ,venv-dir)))
    )
  (jedi:setup)
)

(add-hook 'python-mode-hook 'z:jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
