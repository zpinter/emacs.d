(setq el-get-user-package-directory "~/.emacs.d/el-get-setup")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")


(setq z-el-get-init-files (directory-files "~/.emacs.d/el-get-setup" nil "^init-.*.el$"))

(setq z-el-get-packages
		(mapcar '(lambda (init-file) (replace-regexp-in-string "init-\\|\\.el" "" init-file)) z-el-get-init-files))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync z-el-get-packages)
