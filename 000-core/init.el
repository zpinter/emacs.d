;;core functions

(defun add-path (p)
  (add-to-list 'load-path
               (expand-file-name (concat emacsd p))))

(add-to-list 'auto-mode-alist
             '("\\.emacs\\-[a-z\\-]" . emacs-lisp-mode))

(defun add-module-path (p)
  (add-to-list 'load-path (expand-file-name (concat zconfig-current-module-dir "/" p)))
)