(require 'cask "~/.cask/cask.el")

(setq zconfig-emacsd (file-name-directory load-file-name))
(setq zconfig-emacs-cmd (concat invocation-directory invocation-name))
(message (concat "zconfig-emacsd is " zconfig-emacsd))
(message (concat "zconfig-emacs-cmd is " zconfig-emacs-cmd))

(defadvice cask-add-dependency
  (after cask-add-dependency-message (name &optional version scope) activate)    
  (message (concat "cask-add-dependency for " name))
  (zconfig-load name))

(defun zconfig-load (name)
  (let ((zconfig-config-file (concat zconfig-emacsd name "-config.el")))


    (if (file-exists-p zconfig-config-file)
        (progn
          (message (concat "loading " zconfig-config-file "..."))
          (load-file zconfig-config-file)))))

(zconfig-load "pre-cask")
(zconfig-load "customize")
(cask-initialize zconfig-emacsd)
(zconfig-load "ruby-minimal")
(zconfig-load "post-cask")

