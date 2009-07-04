(require 'open-resource)

(defun dired-open-resource (filepattern)
  (interactive "MFile pattern: ")
  (find-file-in-directory dired-directory filepattern))

(add-hook 'dired-mode-hook
			 '(lambda ()
				 (define-key dired-mode-map "r" 'dired-open-resource)))