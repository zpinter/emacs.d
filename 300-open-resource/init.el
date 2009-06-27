(require 'open-resource)

(defun dired-open-resource (filepattern)
  (interactive "MFile pattern: ")
  (find-file-in-directory dired-directory filepattern))

(define-key dired-mode-map "r" 'dired-open-resource)