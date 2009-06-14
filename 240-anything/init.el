(add-module-path "lisp")
(require 'anything)
(require 'anything-config)

(setq anything-sources
      '(anything-c-source-buffers
        anything-c-source-buffer-not-found
        anything-c-source-file-name-history
        anything-c-source-info-pages
        anything-c-source-info-elisp
        anything-c-source-man-pages
        anything-c-source-locate
        anything-c-source-emacs-commands
        anything-c-source-emacs-variables		  
        anything-c-source-bookmarks
        anything-c-source-org-headline
		  anything-c-source-imenu
		  anything-c-source-google-suggest		  
		  anything-c-source-calculation-result		  
		  anything-c-source-complex-command-history
        ))

(global-set-key (kbd "M-X") 'anything)
