(zconfig-add-lisp-path "contrib/lisp")

(setq org-modules nil)
(add-to-list 'org-modules 'org-mac-iCal)
(add-to-list 'org-modules 'org-gnus)

(require 'org-install)

;; (setq font-lock-maximum-decoration
;;       '((org-mode . nil) (tex-mode . nil) (latex-mode . nil)))

                                        ; (eval-after-load "tex-mode" '(fset 'tex-font-lock-suscript 'ignore))

(setq org-export-with-sub-superscripts nil)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-remember)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "CANCELLED(c)" "DONE(d)")))

(setq org-directory "~/org")
(setq org-archive-location "~/org/archive.txt")
(setq org-default-notes-file "~/org/inbox.txt")
(setq org-special-ctrl-a/e t)
(setq org-return-follows-link nil)
(setq org-fast-tag-selection-single-key t)

(setq org-tag-alist '(
                      ("@home" . ?h)
                      ("@office" . ?o)
                      ("@phone" . ?p)
                      ("@errands" . ?e)
                      ("@mom" . ?m)
                      ("@dad" . ?d)))

(setq org-log-done 'time)
(org-remember-insinuate)

(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" "~/org/inbox.txt" "Tasks")
        ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/journal.txt")))


(require 'org-publish)
(setq org-publish-project-alist
      '(

        ("org-notes"
         :base-directory "~/org/"
         :base-extension "txt"
         :publishing-directory "~/Dropbox/Public/orgpub/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4       ; Just the default for this project.
         :auto-preamble t
         )

        ("org-base"
         :static-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/Public/orgpub/"
         :recursive t
         :publishing-function org-publish-attachment
         )

		  ("org" :components ("org-notes" "org-static"))

        ))

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
;(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
;(setq org-refile-use-outline-path (quote file))
;(setq org-outline-path-complete-in-steps t)

(setq org-refile-use-outline-path nil)
(setq org-refile-targets '( (org-agenda-files :regexp . "Tasks") ))
(setq org-outline-path-complete-in-steps nil)


;ical integration
(setq org-agenda-include-diary t)

(setq org-agenda-custom-commands
      '(("I" "Import diary from iCal" agenda ""
         ((org-agenda-mode-hook
           (lambda ()
             (org-mac-iCal)))))))