(zconfig-add-lisp-path "contrib/lisp")

(setq org-modules nil)
(setq org-startup-folded "showall")
(add-to-list 'org-modules 'org-mac-iCal)

(require 'org)

;; (setq font-lock-maximum-decoration
;;       '((org-mode . nil) (tex-mode . nil) (latex-mode . nil)))

                                        ; (eval-after-load "tex-mode" '(fset 'tex-font-lock-suscript 'ignore))

(defun org-insert-link-as-file ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-insert-link)))

(add-hook 'org-load-hook
            (lambda ()
				  (define-key org-mode-map (kbd "C-c C-g") 'org-insert-link-as-file)))


(setq org-export-with-sub-superscripts nil)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "CANCELLED(c)" "DONE(d)")))

(setq org-directory "~/org")
(setq org-agenda-files '("~/org/gtd"))


;; Disable C-c [ and C-c ] in org-mode, since that screws with org-agenda-files
(add-hook 'org-mode-hook
          (lambda ()
            ;; Undefine C-c [ and C-c ] since this breaks my
            ;; org-agenda files when directories are include It
            ;; expands the files in the directories individually
            (org-defkey org-mode-map "\C-c["    'undefined)
            (org-defkey org-mode-map "\C-c]"    'undefined))
          'append)


(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(setq org-default-notes-file "~/org/gtd.txt")
(setq org-special-ctrl-a/e t)
(setq org-return-follows-link nil)
(setq org-fast-tag-selection-single-key t)

(setq org-tag-alist '(
                      ("@home" . ?h)
                      ("@office" . ?o)
                      ("@phone" . ?p)
                      ("@errands" . ?e)
                      ("@computer" . ?c)
                      ("@towatch" . ?w)
                      ("@toread" . ?r)))

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)


(global-set-key (kbd "C-c r") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/gtd/inbox.org")
               "* TODO %?\n%U\n%a\n")
              ("n" "note" entry (file "~/org/gtd/inbox.org")
               "* %? :NOTE:\n%U\n%a\n")
              ("j" "Journal" entry (file+datetree "~/org/gtd/journal.org")
               "* %?\n%U\n"))))

(setq org-log-done 'time)
;; (org-remember-insinuate)

;; (setq org-remember-templates
;;       '(
;; 		  ("Todo" ?t "* TODO %?\n  %i\n  %a" "~/org/gtd.txt" "Inbox")
;; 		  ("Zebra" ?z "* TODO %?\n  %i\n  %a" "~/org/gtd.txt" "Zebra")
;;         ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/journal.txt")
;; 		  ))

(setq org-export-html-style
		"<link rel=\"stylesheet\" type=\"text/css\" href=\"http://dl.dropbox.com/u/31884/org-style.css\" />")

(require 'org-publish)
(setq org-publish-project-alist
      '(

        ("org-notes"
         :base-directory "~/org/"
         :base-extension "txt"
         :publishing-directory "~/Dropbox/orgpub/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4       ; Just the default for this project.
         :auto-preamble t
         )

        ("org-base"
         :static-directory "~/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/orgpub/"
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
(setq org-refile-targets (quote ((org-agenda-files :level . 1))))
;;(setq org-refile-targets '( (org-agenda-files :regexp . "Tasks") ))
(setq org-outline-path-complete-in-steps nil)


                                        ;ical integration
(setq org-agenda-include-diary t)

(setq org-agenda-custom-commands
      '(("I" "Import diary from iCal" agenda ""
         ((org-agenda-mode-hook
           (lambda ()
             (org-mac-iCal)))))))

(setq org-todo-keyword-faces
		'(("TODO" . (:foreground "red" :weight "bold"))))


(setq org-odt-data-dir (concat zconfig-current-module-dir "/data"))
