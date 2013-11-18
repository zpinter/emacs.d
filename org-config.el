(setq org-startup-folded "showall")

(add-hook 'org-mode-hook #'(lambda () (flyspell-mode -1)))

(defun org-insert-link-as-file ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-insert-link)))

(add-hook 'org-load-hook
            (lambda ()
				  (define-key org-mode-map (kbd "C-c C-g") 'org-insert-link-as-file)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
	(emacs-lisp . t)
	(ruby . t)
   ))

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively t)
(setq org-export-with-sub-superscripts nil)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "MAYBE(m)" "RECURRING(r)" "|" "CANCELLED(c)" "DONE(d)")))

(setq org-directory "~/org")
(setq org-agenda-files '("~/org"))
(setq org-export-backends '(ascii html icalendar latex md))

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
;; (add-hook 'org-mode-hook 'turn-on-flyspell 'append)


(global-set-key (kbd "C-c r") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
		(quote (("t" "Todo" entry (file+headline "~/org/todo.org" "Inbox")
					"* TODO    %? \n")
				  ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
					"* Meeting on %U\n%?")
				  ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
					"* %?")
				  ("j" "Journal" entry (file+datetree "~/org/journal.org")
					"* %?\nEntered on %U\n  %i\n"))))

(add-hook 'org-capture-after-finalize-hook #'(lambda () (org-save-all-org-buffers)))

(defadvice org-capture-refile (after save-after-refile-advice activate)
  (org-save-all-org-buffers))

(setq org-log-done 'time)

                                        ; Use IDO for target completion
(setq org-completion-use-ido t)

                                        ; Targets include this file and any file contributing to the agenda - up to 5 levels deep
                                        ;(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
                                        ;(setq org-refile-use-outline-path (quote file))
                                        ;(setq org-outline-path-complete-in-steps t)

(setq org-refile-use-outline-path t)
(setq org-refile-targets (quote ((org-agenda-files :level . 1))))
;;(setq org-refile-targets '( (org-agenda-files :regexp . "Tasks") ))
(setq org-outline-path-complete-in-steps nil)


                                        ;ical integration
(setq org-agenda-include-diary t)

;; (setq org-agenda-custom-commands
;;       '(("I" "Import diary from iCal" agenda ""
;;          ((org-agenda-mode-hook
;;            (lambda ()
;;              (org-mac-iCal)))))))

(setq org-todo-keyword-faces
		'(
		  ("TODO" . (:foreground "red" :weight "bold"))
		  ("RECURRING" . (:foreground "purple" :weight "bold"))
		  ("MAYBE" . (:foreground "grey" :weight "bold"))
		  ("WAITING" . (:foreground "yellow" :weight "bold"))
		  ))
