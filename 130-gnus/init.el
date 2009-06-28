(setq gnus-read-active-file nil)
(setq mail-user-agent 'gnus-user-agent)

(require 'extract-ical)
(extract-ical-gnus-insinuate)

(setq gnus-ignored-newsgroups "")

(require 'external-abook)
(setq external-abook-command "contacts -lf '%%e\t%%n' '%s'")

(eval-after-load "message"
  '(progn
    (add-to-list 'message-mode-hook
     '(lambda ()
       (define-key message-mode-map (kbd "M-/") 'external-abook-try-expand)))))


