(require 'pop3)

(setq mail-user-agent 'gnus-user-agent)

;; ;; Default smtpmail.el configurations.
;; (require 'smtpmail)
;; (setq send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       mail-from-style nil
;;       user-full-name "Zachary Pinter"
;;       user-mail-address "zpinter@zacharypinter.com"
;;       message-signature-file "~/personal_signature"
;;       smtpmail-debug-info t
;;       smtpmail-debug-verb t)

;; (defun set-smtp-plain (server port)
;;   "Set related SMTP variables for supplied parameters."
;;   (setq smtpmail-smtp-server server
;;         smtpmail-smtp-service port
;;         smtpmail-auth-credentials "~/.authinfo"
;;         smtpmail-starttls-credentials nil)
;;   (message "Setting SMTP server to `%s:%s'."
;;             server port address))

;; (defun set-smtp-ssl (server port key cert)
;;   "Set related SMTP and SSL variables for supplied parameters."
;;   (setq starttls-use-gnutls t
;;         starttls-gnutls-program "gnutls-cli"
;;         starttls-extra-arguments nil
;;         smtpmail-smtp-server server
;;         smtpmail-smtp-service port
;;         smtpmail-starttls-credentials (list (list server port key cert))

;;         smtpmail-auth-credentials "~/.authinfo")
;;   (message
;;    "Setting SMTP server to `%s:%s'. (SSL enabled.)"
;;    server port address))

;; (defun change-smtp ()
;;   "Change the SMTP server according to the current from line."
;;   (save-excursion
;;     (loop with from = (save-restriction
;;                         (message-narrow-to-headers)
;;                         (message-fetch-field "from"))
;;           for (acc-type address . auth-spec) in smtp-accounts
;;           when (string-match address from)
;;           do (cond
;;               ((eql acc-type 'plain)
;;                (return (apply 'set-smtp-plain auth-spec)))
;;               ((eql acc-type 'ssl)
;;                (return (apply 'set-smtp-ssl auth-spec)))
;;               (t (error "Unrecognized SMTP account type: `%s'." acc-type)))
;;           finally (error "Cannot interfere SMTP information."))))

;; (add-hook 'message-send-hook 'change-smtp)

(setq gnus-ignored-newsgroups "")


