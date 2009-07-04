;;; smtpmail.el --- simple SMTP protocol (RFC 821) for sending mail

;; Copyright (C) 1995, 1996, 2001, 2002, 2003, 2004, 2005, 2006, 2007,
;;   2008, 2009  Free Software Foundation, Inc.

;; Author: Tomoji Kagatani <kagatani@rbc.ncl.omron.co.jp>
;; Maintainer: Simon Josefsson <simon@josefsson.org>
;; w32 Maintainer: Brian D. Carlstrom <bdc@ai.mit.edu>
;; ESMTP support: Simon Leinen <simon@switch.ch>
;; Hacked by Mike Taylor, 11th October 1999 to add support for
;; automatically appending a domain to RCPT TO: addresses.
;; AUTH=LOGIN support: Stephen Cranefield <scranefield@infoscience.otago.ac.nz>
;; Keywords: mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Send Mail to smtp host from smtpmail temp buffer.

;; Please add these lines in your .emacs(_emacs) or use customize.
;;
;;(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
;;(setq message-send-mail-function 'smtpmail-send-it) ; if you use message/Gnus
;;(setq smtpmail-default-smtp-server "YOUR SMTP HOST")
;;(setq smtpmail-local-domain "YOUR DOMAIN NAME")
;;(setq smtpmail-sendto-domain "YOUR DOMAIN NAME")
;;(setq smtpmail-debug-info t) ; only to debug problems
;;(setq smtpmail-auth-credentials  ; or use ~/.authinfo
;;      '(("YOUR SMTP HOST" 25 "username" "password")))
;;(setq smtpmail-starttls-credentials
;;      '(("YOUR SMTP HOST" 25 "~/.my_smtp_tls.key" "~/.my_smtp_tls.cert")))
;; Where the 25 equals the value of `smtpmail-smtp-service', it can be an
;; integer or a string, just as long as they match (eq).

;; To queue mail, set smtpmail-queue-mail to t and use
;; smtpmail-send-queued-mail to send.

;; Modified by Stephen Cranefield <scranefield@infoscience.otago.ac.nz>,
;; 22/6/99, to support SMTP Authentication by the AUTH=LOGIN mechanism.
;; See http://help.netscape.com/products/server/messaging/3x/info/smtpauth.html
;; Rewritten by Simon Josefsson to use same credential variable as AUTH
;; support below.

;; Modified by Simon Josefsson <jas@pdc.kth.se>, 22/2/99, to support SMTP
;; Authentication by the AUTH mechanism.
;; See http://www.ietf.org/rfc/rfc2554.txt

;; Modified by Simon Josefsson <simon@josefsson.org>, 2000-10-07, to support
;; STARTTLS.  Requires external program
;; ftp://ftp.opaopa.org/pub/elisp/starttls-*.tar.gz.
;; See http://www.ietf.org/rfc/rfc2246.txt, http://www.ietf.org/rfc/rfc2487.txt

;;; Code:

;; This version of `smtpmail.el' should only be used with Emacs 21.
(if (featurep 'xemacs)
    (error "Please use `smtpmail.el' from the mail-lib package.")
  (when (>= emacs-major-version 22)
    (error "Please use `smtpmail.el' bundled with Emacs.")))

(require 'sendmail)
(autoload 'starttls-any-program-available "starttls")
(autoload 'starttls-open-stream "starttls")
(autoload 'starttls-negotiate "starttls")
(autoload 'mail-strip-quoted-names "mail-utils")
(autoload 'message-make-date "message")
(autoload 'message-make-message-id "message")
(autoload 'rfc2104-hash "rfc2104")
(autoload 'netrc-parse "netrc")
(autoload 'netrc-machine "netrc")
(autoload 'netrc-get "netrc")
(autoload 'auth-source-user-or-password "auth-source")

;;;
(defgroup smtpmail nil
  "SMTP protocol for sending mail."
  :group 'mail)


(defcustom smtpmail-default-smtp-server nil
  "*Specify default SMTP server.
This only has effect if you specify it before loading the smtpmail library."
  :type '(choice (const nil) string)
  :group 'smtpmail)

(defcustom smtpmail-smtp-server
  (or (getenv "SMTPSERVER") smtpmail-default-smtp-server)
  "*The name of the host running SMTP server."
  :type '(choice (const nil) string)
  :group 'smtpmail)

(defcustom smtpmail-smtp-service 25
  "*SMTP service port number.
The default value would be \"smtp\" or 25."
  :type '(choice (integer :tag "Port") (string :tag "Service"))
  :group 'smtpmail)

(defcustom smtpmail-local-domain nil
  "*Local domain name without a host name.
If the function `system-name' returns the full internet address,
don't define this value."
  :type '(choice (const nil) string)
  :group 'smtpmail)

(defcustom smtpmail-sendto-domain nil
  "*Local domain name without a host name.
This is appended (with an @-sign) to any specified recipients which do
not include an @-sign, so that each RCPT TO address is fully qualified.
\(Some configurations of sendmail require this.)

Don't bother to set this unless you have get an error like:
	Sending failed; SMTP protocol error
when sending mail, and the *trace of SMTP session to <somewhere>*
buffer includes an exchange like:
	RCPT TO: <someone>
	501 <someone>: recipient address must contain a domain
"
  :type '(choice (const nil) string)
  :group 'smtpmail)

(defcustom smtpmail-debug-info nil
  "Whether to print info in buffer *trace of SMTP session to <somewhere>*.
See also `smtpmail-debug-verb' which determines if the SMTP protocol should
be verbose as well."
  :type 'boolean
  :group 'smtpmail)

(defcustom smtpmail-debug-verb nil
  "Whether this library sends the SMTP VERB command or not.
The commands enables verbose information from the SMTP server."
  :type 'boolean
  :group 'smtpmail)

(defcustom smtpmail-code-conv-from nil ;; *junet*
  "*smtpmail code convert from this code to *internal*..for tiny-mime.."
  :type 'boolean
  :group 'smtpmail)

(defcustom smtpmail-queue-mail nil
  "*If set, mail is queued; otherwise it is sent immediately.
If queued, it is stored in the directory `smtpmail-queue-dir'
and sent with `smtpmail-send-queued-mail'."
  :type 'boolean
  :group 'smtpmail)

(defcustom smtpmail-queue-dir "~/Mail/queued-mail/"
  "*Directory where `smtpmail.el' stores queued mail."
  :type 'directory
  :group 'smtpmail)

(defcustom smtpmail-auth-credentials "~/.authinfo"
  "Specify username and password for servers, directly or via .netrc file.
This variable can either be a filename pointing to a file in netrc(5)
format, or list of four-element lists that contain, in order,
`servername' (a string), `port' (an integer), `user' (a string) and
`password' (a string, or nil to query the user when needed).  If you
need to enter a `realm' too, add it to the user string, so that it
looks like `user@realm'."
  :type '(choice file
		 (repeat (list (string  :tag "Server")
		       (integer :tag "Port")
		       (string  :tag "Username")
		       (choice (const :tag "Query when needed" nil)
				       (string  :tag "Password")))))
  :version "22.1"
  :group 'smtpmail)

(defcustom smtpmail-starttls-credentials '(("" 25 "" ""))
  "Specify STARTTLS keys and certificates for servers.
This is a list of four-element list with `servername' (a string),
`port' (an integer), `key' (a filename) and `certificate' (a
filename).
If you do not have a certificate/key pair, leave the `key' and
`certificate' fields as `nil'.  A key/certificate pair is only
needed if you want to use X.509 client authenticated
connections."
  :type '(repeat (list (string  :tag "Server")
		       (integer :tag "Port")
		       (file    :tag "Key")
		       (file    :tag "Certificate")))
  :version "21.1"
  :group 'smtpmail)

(defcustom smtpmail-warn-about-unknown-extensions nil
  "*If set, print warnings about unknown SMTP extensions.
This is mainly useful for development purposes, to learn about
new SMTP extensions that might be useful to support."
  :type 'boolean
  :version "21.1"
  :group 'smtpmail)

(defvar smtpmail-queue-index-file "index"
  "File name of queued mail index.
This is relative to `smtpmail-queue-dir'.")

(defvar smtpmail-address-buffer)
(defvar smtpmail-recipient-address-list)

(defvar smtpmail-queue-counter 0)

;; Buffer-local variable.
(defvar smtpmail-read-point)

(defvar smtpmail-queue-index (concat smtpmail-queue-dir
				     smtpmail-queue-index-file))

(defconst smtpmail-auth-supported '(cram-md5 plain login)
  "List of supported SMTP AUTH mechanisms.
The list is in preference order.")

;;;
;;;
;;;

(defvar smtpmail-mail-address nil
  "Value to use for envelope-from address for mail from ambient buffer.")

;;;###autoload
(defun smtpmail-send-it ()
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " smtpmail errors")
		  0))
	(tembuf (generate-new-buffer " smtpmail temp"))
	(case-fold-search nil)
	delimline
	(mailbuf (current-buffer))
        ;; Examine this variable now, so that
	;; local binding in the mail buffer will take effect.
	(smtpmail-mail-address
         (or (and mail-specify-envelope-from (mail-envelope-from))
             user-mail-address))
	(smtpmail-code-conv-from
	 (if enable-multibyte-characters
	     (let ((sendmail-coding-system smtpmail-code-conv-from))
	       (select-message-coding-system)))))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  ;; Use the same buffer-file-coding-system as in the mail
	  ;; buffer, otherwise any write-region invocations (e.g., in
	  ;; mail-do-fcc below) will annoy with asking for a suitable
	  ;; encoding.
	  ;;
	  ;; This file (`gnus/contrib/smtpmail.el') is only useful for Emacs
	  ;; which doesn't support the third argument (NOMODIFY) of
	  ;; `set-buffer-file-coding-system'.
	  (set-buffer-file-coding-system smtpmail-code-conv-from nil)
	  (set-buffer-modified-p nil)
	  (force-mode-line-update)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (mail-sendmail-undelimit-header)
	  (setq delimline (point-marker))
;;	  (sendmail-synch-aliases)
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    ;; We used to process Resent-... headers here,
	    ;; but it was not done properly, and the job
	    ;; is done correctly in smtpmail-deduce-address-list.
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
		(replace-match "")
	      ;; This one matches a Subject just before the header delimiter.
	      (if (and (re-search-forward "^Subject:\\([ \t]*\n\\)+" delimline t)
		       (= (match-end 0) delimline))
		  (replace-match "")))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(let* ((login smtpmail-mail-address)
		       (fullname (user-full-name)))
		  (cond ((eq mail-from-style 'angles)
			 (insert "From: " fullname)
			 (let ((fullname-start (+ (point-min) 6))
			       (fullname-end (point-marker)))
			   (goto-char fullname-start)
			   ;; Look for a character that cannot appear unquoted
			   ;; according to RFC 822.
			   (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
						  fullname-end 1)
			       (progn
				 ;; Quote fullname, escaping specials.
				 (goto-char fullname-start)
				 (insert "\"")
				 (while (re-search-forward "[\"\\]"
							   fullname-end 1)
				   (replace-match "\\\\\\&" t))
				 (insert "\""))))
			 (insert " <" login ">\n"))
			((eq mail-from-style 'parens)
			 (insert "From: " login " (")
			 (let ((fullname-start (point)))
			   (insert fullname)
			   (let ((fullname-end (point-marker)))
			     (goto-char fullname-start)
			     ;; RFC 822 says \ and nonmatching parentheses
			     ;; must be escaped in comments.
			     ;; Escape every instance of ()\ ...
			     (while (re-search-forward "[()\\]" fullname-end 1)
			       (replace-match "\\\\\\&" t))
			     ;; ... then undo escaping of matching parentheses,
			     ;; including matching nested parentheses.
			     (goto-char fullname-start)
			     (while (re-search-forward
				     "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
				     fullname-end 1)
			       (replace-match "\\1(\\3)" t)
			       (goto-char fullname-start))))
			 (insert ")\n"))
			((null mail-from-style)
			 (insert "From: " login "\n")))))
	    ;; Insert a `Message-Id:' field if there isn't one yet.
	    (goto-char (point-min))
	    (unless (re-search-forward "^Message-Id:" delimline t)
	      (insert "Message-Id: " (message-make-message-id) "\n"))
	    ;; Insert a `Date:' field if there isn't one yet.
	    (goto-char (point-min))
	    (unless (re-search-forward "^Date:" delimline t)
	      (insert "Date: " (message-make-date) "\n"))
	    ;; Possibly add a MIME header for the current coding system
	    (let (charset)
	      (goto-char (point-min))
	      (and (eq mail-send-nonascii 'mime)
		   (not (re-search-forward "^MIME-version:" delimline t))
		   (progn (skip-chars-forward "\0-\177")
			  (/= (point) (point-max)))
		   smtpmail-code-conv-from
		   (setq charset
			 (coding-system-get smtpmail-code-conv-from
					    'mime-charset))
		   (goto-char delimline)
		   (insert "MIME-version: 1.0\n"
			   "Content-type: text/plain; charset="
			   (symbol-name charset)
			   "\nContent-Transfer-Encoding: 8bit\n")))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
	    (if (eval mail-mailer-swallows-blank-line)
		(newline))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		;; Force mail-do-fcc to use the encoding of the mail
		;; buffer to encode outgoing messages on FCC files.
		(let ((coding-system-for-write smtpmail-code-conv-from))
		  (mail-do-fcc delimline)))
	    (if mail-interactive
		(with-current-buffer errbuf
		  (erase-buffer))))
	  ;;
	  ;;
	  ;;
	  (setq smtpmail-address-buffer (generate-new-buffer "*smtp-mail*"))
	  (setq smtpmail-recipient-address-list
		    (smtpmail-deduce-address-list tembuf (point-min) delimline))
	  (kill-buffer smtpmail-address-buffer)

	  (smtpmail-do-bcc delimline)
	  ; Send or queue
	  (if (not smtpmail-queue-mail)
	      (if (not (null smtpmail-recipient-address-list))
		  (if (not (smtpmail-via-smtp
			    smtpmail-recipient-address-list tembuf))
		      (error "Sending failed; SMTP protocol error"))
		(error "Sending failed; no recipients"))
	    (let* ((file-data
		    (expand-file-name
		     (format "%s_%i"
			     (format-time-string "%Y-%m-%d_%H:%M:%S")
			     (setq smtpmail-queue-counter
				   (1+ smtpmail-queue-counter)))
		     smtpmail-queue-dir))
		   (file-data (convert-standard-filename file-data))
		   (file-elisp (concat file-data ".el"))
		   (buffer-data (create-file-buffer file-data))
		   (buffer-elisp (create-file-buffer file-elisp))
		   (buffer-scratch "*queue-mail*"))
	      (unless (file-exists-p smtpmail-queue-dir)
		(make-directory smtpmail-queue-dir t))
	      (with-current-buffer buffer-data
		(erase-buffer)
		(set-buffer-file-coding-system smtpmail-code-conv-from nil)
		(insert-buffer-substring tembuf)
		(write-file file-data)
		(set-buffer buffer-elisp)
		(erase-buffer)
		(insert (concat
			 "(setq smtpmail-recipient-address-list '"
			 (prin1-to-string smtpmail-recipient-address-list)
			 ")\n"))
		(write-file file-elisp)
		(set-buffer (generate-new-buffer buffer-scratch))
		(insert (concat file-data "\n"))
		(append-to-file (point-min)
				(point-max)
				smtpmail-queue-index)
		)
	      (kill-buffer buffer-scratch)
	      (kill-buffer buffer-data)
	      (kill-buffer buffer-elisp))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))

;;;###autoload
(defun smtpmail-send-queued-mail ()
  "Send mail that was queued as a result of setting `smtpmail-queue-mail'."
  (interactive)
  (with-temp-buffer
    ;;; Get index, get first mail, send it, update index, get second
    ;;; mail, send it, etc...
    (let ((file-msg ""))
      (insert-file-contents smtpmail-queue-index)
      (goto-char (point-min))
      (while (not (eobp))
	(setq file-msg (buffer-substring (point) (line-end-position)))
	(load file-msg)
	;; Insert the message literally: it is already encoded as per
	;; the MIME headers, and code conversions might guess the
	;; encoding wrongly.
	(with-temp-buffer
	  (let ((coding-system-for-read 'no-conversion))
	    (insert-file-contents file-msg))
          (let ((smtpmail-mail-address
                 (or (and mail-specify-envelope-from (mail-envelope-from))
                     user-mail-address)))
            (if (not (null smtpmail-recipient-address-list))
                (if (not (smtpmail-via-smtp smtpmail-recipient-address-list
                                            (current-buffer)))
                    (error "Sending failed; SMTP protocol error"))
              (error "Sending failed; no recipients"))))
	(delete-file file-msg)
	(delete-file (concat file-msg ".el"))
	(delete-region (point-at-bol) (point-at-bol 2)))
      (write-region (point-min) (point-max) smtpmail-queue-index))))

;(defun smtpmail-via-smtp (host,port,sender,destination,smtpmail-text-buffer)

(defun smtpmail-fqdn ()
  (if smtpmail-local-domain
      (concat (system-name) "." smtpmail-local-domain)
    (system-name)))

(defsubst smtpmail-cred-server (cred)
  (nth 0 cred))

(defsubst smtpmail-cred-port (cred)
  (nth 1 cred))

(defsubst smtpmail-cred-key (cred)
  (nth 2 cred))

(defsubst smtpmail-cred-user (cred)
  (nth 2 cred))

(defsubst smtpmail-cred-cert (cred)
  (nth 3 cred))

(defsubst smtpmail-cred-passwd (cred)
  (nth 3 cred))

(defun smtpmail-find-credentials (cred server port)
  (catch 'done
    (let ((l cred) el)
      (while (setq el (pop l))
	(when (and (equal server (smtpmail-cred-server el))
		   (equal port (smtpmail-cred-port el)))
	  (throw 'done el))))))

(defun smtpmail-maybe-append-domain (recipient)
  (if (or (not smtpmail-sendto-domain)
	  (string-match "@" recipient))
      recipient
    (concat recipient "@" smtpmail-sendto-domain)))

(defun smtpmail-intersection (list1 list2)
  (let ((result nil))
    (dolist (el2 list2)
      (when (memq el2 list1)
	(push el2 result)))
    (nreverse result)))

(defvar starttls-extra-args)
(defvar starttls-extra-arguments)

(defun smtpmail-open-stream (process-buffer host port)
  (let ((cred (smtpmail-find-credentials
	       smtpmail-starttls-credentials host port)))
    (if (null (and cred (starttls-any-program-available)))
	;; The normal case.
	(open-network-stream "SMTP" process-buffer host port)
      (let* ((cred-key (smtpmail-cred-key cred))
	     (cred-cert (smtpmail-cred-cert cred))
	     (starttls-extra-args
	      (append
	       starttls-extra-args
	       (when (and (stringp cred-key) (stringp cred-cert)
			  (file-regular-p
			   (setq cred-key (expand-file-name cred-key)))
			  (file-regular-p
			   (setq cred-cert (expand-file-name cred-cert))))
		 (list "--key-file" cred-key "--cert-file" cred-cert))))
	     (starttls-extra-arguments
	      (append
	       starttls-extra-arguments
	       (when (and (stringp cred-key) (stringp cred-cert)
			  (file-regular-p
			   (setq cred-key (expand-file-name cred-key)))
			  (file-regular-p
			   (setq cred-cert (expand-file-name cred-cert))))
		 (list "--x509keyfile" cred-key "--x509certfile" cred-cert)))))
	(starttls-open-stream "SMTP" process-buffer host port)))))

(defun smtpmail-try-auth-methods (process supported-extensions host port)
  (let* ((mechs (cdr-safe (assoc 'auth supported-extensions)))
	 (mech (car (smtpmail-intersection mechs smtpmail-auth-supported)))
	 (auth-user (auth-source-user-or-password
		     "login" host (or port "smtp")))
	 (auth-pass (auth-source-user-or-password
		     "password" host (or port "smtp")))
	 (cred (if (and auth-user auth-pass) ; try user-auth-* before netrc-*
		   (list host port auth-user auth-pass)
		 ;; else, if auth-source didn't return them...
		 (if (stringp smtpmail-auth-credentials)
		     (let* ((netrc (netrc-parse smtpmail-auth-credentials))
			    (port-name (format "%s" (or port "smtp")))
			    (hostentry (netrc-machine netrc host port-name
						      port-name)))
		       (when hostentry
			 (list host port
			       (netrc-get hostentry "login")
			       (netrc-get hostentry "password"))))
		   ;; else, try smtpmail-find-credentials since
		   ;; smtpmail-auth-credentials is not a string
		   (smtpmail-find-credentials
		    smtpmail-auth-credentials host port))))
	 (prompt (when cred (format "SMTP password for %s:%s: "
				    (smtpmail-cred-server cred)
				    (smtpmail-cred-port cred))))
	 (passwd (when cred
		   (or (smtpmail-cred-passwd cred)
		       (read-passwd
			(format "SMTP password for %s:%s: "
				(smtpmail-cred-server cred)
				(smtpmail-cred-port cred))))))
	 ret)
    (when (and cred mech)
      (cond
       ((eq mech 'cram-md5)
	(smtpmail-send-command process (upcase (format "AUTH %s" mech)))
	(if (or (null (car (setq ret (smtpmail-read-response process))))
		(not (integerp (car ret)))
		(>= (car ret) 400))
	    (throw 'done nil))
	(when (eq (car ret) 334)
	  (let* ((challenge (substring (cadr ret) 4))
		 (decoded (base64-decode-string challenge))
		 (hash (rfc2104-hash 'md5 64 16 passwd decoded))
		 (response (concat (smtpmail-cred-user cred) " " hash))
		 ;; Osamu Yamane <yamane@green.ocn.ne.jp>:
		 ;; SMTP auth fails because the SMTP server identifies
		 ;; only the first part of the string (delimited by
		 ;; new line characters) as a response from the
		 ;; client, and the rest as distinct commands.

		 ;; In my case, the response string is 80 characters
		 ;; long.  Without the no-line-break option for
		 ;; base64-encode-sting, only the first 76 characters
		 ;; are taken as a response to the server, and the
		 ;; authentication fails.
		 (encoded (base64-encode-string response t)))
	    (smtpmail-send-command process (format "%s" encoded))
	    (if (or (null (car (setq ret (smtpmail-read-response process))))
		    (not (integerp (car ret)))
		    (>= (car ret) 400))
		(throw 'done nil)))))
       ((eq mech 'login)
	(smtpmail-send-command process "AUTH LOGIN")
	(if (or (null (car (setq ret (smtpmail-read-response process))))
		(not (integerp (car ret)))
		(>= (car ret) 400))
	    (throw 'done nil))
	(smtpmail-send-command
	 process (base64-encode-string (smtpmail-cred-user cred) t))
	(if (or (null (car (setq ret (smtpmail-read-response process))))
		(not (integerp (car ret)))
		(>= (car ret) 400))
	    (throw 'done nil))
	(smtpmail-send-command process (base64-encode-string passwd t))
	(if (or (null (car (setq ret (smtpmail-read-response process))))
		(not (integerp (car ret)))
		(>= (car ret) 400))
	    (throw 'done nil)))
       ((eq mech 'plain)
	;; We used to send an empty initial request, and wait for an
	;; empty response, and then send the password, but this
	;; violate a SHOULD in RFC 2222 paragraph 5.1.  Note that this
	;; is not sent if the server did not advertise AUTH PLAIN in
	;; the EHLO response.  See RFC 2554 for more info.
	(smtpmail-send-command process
			       (concat "AUTH PLAIN "
				       (base64-encode-string
					(concat "\0"
						(smtpmail-cred-user cred)
						"\0"
						passwd) t)))
	(if (or (null (car (setq ret (smtpmail-read-response process))))
		(not (integerp (car ret)))
		(not (equal (car ret) 235)))
	    (throw 'done nil)))

       (t
	(error "Mechanism %s not implemented" mech)))
      ;; Remember the password.
      (when (and (not (stringp smtpmail-auth-credentials))
		 (null (smtpmail-cred-passwd cred)))
	(setcar (cdr (cdr (cdr cred))) passwd)))))

(defun smtpmail-via-smtp (recipient smtpmail-text-buffer)
  (let ((process nil)
	(host (or smtpmail-smtp-server
		  (error "`smtpmail-smtp-server' not defined")))
	(port smtpmail-smtp-service)
        ;; smtpmail-mail-address should be set to the appropriate
        ;; buffer-local value by the caller, but in case not:
        (envelope-from (or smtpmail-mail-address
                           (and mail-specify-envelope-from
                                (mail-envelope-from))
                           user-mail-address))
	response-code
	greeting
	process-buffer
	(supported-extensions '()))
    (unwind-protect
	(catch 'done
	  ;; get or create the trace buffer
	  (setq process-buffer
		(get-buffer-create (format "*trace of SMTP session to %s*" host)))

	  ;; clear the trace buffer of old output
	  (with-current-buffer process-buffer
	    (setq buffer-undo-list t)
	    (erase-buffer))

	  ;; open the connection to the server
	  (setq process (smtpmail-open-stream process-buffer host port))
	  (and (null process) (throw 'done nil))

	  ;; set the send-filter
	  (set-process-filter process 'smtpmail-process-filter)

	  (with-current-buffer process-buffer
	    (set-buffer-process-coding-system 'raw-text-unix 'raw-text-unix)
	    (make-local-variable 'smtpmail-read-point)
	    (setq smtpmail-read-point (point-min))


	    (if (or (null (car (setq greeting (smtpmail-read-response process))))
		    (not (integerp (car greeting)))
		    (>= (car greeting) 400))
		(throw 'done nil)
	      )

	    (let ((do-ehlo t)
		  (do-starttls t))
	      (while do-ehlo
	    ;; EHLO
	    (smtpmail-send-command process (format "EHLO %s" (smtpmail-fqdn)))

	    (if (or (null (car (setq response-code
				     (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(progn
		  ;; HELO
		  (smtpmail-send-command
		   process (format "HELO %s" (smtpmail-fqdn)))

		  (if (or (null (car (setq response-code
					   (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil)))
	      (dolist (line (cdr (cdr response-code)))
		(let ((name (mapcar (lambda (s) (intern (downcase s)))
				    (split-string (substring line 4) "[ ]"))))
		  (and (eq (length name) 1)
		       (setq name (car name)))
		    (and name
		       (cond ((memq (if (consp name) (car name) name)
				    '(verb xvrb 8bitmime onex xone
						  expn size dsn etrn
				      enhancedstatuscodes
				      help xusr
				      auth=login auth starttls))
				(setq supported-extensions
				      (cons name supported-extensions)))
			       (smtpmail-warn-about-unknown-extensions
			      (message "Unknown extension %s" name)))))))

	    (if (and do-starttls
		     (smtpmail-find-credentials smtpmail-starttls-credentials host port)
		     (member 'starttls supported-extensions)
		     (numberp (process-id process)))
		(progn
		  (smtpmail-send-command process (format "STARTTLS"))
		  (if (or (null (car (setq response-code (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))
		  (starttls-negotiate process)
		  (setq do-starttls nil))
	      (setq do-ehlo nil))))

	    (smtpmail-try-auth-methods process supported-extensions host port)

	    (if (or (member 'onex supported-extensions)
		    (member 'xone supported-extensions))
		(progn
		  (smtpmail-send-command process (format "ONEX"))
		  (if (or (null (car (setq response-code (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))))

	    (if (and smtpmail-debug-verb
		     (or (member 'verb supported-extensions)
			 (member 'xvrb supported-extensions)))
		(progn
		  (smtpmail-send-command process (format "VERB"))
		  (if (or (null (car (setq response-code (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))))

	    (if (member 'xusr supported-extensions)
		(progn
		  (smtpmail-send-command process (format "XUSR"))
		  (if (or (null (car (setq response-code (smtpmail-read-response process))))
			  (not (integerp (car response-code)))
			  (>= (car response-code) 400))
		      (throw 'done nil))))

	    ;; MAIL FROM:<sender>
	    (let ((size-part
		   (if (or (member 'size supported-extensions)
			   (assoc 'size supported-extensions))
		       (format " SIZE=%d"
			       (with-current-buffer smtpmail-text-buffer
				 ;; size estimate:
				 (+ (- (point-max) (point-min))
				    ;; Add one byte for each change-of-line
				    ;; because of CR-LF representation:
				    (count-lines (point-min) (point-max)))))
		     ""))
		  (body-part
		   (if (member '8bitmime supported-extensions)
		       ;; FIXME:
		       ;; Code should be added here that transforms
		       ;; the contents of the message buffer into
		       ;; something the receiving SMTP can handle.
		       ;; For a receiver that supports 8BITMIME, this
		       ;; may mean converting BINARY to BASE64, or
		       ;; adding Content-Transfer-Encoding and the
		       ;; other MIME headers.  The code should also
		       ;; return an indication of what encoding the
		       ;; message buffer is now, i.e. ASCII or
		       ;; 8BITMIME.
		       (if nil
			   " BODY=8BITMIME"
			 "")
		     "")))
;	      (smtpmail-send-command process (format "MAIL FROM:%s@%s" (user-login-name) (smtpmail-fqdn)))
	      (smtpmail-send-command process (format "MAIL FROM:<%s>%s%s"
                                                     envelope-from
						     size-part
						     body-part))

	      (if (or (null (car (setq response-code (smtpmail-read-response process))))
		      (not (integerp (car response-code)))
		      (>= (car response-code) 400))
		  (throw 'done nil)
		))

	    ;; RCPT TO:<recipient>
	    (let ((n 0))
	      (while (not (null (nth n recipient)))
		(smtpmail-send-command process (format "RCPT TO:<%s>" (smtpmail-maybe-append-domain (nth n recipient))))
		(setq n (1+ n))

		(setq response-code (smtpmail-read-response process))
		(if (or (null (car response-code))
			(not (integerp (car response-code)))
			(>= (car response-code) 400))
		    (throw 'done nil)
		  )
		))

	    ;; DATA
	    (smtpmail-send-command process "DATA")

	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )

	    ;; Mail contents
	    (smtpmail-send-data process smtpmail-text-buffer)

	    ;;DATA end "."
	    (smtpmail-send-command process ".")

	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
		    (not (integerp (car response-code)))
		    (>= (car response-code) 400))
		(throw 'done nil)
	      )

	    ;;QUIT
;	    (smtpmail-send-command process "QUIT")
;	    (and (null (car (smtpmail-read-response process)))
;		 (throw 'done nil))
	    t ))
      (if process
	  (with-current-buffer (process-buffer process)
	    (smtpmail-send-command process "QUIT")
	    (smtpmail-read-response process)

;	    (if (or (null (car (setq response-code (smtpmail-read-response process))))
;		    (not (integerp (car response-code)))
;		    (>= (car response-code) 400))
;		(throw 'done nil)
;	      )
	    (delete-process process)
	    (unless smtpmail-debug-info
	      (kill-buffer process-buffer)))))))


(defun smtpmail-process-filter (process output)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output)))

(defun smtpmail-read-response (process)
  (let ((case-fold-search nil)
	(response-strings nil)
	(response-continue t)
	(return-value '(nil ()))
	match-end)
    (catch 'done
      (while response-continue
	(goto-char smtpmail-read-point)
	(while (not (search-forward "\r\n" nil t))
	  (unless (memq (process-status process) '(open run))
	    (throw 'done nil))
	  (accept-process-output process)
	  (goto-char smtpmail-read-point))

	(setq match-end (point))
	(setq response-strings
	      (cons (buffer-substring smtpmail-read-point (- match-end 2))
		    response-strings))

	(goto-char smtpmail-read-point)
	(if (looking-at "[0-9]+ ")
	    (let ((begin (match-beginning 0))
		  (end (match-end 0)))
	      (if smtpmail-debug-info
		  (message "%s" (car response-strings)))

	      (setq smtpmail-read-point match-end)

	      ;; ignore lines that start with "0"
	      (if (looking-at "0[0-9]+ ")
		  nil
		(setq response-continue nil)
		(setq return-value
		      (cons (string-to-number
			     (buffer-substring begin end))
			    (nreverse response-strings)))))

	  (if (looking-at "[0-9]+-")
	      (progn (if smtpmail-debug-info
			 (message "%s" (car response-strings)))
		     (setq smtpmail-read-point match-end)
		     (setq response-continue t))
	    (progn
	      (setq smtpmail-read-point match-end)
	      (setq response-continue nil)
	      (setq return-value
		    (cons nil (nreverse response-strings)))))))
      (setq smtpmail-read-point match-end))
    return-value))


(defun smtpmail-send-command (process command)
  (goto-char (point-max))
  (if (= (aref command 0) ?P)
      (insert "PASS <omitted>\r\n")
    (insert command "\r\n"))
  (setq smtpmail-read-point (point))
  (process-send-string process command)
  (process-send-string process "\r\n"))

(defun smtpmail-send-data-1 (process data)
  (goto-char (point-max))

  (if (and (multibyte-string-p data)
	   smtpmail-code-conv-from)
      (setq data (string-as-multibyte
		  (encode-coding-string data smtpmail-code-conv-from))))

  (if smtpmail-debug-info
      (insert data "\r\n"))

  (setq smtpmail-read-point (point))
  ;; Escape "." at start of a line
  (if (eq (string-to-char data) ?.)
      (process-send-string process "."))
  (process-send-string process data)
  (process-send-string process "\r\n")
  )

(defun smtpmail-send-data (process buffer)
  (let ((data-continue t) sending-data)
    (with-current-buffer buffer
      (goto-char (point-min)))
    (while data-continue
      (with-current-buffer buffer
        (setq sending-data (buffer-substring (point-at-bol) (point-at-eol)))
	(end-of-line 2)
        (setq data-continue (not (eobp))))
      (smtpmail-send-data-1 process sending-data))))

(defun smtpmail-deduce-address-list (smtpmail-text-buffer header-start header-end)
  "Get address list suitable for smtp RCPT TO: <address>."
  (unwind-protect
      (with-current-buffer smtpmail-address-buffer
	(erase-buffer)
	(let
	    ((case-fold-search t)
	     (simple-address-list "")
	     this-line
	     this-line-end
	     addr-regexp)
	  (insert-buffer-substring smtpmail-text-buffer header-start header-end)
	  (goto-char (point-min))
	  ;; RESENT-* fields should stop processing of regular fields.
	  (save-excursion
	    (setq addr-regexp
		  (if (re-search-forward "^Resent-\\(to\\|cc\\|bcc\\):"
					 header-end t)
		      "^Resent-\\(to\\|cc\\|bcc\\):"
		    "^\\(To:\\|Cc:\\|Bcc:\\)")))

	  (while (re-search-forward addr-regexp header-end t)
	    (replace-match "")
	    (setq this-line (match-beginning 0))
	    (forward-line 1)
	    ;; get any continuation lines
	    (while (and (looking-at "^[ \t]+") (< (point) header-end))
	      (forward-line 1))
	    (setq this-line-end (point-marker))
	    (setq simple-address-list
		  (concat simple-address-list " "
			  (mail-strip-quoted-names (buffer-substring this-line this-line-end))))
	    )
	  (erase-buffer)
	  (insert " " simple-address-list "\n")
	  (subst-char-in-region (point-min) (point-max) 10 ?  t);; newline --> blank
	  (subst-char-in-region (point-min) (point-max) ?, ?  t);; comma   --> blank
	  (subst-char-in-region (point-min) (point-max)  9 ?  t);; tab     --> blank

	  (goto-char (point-min))
	  ;; tidyness in case hook is not robust when it looks at this
	  (while (re-search-forward "[ \t]+" header-end t) (replace-match " "))

	  (goto-char (point-min))
	  (let (recipient-address-list)
	    (while (re-search-forward " \\([^ ]+\\) " (point-max) t)
	      (backward-char 1)
	      (setq recipient-address-list (cons (buffer-substring (match-beginning 1) (match-end 1))
						 recipient-address-list))
	      )
	    (setq smtpmail-recipient-address-list recipient-address-list))

	  )
	)
    )
  )


(defun smtpmail-do-bcc (header-end)
  "Delete [Resent-]BCC: and their continuation lines from the header area.
There may be multiple BCC: lines, and each may have arbitrarily
many continuation lines."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      ;; iterate over all BCC: lines
      (while (re-search-forward "^\\(RESENT-\\)?BCC:" header-end t)
	(delete-region (match-beginning 0)
		       (progn (forward-line 1) (point)))
	;; get rid of any continuation lines
	(while (and (looking-at "^[ \t].*\n") (< (point) header-end))
	  (replace-match ""))))))


(provide 'smtpmail)

;; arch-tag: a76992df-6d71-43b7-9e72-4bacc6c05466
;;; smtpmail.el ends here
