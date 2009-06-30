;;; auth-source.el --- authentication sources for Gnus and Emacs

;; Copyright (C) 2008, 2009 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: news

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

;; This is the auth-source.el package.  It lets users tell Gnus how to
;; authenticate in a single place.  Simplicity is the goal.  Instead
;; of providing 5000 options, we'll stick to simple, easy to
;; understand options.

;; Easy setup:
;; (require 'auth-source)
;; (customize-variable 'auth-sources) ;; optional

;; now, whatever sources you've defined for password have to be available

;; if you want encrypted sources, which is strongly recommended, do
;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epa-file-cache-passphrase-for-symmetric-encryption t) ; VERY important

;; before you put some data in ~/.authinfo.gpg (the default place)

;;; For url-auth authentication (HTTP/HTTPS), you need to use:

;;; machine yourmachine.com:80 port http login testuser password testpass

;;; This will match any realm and authentication method (basic or
;;; digest).  If you want finer controls, explore the url-auth source
;;; code and variables.

;;; For tramp authentication, use:

;;; machine yourmachine.com port scp login testuser password testpass

;;; Note that the port denotes the Tramp connection method.  When you
;;; don't use a port entry, you match any Tramp method.

;;; Code:

(require 'gnus-util)

(eval-when-compile (require 'cl))
(eval-when-compile (require 'netrc))

(defgroup auth-source nil
  "Authentication sources."
  :version "23.1" ;; No Gnus
  :group 'gnus)

(defcustom auth-source-protocols '((imap "imap" "imaps" "143" "993")
				   (pop3 "pop3" "pop" "pop3s" "110" "995")
				   (ssh  "ssh" "22")
				   (sftp "sftp" "115")
				   (smtp "smtp" "25"))
  "List of authentication protocols and their names"

  :group 'auth-source
  :version "23.1" ;; No Gnus
  :type '(repeat :tag "Authentication Protocols"
		 (cons :tag "Protocol Entry"
		       (symbol :tag "Protocol")
		       (repeat :tag "Names"
			       (string :tag "Name")))))

;;; generate all the protocols in a format Customize can use
(defconst auth-source-protocols-customize
  (mapcar (lambda (a)
	    (let ((p (car-safe a)))
	      (list 'const
		    :tag (upcase (symbol-name p))
		    p)))
	  auth-source-protocols))

(defvar auth-source-cache (make-hash-table :test 'equal)
  "Cache for auth-source data")

(defcustom auth-source-do-cache t
  "Whether auth-source should cache information."
  :group 'auth-source
  :version "23.1" ;; No Gnus
  :type `boolean)

(defcustom auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t))
  "List of authentication sources.

Each entry is the authentication type with optional properties."
  :group 'auth-source
  :version "23.1" ;; No Gnus
  :type `(repeat :tag "Authentication Sources"
		 (list :tag "Source definition"
		       (const :format "" :value :source)
		       (string :tag "Authentication Source")
		       (const :format "" :value :host)
		       (choice :tag "Host (machine) choice"
			       (const :tag "Any" t)
			       (regexp :tag "Host (machine) regular expression (TODO)")
			       (const :tag "Fallback" nil))
		       (const :format "" :value :protocol)
		       (choice :tag "Protocol"
			       (const :tag "Any" t)
			       (const :tag "Fallback" nil)
			       ,@auth-source-protocols-customize))))

;; temp for debugging
;; (unintern 'auth-source-protocols)
;; (unintern 'auth-sources)
;; (customize-variable 'auth-sources)
;; (setq auth-sources nil)
;; (format "%S" auth-sources)
;; (customize-variable 'auth-source-protocols)
;; (setq auth-source-protocols nil)
;; (format "%S" auth-source-protocols)
;; (auth-source-pick "a" 'imap)
;; (auth-source-user-or-password "login" "imap.myhost.com" 'imap)
;; (auth-source-user-or-password "password" "imap.myhost.com" 'imap)
;; (auth-source-user-or-password-imap "login" "imap.myhost.com")
;; (auth-source-user-or-password-imap "password" "imap.myhost.com")
;; (auth-source-protocol-defaults 'imap)

(defun auth-source-pick (host protocol &optional fallback)
  "Parse `auth-sources' for HOST, and PROTOCOL matches.

Returns fallback choices (where PROTOCOL or HOST are nil) with FALLBACK t."
  (interactive "sHost: \nsProtocol: \n") ;for testing
  (let (choices)
    (dolist (choice auth-sources)
      (let ((h (plist-get choice :host))
	    (p (plist-get choice :protocol)))
	(when (and
	       (or (equal t h)
		   (and (stringp h) (string-match h host))
		   (and fallback (equal h nil)))
	       (or (equal t p)
		   (and (symbolp p) (equal p protocol))
		   (and fallback (equal p nil))))
	  (push choice choices))))
    (if choices
	choices
      (unless fallback
	(auth-source-pick host protocol t)))))

(defun auth-source-forget-user-or-password (mode host protocol)
  (interactive "slogin/password: \nsHost: \nsProtocol: \n") ;for testing
  (remhash (format "%s %s:%s" mode host protocol) auth-source-cache))

(defun auth-source-forget-all-cached ()
  "Forget all cached auth-source authentication tokens."
  (interactive)
  (setq auth-source-cache (make-hash-table :test 'equal)))

(defun auth-source-user-or-password (mode host protocol)
  "Find MODE (string or list of strings) matching HOST and PROTOCOL.
MODE can be \"login\" or \"password\" for example."
  (gnus-message 9
		"auth-source-user-or-password: get %s for %s (%s)"
		mode host protocol)
  (let* ((listy (listp mode))
	 (mode (if listy mode (list mode)))
	 (cname (format "%s %s:%s" mode host protocol))
	 (found (gethash cname auth-source-cache)))
    (if found
	(progn
	  (gnus-message 9
			"auth-source-user-or-password: cached %s=%s for %s (%s)"
			mode
			;; don't show the password
			(if (member "password" mode) "SECRET" found)
			host protocol)
	  found)
      (dolist (choice (auth-source-pick host protocol))
	(setq found (netrc-machine-user-or-password
		     mode
		     (plist-get choice :source)
		     (list host)
		     (list (format "%s" protocol))
		     (auth-source-protocol-defaults protocol)))
	(when found
	  (gnus-message 9
			"auth-source-user-or-password: found %s=%s for %s (%s)"
			mode
			;; don't show the password
			(if (member "password" mode) "SECRET" found)
			host protocol)
	  (setq found (if listy found (car-safe found)))
	  (when auth-source-do-cache
	    (puthash cname found auth-source-cache)))
	(return found)))))

(defun auth-source-protocol-defaults (protocol)
  "Return a list of default ports and names for PROTOCOL."
  (cdr-safe (assoc protocol auth-source-protocols)))

(defun auth-source-user-or-password-imap (mode host)
  (auth-source-user-or-password mode host 'imap))

(defun auth-source-user-or-password-pop3 (mode host)
  (auth-source-user-or-password mode host 'pop3))

(defun auth-source-user-or-password-ssh (mode host)
  (auth-source-user-or-password mode host 'ssh))

(defun auth-source-user-or-password-sftp (mode host)
  (auth-source-user-or-password mode host 'sftp))

(defun auth-source-user-or-password-smtp (mode host)
  (auth-source-user-or-password mode host 'smtp))

(provide 'auth-source)

;; arch-tag: ff1afe78-06e9-42c2-b693-e9f922cbe4ab
;;; auth-source.el ends here
