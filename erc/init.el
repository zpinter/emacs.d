(require 'erc-auto)

(setq erc-echo-notices-in-minibuffer-flag t)


;;;;;; Wrapper for growlnotify
(defun growl-chat (title message &optional sticky)
  (interactive "sTitle: \nsGrowl: ")
  (shell-command
   (format "/usr/local/bin/growlnotify %s -m '%s' --appIcon 'Emacs' %s" title message (if sticky "--sticky" ""))))

(defun growl-chat-sticky (title message)
  (interactive "sTitle: \nsGrowl: ")
  (growl-chat title message t))

(defvar my-erc-page-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defvar my-erc-page-timeout 1
  "Number of seconds that must elapse between notifications from
the same person.")

(defun my-erc-page-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`my-erc-page-timeout'."
  (unless delay (setq delay my-erc-page-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick my-erc-page-nick-alist))
        (last-time nil))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) my-erc-page-nick-alist)
      nil)))

(defun my-erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (my-erc-page-allowed nick))
		(growl-chat (format "Message from: %s" nick) msg)
      nil)))

(eval-after-load "erc"
  '(add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG t))

;; (eval-after-load "erc"
;;   '(progn
;;     (setq erc-auto-query t
;;      erc-bbdb-auto-create-on-whois-p t
;;      erc-fill-column (- (window-width) 2))
;;     (require 'erc-imenu)
;;     (require 'erc-menu)
;;     (require 'erc-notify)
;;     (require 'erc-ring)
;;     (erc-button-mode 1)
;;     (erc-completion-mode 1)
;;     (erc-fill-mode 1)
;;     (erc-match-mode 1)
;;     (erc-netsplit-mode 1)
;;     (erc-services-mode 1)
;;     (erc-timestamp-mode 1)
;;     (erc-track-mode 1)
;;     (add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)
;;     (add-to-list 'erc-nick-popup-alist
;;      '("DebianDB" .
;;        (shell-command
;;         (format
;;          "ldapsearch -x -P 2 -h db.debian.org -b dc=debian,dc=org ircnick=%s"
;;          nick))))))