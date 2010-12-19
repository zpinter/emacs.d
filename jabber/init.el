(load "jabber-autoloads")

(setq jabber-history-enabled t)

(setq jabber-roster-line-format " %c %-25n %u %-8s  %S")

(setq jabber-account-list
      '(("zpinter@gmail.com"
         (:password . nil)
         (:network-server . "talk.google.com")
         (:port . 443)
         (:connection-type . ssl))
        ))
