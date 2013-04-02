(require 'go-mode)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/go/bin"))
(add-to-list 'exec-path "/usr/local/go/bin" t)
