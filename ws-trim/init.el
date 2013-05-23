(require 'ws-trim)
(setq ws-trim-level 1)
(setq ws-trim-global-modes '(guess (not message-mode eshell-mode org-mode)))
(global-ws-trim-mode t)

