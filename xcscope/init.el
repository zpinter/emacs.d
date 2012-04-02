(require 'xcscope)
(setq cscope-do-not-update-database t)
(add-hook 'java-mode-hook (function cscope:hook))
