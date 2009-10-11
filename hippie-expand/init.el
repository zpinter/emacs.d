(require 'hippie-exp)

;; (global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-.") 'hippie-expand)

;default list
(setq hippie-expand-try-functions-list
  '(try-complete-file-name-partially
    try-complete-file-name
    try-expand-all-abbrevs
    try-expand-list
    try-expand-line
    try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol))