(require 'cask "~/.cask/cask.el")

(defadvice cask-read
  (after cask-read-message (filename) activate)
  (message (concat "cask-read called for " filename))
  )

(defadvice cask-add-dependency
  (after cask-add-dependency-message (name &optional version scope) activate)
  (message (concat "cask-add-dependency called for " name))
  )

(cask-initialize)
