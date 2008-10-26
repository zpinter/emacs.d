(add-module-path "nxhtml")
(load "autostart.el")

(fset 'xml-mode 'nxml-mode)
(fset 'sgml-mode 'nxml-mode)

(defun nxml-mode-additional-keys ()
  "Key bindings to add to `nxml-mode'."
  (define-key nxml-mode-map [(control return)] 'set-mark-command)
  )

(add-hook 'nxml-mode-hook 'nxml-mode-additional-keys)

