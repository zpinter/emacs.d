;;(add-module-path "nxhtml")
;(add-module-path "nxml-mode-20041004")
;(load "rng-auto.el")
(set 'nxml-path (concat zconfig-current-module-dir "/nxml-mode-20041004"))

(add-to-list 'auto-mode-alist
             (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                   'nxml-mode))

(unify-8859-on-decoding-mode)

(fset 'xml-mode 'nxml-mode)
;; (fset 'sgml-mode 'nxml-mode)
;; (fset 'html-mode 'nxml-mode)

;; (zconfig-add-lisp-path "nxhtml")
(zconfig-add-lisp-path "nxhtml/util")

(autoload 'emacs--debug-init "ourcomments-util" "Handy emacs debug launch" t)

;; (load "autostart.el")


(defun xml-pretty-print-buffer ()
    (interactive)
    (save-excursion
        (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
        (nxml-mode)
        (indent-region begin end)))
