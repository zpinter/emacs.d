;;; mirah-complete-config.el --- Auto Java Completion  for GNU Emacs

;;  Install

;; add these line in your emacs init file .
;; (add-to-list 'load-path  "~/.emacs.d/mirah-complete/")
;; (require 'mirah-complete-config)
;; (add-hook 'mirah-mode-hook 'mirah-complete-mode)
;; (add-hook 'find-file-hook 'amc-4-jsp-find-file-hook)

;; to enable Auto Java Complete ,just need to add mirah-complete-mode
;; minor-mode in your mode hook, for example
;;         (add-hook 'mirah-mode-hook 'mirah-complete-mode)
;;  If your want to enable  mirah-complete-mode when openning
;;  a jsp file. you can
;;         (add-hook 'jsp-mode 'mirah-complete-mode)
;;  if you has a jsp-mode,
;;  if not ,you can do it like this
;;         (add-hook 'find-file-hook 'amc-4-jsp-find-file-hook)


;;  Code

(require 'auto-complete)
(require 'mirah-complete)
;; conflect with 
;; (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; when complete constructor 

;; sources for auto complete
(ac-define-source amc-import
  '((candidates . (amc-import-package-candidates))
    (prefix . prefix-support-jsp-importing)))

(ac-define-source amc-class
  '((candidates . (amc-complete-class-candidates ))
   (prefix . "\\b\\([A-Z][a-zA-Z0-9_]*\\)")
   (cache)))

;; (ac-define-source amc-constructor
;;   '((candidates . (amc-complete-constructor-candidates ))
;;    (cache)
;;    (requires . 3)
;;    (prefix . "\\bnew[ \t]+\\([A-Z][a-zA-Z0-9_]*[ \t]*(?\\)")
;;    (action . amc-expand-yasnippet-templete-with-ac)))

(ac-define-source amc-method
  '((candidates . (amc-complete-method-candidates))
  (cache)
  (requires . 0)
  (prefix . "\\.\\(.*\\)") 
  (action .  amc-expand-yasnippet-templete-with-ac)))

(ac-define-source amc-keywords
  '((candidates . (amc-mirah-keywords-candidates))))
;; end of sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;hooks
(defun mirah-complete-init()
  (amc-init)
  (add-to-list 'ac-sources 'ac-source-amc-keywords)
  (add-to-list 'ac-sources 'ac-source-amc-method)
  (add-to-list 'ac-sources 'ac-source-amc-class)
  ;; (add-to-list 'ac-sources 'ac-source-amc-constructor)
  (add-to-list 'ac-sources 'ac-source-amc-import)
;; auto import all Class in source file    
(local-set-key (kbd "C-c i") 'amc-import-all-unimported-class)
;; import Class where under point 
(local-set-key (kbd "C-c m") 'amc-import-class-under-point))

(defun mirah-complete-exit()
  ;; (setq ac-sources (delete 'ac-source-amc-constructor ac-sources))
  (setq ac-sources (delete 'ac-source-amc-class ac-sources))
  (setq ac-sources (delete 'ac-source-amc-method ac-sources))
  (setq ac-sources (delete 'ac-source-amc-keywords ac-sources))
  (setq ac-sources (delete 'ac-source-amc-import ac-sources)))


(defvar mirah-complete-mode-hook nil)
;;define minor-mode
(define-minor-mode mirah-complete-mode
  "AutoMirahComplete mode"
  :lighter " amc"
  ;;  :keymap amc-mode-map
  :group 'mirah-complete
  (if mirah-complete-mode
      (when (featurep 'auto-complete)
        (unless auto-complete-mode (auto-complete-mode))
        (mirah-complete-init))
    (mirah-complete-exit)))

;; ;; (add-hook 'find-file-hook 'amc-4-jsp-find-file-hook)
;; (defun amc-4-jsp-find-file-hook ()
;;   (let ((file-name-ext (file-name-extension (buffer-file-name)) ))
;;     (when (and file-name-ext (string-match "jsp" file-name-ext))
;;     (mirah-complete-mode))
;;   ))

(provide 'mirah-complete-config)

