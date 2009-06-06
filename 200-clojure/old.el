;; (setq swank-clojure-binary "clj-cmd")
;; (defvar clj-cmd)
;; (setenv "CLJ_CMD" 
;; 	(setq clj-cmd
;; 	      (concat "java "
;; 		      "-server "
;; 		      "-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8888 "
;; 		      "-cp "
;; 		      (concat clj-root "clojure/clojure.jar:")
;; 		      (concat clj-root "src/*:")
;; 		      (concat (expand-file-name "~") "/.clojure:")
;; 		      (concat clj-root "clojure-contrib/clojure-contrib.jar:")
;; 		      ;(concat clj-root "src/programming-clojure/code:")
;; 		      " clojure.lang.Repl")))

;; (eval-after-load "slime"
;;   '(progn
;;      (slime-setup '(slime-repl))
;;      (setq slime-lisp-implementations
;; 	   `((clojure ("clj-cmd") :init swank-clojure-init)
;; 	     ,@slime-lisp-implementations))))

;; (defun check-region-parens () 
;;   "Check if parentheses in the region are balanced. Signals a 
;; scan-error if not." 
;;   (interactive) 
;;   (save-restriction 
;;     (save-excursion 
;;     (let ((deactivate-mark nil)) 
;;       (condition-case c 
;;           (progn 
;;             (narrow-to-region (region-beginning) (region-end)) 
;;             (goto-char (point-min)) 
;;             (while (/= 0 (- (point) 
;;                             (forward-list)))) 
;;             t) 
;;         (scan-error (signal 'scan-error '("Region parentheses not balanced")))))))) 

;; (defun paredit-backward-maybe-delete-region () 
;;   (interactive) 
;;   (if mark-active 
;;       (progn 
;;         (check-region-parens) 
;;         (cua-delete-region)) 
;;     (paredit-backward-delete))) 

;; (defun paredit-forward-maybe-delete-region () 
;;   (interactive) 
;;   (if mark-active 
;;       (progn 
;;         (check-region-parens) 
;;         (cua-delete-region)) 
;;     (paredit-forward-delete))) 

;; (eval-after-load 'paredit
;;   '(progn
;;      (define-key paredit-mode-map (kbd "<delete>") 'paredit-forward-maybe-delete-region)
;;      (define-key paredit-mode-map (kbd "DEL") 'paredit-backward-maybe-delete-region)
;;      (define-key paredit-mode-map (kbd ";")   'self-insert-command)))













(defun slime-java-describe (symbol-name)
  "Get details on Java class/instance at point."
  (interactive (list (slime-read-symbol-name "Java Class/instance: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (save-excursion
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(clojure.contrib.repl-utils/show " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1))))

(defun slime-javadoc (symbol-name)
  "Get JavaDoc documentation on Java class at point."
  (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max))
  (insert (concat "(clojure.contrib.javadoc/javadoc " symbol-name ")"))
  (when symbol-name
    (slime-repl-return)
    (other-window 1)))

(setq slime-browse-local-javadoc-root (concat (expand-file-name "~") "/lisp/docs/" "java"))

(defun slime-browse-local-javadoc (ci-name)
  "Browse local JavaDoc documentation on Java class/Interface at point."
  (interactive (list (slime-read-symbol-name "Class/Interface name: ")))
  (when (not ci-name)
    (error "No name given"))
  (let ((name (replace-regexp-in-string "\\$" "." ci-name))
	(path (concat (expand-file-name slime-browse-local-javadoc-root) "/docs/api/")))
    (with-temp-buffer
      (insert-file-contents (concat path "allclasses-noframe.html"))
      (let ((l (delq nil
		     (mapcar #'(lambda (rgx)
				 (let* ((r (concat "\\.?\\(" rgx "[^./]+\\)[^.]*\\.?$"))
					(n (if (string-match r name)
					       (match-string 1 name)
					     name)))
				   (if (re-search-forward (concat "<A HREF=\"\\(.+\\)\" +.*>" n "<.*/A>") nil t)
				       (match-string 1)
				     nil)))
			     '("[^.]+\\." "")))))
	(if l
	    (browse-url (concat "file://" path (car l)))
	  (error (concat "Not found: " ci-name)))))))

(global-set-key [f5] 'slime)
(global-set-key [(control f11)] 'slime-selector)

;(require 'clojure-paredit)
(require 'swank-clojure-autoload)

(add-hook 'slime-connected-hook (lambda ()
				  (require 'clojure-mode)
				  (slime-redirect-inferior-output)
				  (def-slime-selector-method ?j
				    "most recently visited clojure-mode buffer."
				    (slime-recently-visited-buffer 'clojure-mode))
				  (define-key slime-mode-map (kbd "<return>") 'newline-and-indent)
				  (define-key slime-mode-map (kbd "C-j") 'newline)
				  (define-key slime-mode-map (kbd "C-c b") 'slime-browse-local-javadoc)
				  (define-key slime-repl-mode-map (kbd "C-c b") 'slime-browse-local-javadoc)
				  (define-key slime-mode-map (kbd "C-c d") 'slime-java-describe)
				  (define-key slime-repl-mode-map (kbd "C-c d") 'slime-java-describe)
				  (define-key slime-mode-map (kbd "C-c D") 'slime-javadoc)
				  (define-key slime-repl-mode-map (kbd "C-c D") 'slime-javadoc)))