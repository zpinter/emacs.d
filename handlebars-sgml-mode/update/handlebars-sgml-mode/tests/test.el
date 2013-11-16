(require 'handlebars-sgml-mode)

(defun is-comment ()
  (goto-char (point-min))
  (search-forward "this is a comment")
  (setq text-props (text-properties-at (point)))
  (and (memq 'syntax-table text-props) (member '(2) text-props)))

(defun assert-comment ()
  (let ((text-props ))
    (unless (is-comment)
      (error "%s should be a comment" text-props))))

(defun refute-comment ()
  (let ((text-props))
    (if (is-comment)
        (error "%s should not be a comment" text-props))))

(defun assert-handlebars (msg)
  (find-file "on-test.html")
  (indent-region (point-min) (point-max))
  (if (buffer-modified-p)
      (error "%s: on-test.html file indented incorrectly" msg))
  (assert-comment)
  (message "PASS. %s" msg)
  (kill-buffer))

(defun refute-handlebars (msg)
  (find-file "off-test.html")
  (indent-region (point-min) (point-max))
  (if (buffer-modified-p)
      (error "%s: off-test.html file indented incorrectly" msg))
  (refute-comment)
  (message "PASS. %s" msg)
  (kill-buffer))

;; =========== run tests =============
(defun run-tests ()

  (refute-handlebars "std sgml-mode")


  ;; ====== global handlebars ==========
  (handlebars-use-mode 'global)
  (assert-handlebars "global")

  ;; ===== conditional sgml-mode =======
  (handlebars-use-mode 'minor)

  (refute-handlebars "not in minor mode")

  (add-hook 'html-mode-hook 'handlebars-sgml-minor-mode)
  (assert-handlebars "minor mode")

  (handlebars-use-mode nil)
  (refute-handlebars "off")

  ;; === Compile errors and warnings ===
  (switch-to-buffer "*Compile-Log*")

  (goto-char (point-min))

  (and (search-forward-regexp ":\\(Warning\\|Error\\):" nil t) t))
