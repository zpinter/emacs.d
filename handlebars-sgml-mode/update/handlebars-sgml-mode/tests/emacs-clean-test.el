:;exec emacs -nw -Q -l "$0" -f main "$@"
(require 'cl)
(toggle-debug-on-error)

(defconst test-file (expand-file-name "./test.el"))

(defun main ()
  (interactive)
  (destructuring-bind (package elpa-parent) command-line-args-left
    ;; Make the elpa dir for this if we need to.
    (setq package-user-dir
          (concat elpa-parent "/.elpa"))
    (setq package-archives
          '(("gnu" . "http://elpa.gnu.org/packages/")
            ("marmalade" . "http://marmalade-repo.org/packages/")))
    (when (not (file-exists-p elpa-parent))
      (make-directory elpa-parent t)
      ;; Package stuff
      (package-initialize)
      (package-refresh-contents))
    (let ((pkg-path (expand-file-name package)))
      (if (and (file-exists-p pkg-path)
               (not (file-directory-p pkg-path)))
          (progn
            (package-install-file pkg-path)
            (when (file-exists-p test-file)
              (load test-file)
              (message "\n\nrunning tests: '%s'.\n" test-file)
              (if (run-tests)
                (with-current-buffer "*Messages*"
                  (message "\n\ntests Failed!!!.\n\n")
                  (write-file "test-results.txt"))

                (with-current-buffer "*Messages*"
                  (message "\n\ntests successful.\n")
                  (write-file "test-results.txt")
                  (kill-emacs)
                  ))))
        ;; Else must just be a package
        (package-install (intern package))))))

;; End
