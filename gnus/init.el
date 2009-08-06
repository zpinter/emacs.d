;; (zconfig-add-lisp-path "gnus-cvs/lisp")
;(require 'gnus-load)

(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(setq gnus-read-active-file nil)
(setq mail-user-agent 'gnus-user-agent)

(defun mm-substring-no-properties (file)
  file)


(require 'extract-ical)
(extract-ical-gnus-insinuate)

(setq gnus-ignored-newsgroups "")
(setq gnus-gcc-mark-as-read t)

;; (require 'external-abook)
;; (setq external-abook-command "contacts -lSf '%%e\t\"%%n\"' '%s'")

;; (eval-after-load "message"
;;   '(progn
;;     (add-to-list 'message-mode-hook
;;      '(lambda ()
;;        (define-key message-mode-map (kbd "M-/") 'external-abook-try-expand)))))

(setq gnus-buttonized-mime-types
      '("multipart/alternative" "multipart/signed"))

;; eye candy
(copy-face 'font-lock-variable-name-face 'gnus-face-6)
(setq gnus-face-6 'gnus-face-6)
(copy-face 'font-lock-constant-face 'gnus-face-7)
(setq gnus-face-7 'gnus-face-7)
(copy-face 'gnus-face-7 'gnus-summary-normal-unread)
(copy-face 'font-lock-constant-face 'gnus-face-8)
(set-face-foreground 'gnus-face-8 "gray50")
(setq gnus-face-8 'gnus-face-8)
(copy-face 'font-lock-constant-face 'gnus-face-9)
(set-face-foreground 'gnus-face-9 "gray70")
(setq gnus-face-9 'gnus-face-9)
(setq gnus-summary-make-false-root 'dummy)
(setq gnus-summary-make-false-root-always nil)
(defun oxy-unicode-threads ()
  (interactive)
  (setq gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{│%}%) %6{□%}  %S\n"
        gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{│%} %6{%B%} %s\n"
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root "■ "
        gnus-sum-thread-tree-false-root "□ "
        gnus-sum-thread-tree-single-indent "▣ "
        gnus-sum-thread-tree-leaf-with-other "├─▶ "
        gnus-sum-thread-tree-vertical "│"
        gnus-sum-thread-tree-single-leaf "└─▶ "))

(defun oxy-unicode-threads-heavy ()
  (interactive)
  (setq gnus-summary-line-format "%8{%4k│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %s\n"
        gnus-summary-dummy-line-format "    %8{│%}   %(%8{│%}                       %7{║%}%) %6{┏○%}  %S\n"
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root "┏● "
        gnus-sum-thread-tree-false-root " ○ "
        gnus-sum-thread-tree-single-indent "  ● "
        gnus-sum-thread-tree-leaf-with-other "┣━━❯ "
        gnus-sum-thread-tree-vertical "┃"
        gnus-sum-thread-tree-single-leaf "┗━━❯ "))

(oxy-unicode-threads-heavy)

;; window layout
(defun door-gnus ()
  "Switch between gnus and non-gnus buffers, preserving window configurations."
  (interactive)
  (let ((bufname (buffer-name)))
    (if (or
         (string-equal "*Group*" bufname)
         (string-equal "*BBDB*" bufname)
         (string-match "\*Summary" bufname)
         (string-match "\*mail" bufname)
         (string-match "\*wide" bufname)
         (string-match "\*reply" bufname)
         (string-match "\*Article" bufname))
        (progn
          (door-bury-gnus))
        (if (get-buffer "*Group*")
            (door-unbury-gnus)
            (progn
              (setq gnus-unbury-window-configuration (current-window-configuration))
              (delete-other-windows)
              (gnus))))))

(defun door-unbury-gnus ()
  (interactive)
  (setq gnus-unbury-window-configuration nil)
  (let ((buf nil)
        (bufname nil))
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (when (and (boundp 'gnus-bury-window-configuration) gnus-bury-window-configuration)
        (unless gnus-unbury-window-configuration
          (setq gnus-unbury-window-configuration (current-window-configuration)))
        (delete-other-windows)
        (if (eq (current-buffer) buf)
            (bury-buffer)
            (bury-buffer buf))
        (set-window-configuration gnus-bury-window-configuration)))))

(defun door-bury-gnus ()
  (interactive)
  (setq gnus-bury-window-configuration nil)
  (let ((buf nil)
        (bufname nil))
    (dolist (buf (buffer-list))
      (setq bufname (buffer-name buf))
      (when (or
             (string-equal "*Group*" bufname)
             (string-equal "*BBDB*" bufname)
             (string-match "\*Summary" bufname)
             (string-match "\*mail" bufname)
             (string-match "\*reply" bufname)
             (string-match "\*wide" bufname)
             (string-match "\*Article" bufname))
        (unless gnus-bury-window-configuration
          (setq gnus-bury-window-configuration (current-window-configuration)))
        (delete-other-windows)
        (if (eq (current-buffer) buf)
            (bury-buffer)
            (bury-buffer buf))
        (set-window-configuration gnus-unbury-window-configuration)))))

(global-set-key [(meta f7)] 'door-gnus)


;;scoring
(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (from 4))
        (gnus-dormant-mark (from 5))
        (gnus-del-mark (subject -4))
        (gnus-read-mark (from 1) (subject 2))
        (gnus-expirable-mark (from 0) (subject -1))
        (gnus-killed-mark (from -1) (subject -3))
        (gnus-kill-file-mark)
        (gnus-ancient-mark)
        (gnus-low-score-mark)
        (gnus-catchup-mark (subject -4) (from -1))
        ))

;;expiry
(defun zpinter-get-expiry-target (group)
  (if (not (string-match "^mail\\." group))
      'delete
      (re-search-forward "Date:.*?\\(\\w+\\) 20\\(0.\\)")
      (concat "archive." group "." (match-string 1) (match-string 2))))

(setq nnmail-expiry-target 'zpinter-get-expiry-target)
(setq nnmail-expiry-wait 30)

;; fixes bug: "Lisp nesting exceeds max-lisp-eval-depth"
(setq max-lisp-eval-depth 1024)