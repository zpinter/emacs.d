(when (not (iswindows))

(zconfig-add-lisp-path-end "flim-1.14.9")

(setq w3m-command (executable-find "w3m"))

(require 'w3m-load)
(require 'mime-w3m)

;; prevent w3m from requiring a prefix for following links
(setq w3m-safe-url-regexp nil)
(setq mm-w3m-safe-url-regexp nil)

;; (setq w3m-imitate-widget-button nil)

(defun wicked/w3m-open-current-page-in-firefox ()
  "Open the current URL in in browser function (usually firefox)."
  (interactive)
  (browse-url-default-macosx-browser w3m-current-url))

(defun wicked/w3m-open-link-or-image-in-firefox ()
  "Open the current link or image in browser function (usually firefox)."
  (interactive)
  (browse-url-default-macosx-browser (or (w3m-anchor)
                                         (w3m-image))))


(defun zpinter-w3m-keys ()
  ;; (define-key w3m-mode-map [(button2)] 'wicked/w3m-open-link-or-image-in-firefox)
  ;; (define-key w3m-mode-map [mouse-2] 'wicked/w3m-open-link-or-image-in-firefox)
  (define-key w3m-mode-map "f" 'wicked/w3m-open-current-page-in-firefox)
  (define-key w3m-mode-map "F" 'wicked/w3m-open-link-or-image-in-firefox))

(add-hook 'w3m-mode-hook 'zpinter-w3m-keys)

(setq w3m-goto-article-function 'browse-url-default-macosx-browser)

(defun zpinter-w3m-rename-buffer (url)
  "base buffer name on title"
  (let* ((size 32)
         (title w3m-current-title)
         (name (truncate-string-to-width
                (replace-regexp-in-string " " "_" title)
                size)))
    (rename-buffer name t)))

(add-hook 'w3m-display-hook 'zpinter-w3m-rename-buffer)

(defadvice w3m-modeline-title (around my-w3m-modeline-title)
  "prevent original function from running; cleanup remnants"
  (setq w3m-modeline-separator ""
        w3m-modeline-title-string ""))
(ad-activate 'w3m-modeline-title)

)
