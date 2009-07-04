(require 'actionscript-mode)
(load "ani-fcsh.el")

(defconst mumamo-actionscript-tag-start-regex
  (rx "<mx:Script>"
      ;; FIX-ME: Commented out because of bug in Emacs
      ;;
      ;;(optional (0+ space) "<![CDATA[" )
      ))

(defun mumamo-search-bw-exc-start-inlined-actionscript (pos min)
  "Helper for `mumamo-chunk-inlined-actionscript'.
POS is where to start search and MIN is where to stop."
  (goto-char (+ pos 10))
  (let ((marker-start (search-backward "<mx:Script" min t))
        exc-mode
        exc-start)
    (when marker-start
      (when (looking-at mumamo-actionscript-tag-start-regex)
        (setq exc-start (match-end 0))
        (goto-char exc-start)
        (when (<= exc-start pos)
          ;;(cons (point) 'javascript-mode)
          (list (point) 'actionscript-mode '(nxml-mode))
          )
        ))))

(defun mumamo-search-bw-exc-end-inlined-actionscript (pos min)
  "Helper for `mumamo-chunk-inlined-actionscript'.
POS is where to start search and MIN is where to stop."
  (mumamo-chunk-end-bw-str pos min "</mx:Script>"))

(defun mumamo-search-fw-exc-start-inlined-actionscript (pos max)
  "Helper for `mumamo-chunk-inlined-actionscript'.
POS is where to start search and MAX is where to stop."
  (goto-char (1+ pos))
  (skip-chars-backward "^<")
  ;; Handle <![CDATA[
  (when (and
         (eq ?< (char-before))
         (eq ?! (char-after))
         (not (bobp)))
    (backward-char)
    (skip-chars-backward "^<"))
  (unless (bobp)
    (backward-char 1))
  (let ((exc-start (search-forward "<mx:Script" max t))
        exc-mode)
    (when exc-start
      (goto-char (- exc-start 10))
      (when (looking-at mumamo-actionscript-tag-start-regex)
        (goto-char (match-end 0))
        (point)
        ))))

(defun mumamo-search-fw-exc-end-inlined-actionscript (pos max)
  "Helper for `mumamo-chunk-inlined-actionscript'.
POS is where to start search and MAX is where to stop."
  (save-match-data
    (mumamo-chunk-end-fw-str pos max "</mx:Script>")))

(defun mumamo-chunk-inlined-actionscript (pos min max)
  "Find <script>...</script>.  Return range and 'javascript-mode.
See `mumamo-find-possible-chunk' for POS, MIN and MAX."
  (mumamo-find-possible-chunk pos min max
                              'mumamo-search-bw-exc-start-inlined-actionscript
                              'mumamo-search-bw-exc-end-inlined-actionscript
                              'mumamo-search-fw-exc-start-inlined-actionscript
                              'mumamo-search-fw-exc-end-inlined-actionscript))


(define-mumamo-multi-major-mode mxml-actionscript-mumamo-mode
    "Turn on multiple major modes for MXML with main mode `nxml-mode'.
This covers inlined style and script for mxml."
  ("NXML Family" nxml-mode
                 (
                  mumamo-chunk-inlined-actionscript
                  )))

(add-to-list 'auto-mode-alist '("\\.mxml\\'" . mxml-actionscript-mumamo-mode))