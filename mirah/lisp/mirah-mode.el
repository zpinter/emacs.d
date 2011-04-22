;;;
;;;  mirah-mode.el -
;;;
;;;  $Author: akr $
;;;  created at: Fri Feb  4 14:49:13 JST 1994
;;;

(defconst mirah-mode-revision "$Revision: 14912 $")

(defconst mirah-mode-version
  (progn
   (string-match "[0-9.]+" mirah-mode-revision)
   (substring mirah-mode-revision (match-beginning 0) (match-end 0))))

(defconst mirah-block-beg-re
  "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do"
  )

(defconst mirah-non-block-do-re
  "\\(while\\|until\\|for\\|rescue\\)\\>[^_]"
  )

(defconst mirah-indent-beg-re
  "\\(\\s *\\(class\\|module\\|def\\)\\)\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin"
    )

(defconst mirah-modifier-beg-re
  "if\\|unless\\|while\\|until"
  )

(defconst mirah-modifier-re
  (concat mirah-modifier-beg-re "\\|rescue")
  )

(defconst mirah-block-mid-re
  "then\\|else\\|elsif\\|when\\|rescue\\|ensure"
  )

(defconst mirah-block-op-re
  "and\\|or\\|not"
  )

(defconst mirah-block-hanging-re
  (concat mirah-modifier-beg-re "\\|" mirah-block-op-re)
  )

(defconst mirah-block-end-re "\\<end\\>")

(defconst mirah-here-doc-beg-re
  "<<\\(-\\)?\\(\\([a-zA-Z0-9_]+\\)\\|[\"]\\([^\"]+\\)[\"]\\|[']\\([^']+\\)[']\\)")

(defun mirah-here-doc-end-match ()
  (concat "^"
	  (if (match-string 1) "[ \t]*" nil)
	  (regexp-quote
	   (or (match-string 3)
	       (match-string 4)
	       (match-string 5)))))

(defconst mirah-delimiter
  (concat "[?$/%(){}#\"'`.:]\\|<<\\|\\[\\|\\]\\|\\<\\("
	  mirah-block-beg-re
	  "\\)\\>\\|" mirah-block-end-re
	  "\\|^=begin\\|" mirah-here-doc-beg-re)
  )

(defconst mirah-negative
  (concat "^[ \t]*\\(\\(" mirah-block-mid-re "\\)\\>\\|"
	    mirah-block-end-re "\\|}\\|\\]\\)")
  )

(defconst mirah-operator-chars "-,.+*/%&|^~=<>:")
(defconst mirah-operator-re (concat "[" mirah-operator-chars "]"))

(defconst mirah-symbol-chars "a-zA-Z0-9_")
(defconst mirah-symbol-re (concat "[" mirah-symbol-chars "]"))

(defvar mirah-mode-abbrev-table nil
  "Abbrev table in use in mirah-mode buffers.")

(define-abbrev-table 'mirah-mode-abbrev-table ())

(defvar mirah-mode-map nil "Keymap used in mirah mode.")

(if mirah-mode-map
    nil
  (setq mirah-mode-map (make-sparse-keymap))
  (define-key mirah-mode-map "{" 'mirah-electric-brace)
  (define-key mirah-mode-map "}" 'mirah-electric-brace)
  (define-key mirah-mode-map "\e\C-a" 'mirah-beginning-of-defun)
  (define-key mirah-mode-map "\e\C-e" 'mirah-end-of-defun)
  (define-key mirah-mode-map "\e\C-b" 'mirah-backward-sexp)
  (define-key mirah-mode-map "\e\C-f" 'mirah-forward-sexp)
  (define-key mirah-mode-map "\e\C-p" 'mirah-beginning-of-block)
  (define-key mirah-mode-map "\e\C-n" 'mirah-end-of-block)
  (define-key mirah-mode-map "\e\C-h" 'mirah-mark-defun)
  (define-key mirah-mode-map "\e\C-q" 'mirah-indent-exp)
  (define-key mirah-mode-map "\t" 'mirah-indent-command)
  (define-key mirah-mode-map "\C-c\C-e" 'mirah-insert-end)
  (define-key mirah-mode-map "\C-j" 'mirah-reindent-then-newline-and-indent)
  (define-key mirah-mode-map "\C-m" 'newline))

(defvar mirah-mode-syntax-table nil
  "Syntax table in use in mirah-mode buffers.")

(if mirah-mode-syntax-table
    ()
  (setq mirah-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\"" mirah-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" mirah-mode-syntax-table)
  (modify-syntax-entry ?\` "\"" mirah-mode-syntax-table)
  (modify-syntax-entry ?# "<" mirah-mode-syntax-table)
  (modify-syntax-entry ?\n ">" mirah-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" mirah-mode-syntax-table)
  (modify-syntax-entry ?$ "." mirah-mode-syntax-table)
  (modify-syntax-entry ?? "_" mirah-mode-syntax-table)
  (modify-syntax-entry ?_ "_" mirah-mode-syntax-table)
  (modify-syntax-entry ?< "." mirah-mode-syntax-table)
  (modify-syntax-entry ?> "." mirah-mode-syntax-table)
  (modify-syntax-entry ?& "." mirah-mode-syntax-table)
  (modify-syntax-entry ?| "." mirah-mode-syntax-table)
  (modify-syntax-entry ?% "." mirah-mode-syntax-table)
  (modify-syntax-entry ?= "." mirah-mode-syntax-table)
  (modify-syntax-entry ?/ "." mirah-mode-syntax-table)
  (modify-syntax-entry ?+ "." mirah-mode-syntax-table)
  (modify-syntax-entry ?* "." mirah-mode-syntax-table)
  (modify-syntax-entry ?- "." mirah-mode-syntax-table)
  (modify-syntax-entry ?\; "." mirah-mode-syntax-table)
  (modify-syntax-entry ?\( "()" mirah-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" mirah-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" mirah-mode-syntax-table)
  (modify-syntax-entry ?\} "){" mirah-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" mirah-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" mirah-mode-syntax-table)
  )

(defcustom mirah-indent-tabs-mode nil
  "*Indentation can insert tabs in mirah mode if this is non-nil."
  :type 'boolean :group 'mirah)

(defcustom mirah-indent-level 2
  "*Indentation of mirah statements."
  :type 'integer :group 'mirah)

(defcustom mirah-comment-column 32
  "*Indentation column of comments."
  :type 'integer :group 'mirah)

(defcustom mirah-deep-arglist t
  "*Deep indent lists in parenthesis when non-nil.
Also ignores spaces after parenthesis when 'space."
  :group 'mirah)

(defcustom mirah-deep-indent-paren '(?\( ?\[ ?\] t)
  "*Deep indent lists in parenthesis when non-nil. t means continuous line.
Also ignores spaces after parenthesis when 'space."
  :group 'mirah)

(defcustom mirah-deep-indent-paren-style 'space
  "Default deep indent style."
  :options '(t nil space) :group 'mirah)

(eval-when-compile (require 'cl))
(defun mirah-imenu-create-index-in-block (prefix beg end)
  (let ((index-alist '()) (case-fold-search nil)
	name next pos decl sing)
    (goto-char beg)
    (while (re-search-forward "^\\s *\\(\\(class\\>\\(\\s *<<\\)?\\|module\\>\\)\\s *\\([^\(<\n ]+\\)\\|\\(def\\|alias\\)\\>\\s *\\([^\(\n ]+\\)\\)" end t) ;;"
      (setq sing (match-beginning 3))
      (setq decl (match-string 5))
      (setq next (match-end 0))
      (setq name (or (match-string 4) (match-string 6)))
      (setq pos (match-beginning 0))
      (cond
       ((string= "alias" decl)
	(if prefix (setq name (concat prefix name)))
	(push (cons name pos) index-alist))
       ((string= "def" decl)
	(if prefix
	    (setq name
		  (cond
		   ((string-match "^self\." name)
		    (concat (substring prefix 0 -1) (substring name 4)))
		  (t (concat prefix name)))))
	(push (cons name pos) index-alist)
	(mirah-accurate-end-of-block end))
       (t
	(if (string= "self" name)
	    (if prefix (setq name (substring prefix 0 -1)))
	  (if prefix (setq name (concat (substring prefix 0 -1) "::" name)))
	  (push (cons name pos) index-alist))
	(mirah-accurate-end-of-block end)
	(setq beg (point))
	(setq index-alist
	      (nconc (mirah-imenu-create-index-in-block
		      (concat name (if sing "." "#"))
		      next beg) index-alist))
	(goto-char beg))))
    index-alist))

(defun mirah-imenu-create-index ()
  (nreverse (mirah-imenu-create-index-in-block nil (point-min) nil)))

(defun mirah-accurate-end-of-block (&optional end)
  (let (state)
    (or end (setq end (point-max)))
    (while (and (setq state (apply 'mirah-parse-partial end state))
		(>= (nth 2 state) 0) (< (point) end)))))

(defun mirah-mode-variables ()
  (set-syntax-table mirah-mode-syntax-table)
  (setq local-abbrev-table mirah-mode-abbrev-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'mirah-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-variable-buffer-local 'comment-start)
  (setq comment-start "# ")
  (make-variable-buffer-local 'comment-end)
  (setq comment-end "")
  (make-variable-buffer-local 'comment-column)
  (setq comment-column mirah-comment-column)
  (make-variable-buffer-local 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (setq indent-tabs-mode mirah-indent-tabs-mode)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t))

;;;###autoload
(defun mirah-mode ()
  "Major mode for editing mirah scripts.
\\[mirah-indent-command] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable mirah-indent-level controls the amount of indentation.
\\{mirah-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mirah-mode-map)
  (setq mode-name "Mirah")
  (setq major-mode 'mirah-mode)
  (mirah-mode-variables)

  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'mirah-imenu-create-index)

  (make-local-variable 'add-log-current-defun-function)
  (setq add-log-current-defun-function 'mirah-add-log-current-method)

  (run-hooks 'mirah-mode-hook))

(defun mirah-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (back-to-indentation)
    (current-column)))

(defun mirah-indent-line (&optional flag)
  "Correct indentation of the current mirah line."
  (mirah-indent-to (mirah-calculate-indent)))

(defun mirah-indent-command ()
  (interactive)
  (mirah-indent-line t))

(defun mirah-indent-to (x)
  (if x
      (let (shift top beg)
	(and (< x 0) (error "invalid nest"))
	(setq shift (current-column))
	(beginning-of-line)
	(setq beg (point))
	(back-to-indentation)
	(setq top (current-column))
	(skip-chars-backward " \t")
	(if (>= shift top) (setq shift (- shift top))
	  (setq shift 0))
	(if (and (bolp)
		 (= x top))
	    (move-to-column (+ x shift))
	  (move-to-column top)
	  (delete-region beg (point))
	  (beginning-of-line)
	  (indent-to x)
	  (move-to-column (+ x shift))))))

(defun mirah-special-char-p (&optional pnt)
  (setq pnt (or pnt (point)))
  (let ((c (char-before pnt)) (b (and (< (point-min) pnt) (char-before (1- pnt)))))
    (cond ((or (eq c ??) (eq c ?$)))
	  ((and (eq c ?:) (or (not b) (eq (char-syntax b) ? ))))
	  ((eq c ?\\) (eq b ??)))))

(defun mirah-expr-beg (&optional option)
  (save-excursion
    (store-match-data nil)
    (let ((space (skip-chars-backward " \t"))
	  (start (point)))
      (cond
       ((bolp) t)
       ((progn
	  (forward-char -1)
	  (and (looking-at "\\?")
	       (or (eq (char-syntax (char-before (point))) ?w)
		   (mirah-special-char-p))))
	nil)
       ((and (eq option 'heredoc) (< space 0)) t)
       ((or (looking-at mirah-operator-re)
	    (looking-at "[\\[({,;]")
	    (and (looking-at "[!?]")
		 (or (not (eq option 'modifier))
		     (bolp)
		     (save-excursion (forward-char -1) (looking-at "\\Sw$"))))
	    (and (looking-at mirah-symbol-re)
		 (skip-chars-backward mirah-symbol-chars)
		 (cond
		  ((or (looking-at (concat "\\<\\(" mirah-block-beg-re
					   "|" mirah-block-op-re
					   "|" mirah-block-mid-re "\\)\\>")))
		   (goto-char (match-end 0))
		   (not (looking-at "\\s_")))
		  ((eq option 'expr-qstr)
		   (looking-at "[a-zA-Z][a-zA-z0-9_]* +%[^ \t]"))
		  ((eq option 'expr-re)
		   (looking-at "[a-zA-Z][a-zA-z0-9_]* +/[^ \t]"))
		  (t nil)))))))))

(defun mirah-forward-string (term &optional end no-error expand)
  (let ((n 1) (c (string-to-char term))
	(re (if expand
		(concat "[^\\]\\(\\\\\\\\\\)*\\([" term "]\\|\\(#{\\)\\)")
	      (concat "[^\\]\\(\\\\\\\\\\)*[" term "]"))))
    (while (and (re-search-forward re end no-error)
		(if (match-beginning 3)
		    (mirah-forward-string "}{" end no-error nil)
		  (> (setq n (if (eq (char-before (point)) c)
				     (1- n) (1+ n))) 0)))
      (forward-char -1))
    (cond ((zerop n))
	  (no-error nil)
	  ((error "unterminated string")))))

(defun mirah-deep-indent-paren-p (c)
  (cond ((listp mirah-deep-indent-paren)
	 (let ((deep (assoc c mirah-deep-indent-paren)))
	   (cond (deep
		  (or (cdr deep) mirah-deep-indent-paren-style))
		 ((memq c mirah-deep-indent-paren)
		  mirah-deep-indent-paren-style))))
	((eq c mirah-deep-indent-paren) mirah-deep-indent-paren-style)
	((eq c ?\( ) mirah-deep-arglist)))

(defun mirah-parse-partial (&optional end in-string nest depth pcol indent)
  (or depth (setq depth 0))
  (or indent (setq indent 0))
  (when (re-search-forward mirah-delimiter end 'move)
    (let ((pnt (point)) w re expand)
      (goto-char (match-beginning 0))
      (cond
       ((and (memq (char-before) '(?@ ?$)) (looking-at "\\sw"))
	(goto-char pnt))
       ((looking-at "[\"`]")		;skip string
	(cond
	 ((and (not (eobp))
	       (mirah-forward-string (buffer-substring (point) (1+ (point))) end t t))
	  nil)
	 (t
	  (setq in-string (point))
	  (goto-char end))))
       ((looking-at "'")
	(cond
	 ((and (not (eobp))
	       (re-search-forward "[^\\]\\(\\\\\\\\\\)*'" end t))
	  nil)
	 (t
	  (setq in-string (point))
	  (goto-char end))))
       ((looking-at "/=") 
	(goto-char pnt))
       ((looking-at "/")
	(cond
	 ((and (not (eobp)) (mirah-expr-beg 'expr-re))
	  (if (mirah-forward-string "/" end t t)
	      nil
	    (setq in-string (point))
	    (goto-char end)))
	 (t
	  (goto-char pnt))))
       ((looking-at "%")
	(cond
	 ((and (not (eobp))
	       (mirah-expr-beg 'expr-qstr)
	       (not (looking-at "%="))
	       (looking-at "%[QqrxWw]?\\([^a-zA-Z0-9 \t\n]\\)"))
	  (goto-char (match-beginning 1))
	  (setq expand (not (memq (char-before) '(?q ?w))))
	  (setq w (match-string 1))
	  (cond
	   ((string= w "[") (setq re "]["))
	   ((string= w "{") (setq re "}{"))
	   ((string= w "(") (setq re ")("))
	   ((string= w "<") (setq re "><"))
	   ((and expand (string= w "\\"))
	    (setq w (concat "\\" w))))
	  (unless (cond (re (mirah-forward-string re end t expand))
			(expand (mirah-forward-string w end t t))
			(t (re-search-forward
			    (if (string= w "\\")
				"\\\\[^\\]*\\\\"
			      (concat "[^\\]\\(\\\\\\\\\\)*" w))
			    end t)))
	    (setq in-string (point))
	    (goto-char end)))
	 (t
	  (goto-char pnt))))
       ((looking-at "\\?")		;skip ?char
	(cond
	 ((and (mirah-expr-beg)
	       (looking-at "?\\(\\\\C-\\|\\\\M-\\)*\\\\?."))
	  (goto-char (match-end 0)))
	 (t
	  (goto-char pnt))))
       ((looking-at "\\$")		;skip $char
	(goto-char pnt)
	(forward-char 1))
       ((looking-at "#")		;skip comment
	(forward-line 1)
	(goto-char (point))
	)
       ((looking-at "[\\[{(]")
	(let ((deep (mirah-deep-indent-paren-p (char-after))))
	  (if (and deep (or (not (eq (char-after) ?\{)) (mirah-expr-beg)))
	      (progn
		(and (eq deep 'space) (looking-at ".\\s +[^# \t\n]")
		     (setq pnt (1- (match-end 0))))
		(setq nest (cons (cons (char-after (point)) pnt) nest))
		(setq pcol (cons (cons pnt depth) pcol))
		(setq depth 0))
	    (setq nest (cons (cons (char-after (point)) pnt) nest))
	    (setq depth (1+ depth))))
	(goto-char pnt)
	)
       ((looking-at "[])}]")
	(if (mirah-deep-indent-paren-p (matching-paren (char-after)))
	    (setq depth (cdr (car pcol)) pcol (cdr pcol))
	  (setq depth (1- depth)))
	(setq nest (cdr nest))
	(goto-char pnt))
       ((looking-at mirah-block-end-re)
	(if (or (and (not (bolp))
		     (progn
		       (forward-char -1)
		       (setq w (char-after (point)))
		       (or (eq ?_ w)
			   (eq ?. w))))
		(progn
		  (goto-char pnt)
		  (setq w (char-after (point)))
		  (or (eq ?_ w)
		      (eq ?! w)
		      (eq ?? w))))
	    nil
	  (setq nest (cdr nest))
	  (setq depth (1- depth)))
	(goto-char pnt))
       ((looking-at "def\\s +[^(\n;]*")
	(if (or (bolp)
		(progn
		  (forward-char -1)
		  (not (eq ?_ (char-after (point))))))
	    (progn
	      (setq nest (cons (cons nil pnt) nest))
	      (setq depth (1+ depth))))
	(goto-char (match-end 0)))
       ((looking-at (concat "\\<\\(" mirah-block-beg-re "\\)\\>"))
	(and
	 (save-match-data
	   (or (not (looking-at "do\\>[^_]"))
	       (save-excursion
		 (back-to-indentation)
		 (not (looking-at mirah-non-block-do-re)))))
	 (or (bolp)
	     (progn
	       (forward-char -1)
	       (setq w (char-after (point)))
	       (not (or (eq ?_ w)
			(eq ?. w)))))
	 (goto-char pnt)
	 (setq w (char-after (point)))
	 (not (eq ?_ w))
	 (not (eq ?! w))
	 (not (eq ?? w))
	 (skip-chars-forward " \t")
	 (goto-char (match-beginning 0))
	 (or (not (looking-at mirah-modifier-re))
	     (mirah-expr-beg 'modifier))
	 (goto-char pnt)
	 (setq nest (cons (cons nil pnt) nest))
	 (setq depth (1+ depth)))
	(goto-char pnt))
       ((looking-at ":\\(['\"]\\)")
	(goto-char (match-beginning 1))
	(mirah-forward-string (buffer-substring (match-beginning 1) (match-end 1)) end))
       ((looking-at ":\\([-,.+*/%&|^~<>]=?\\|===?\\|<=>\\)")
	(goto-char (match-end 0)))
       ((looking-at ":\\([a-zA-Z_][a-zA-Z_0-9]*[!?=]?\\)?")
	(goto-char (match-end 0)))
       ((or (looking-at "\\.\\.\\.?")
	    (looking-at "\\.[0-9]+")
	    (looking-at "\\.[a-zA-Z_0-9]+")
	    (looking-at "\\."))
	(goto-char (match-end 0)))
       ((looking-at "^=begin")
	(if (re-search-forward "^=end" end t)
	    (forward-line 1)
	  (setq in-string (match-end 0))
	  (goto-char end)))
       ((looking-at "<<")
	(cond
	 ((and (mirah-expr-beg 'heredoc)
	       (looking-at "<<\\(-\\)?\\(\\([\"'`]\\)\\([^\n]+?\\)\\3\\|\\(?:\\sw\\|\\s_\\)+\\)"))
	  (setq re (regexp-quote (or (match-string 4) (match-string 2))))
	  (if (match-beginning 1) (setq re (concat "\\s *" re)))
	  (let* ((id-end (goto-char (match-end 0)))
		 (line-end-position (save-excursion (end-of-line) (point)))
		 (state (list in-string nest depth pcol indent)))
	    ;; parse the rest of the line
	    (while (and (> line-end-position (point))
			(setq state (apply 'mirah-parse-partial
					   line-end-position state))))
	    (setq in-string (car state)
		  nest (nth 1 state)
		  depth (nth 2 state)
		  pcol (nth 3 state)
		  indent (nth 4 state))
	    ;; skip heredoc section
	    (if (re-search-forward (concat "^" re "$") end 'move)
		(forward-line 1)
	      (setq in-string id-end)
	      (goto-char end))))
	 (t
	  (goto-char pnt))))
       ((looking-at "^__END__$")
	(goto-char pnt))
       ((looking-at mirah-here-doc-beg-re)
	(if (re-search-forward (mirah-here-doc-end-match)
			       indent-point t)
	    (forward-line 1)
	  (setq in-string (match-end 0))
	  (goto-char indent-point)))
       (t
	(error (format "bad string %s"
		       (buffer-substring (point) pnt)
		       ))))))
  (list in-string nest depth pcol))

(defun mirah-parse-region (start end)
  (let (state)
    (save-excursion
      (if start
	  (goto-char start)
	(mirah-beginning-of-indent))
      (save-restriction
	(narrow-to-region (point) end)
	(while (and (> end (point))
		    (setq state (apply 'mirah-parse-partial end state))))))
    (list (nth 0 state)			; in-string
	  (car (nth 1 state))		; nest
	  (nth 2 state)			; depth
	  (car (car (nth 3 state)))	; pcol
	  ;(car (nth 5 state))		; indent
	  )))

(defun mirah-indent-size (pos nest)
  (+ pos (* (or nest 1) mirah-indent-level)))

(defun mirah-calculate-indent (&optional parse-start)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state bol eol begin op-end
	  (paren (progn (skip-syntax-forward " ")
			(and (char-after) (matching-paren (char-after)))))
	  (indent 0))
      (if parse-start
	  (goto-char parse-start)
	(mirah-beginning-of-indent)
	(setq parse-start (point)))
      (back-to-indentation)
      (setq indent (current-column))
      (setq state (mirah-parse-region parse-start indent-point))
      (cond
       ((nth 0 state)			; within string
	(setq indent nil))		;  do nothing
       ((car (nth 1 state))		; in paren
	(goto-char (setq begin (cdr (nth 1 state))))
	(let ((deep (mirah-deep-indent-paren-p (car (nth 1 state)))))
	  (if deep
	      (cond ((and (eq deep t) (eq (car (nth 1 state)) paren))
		     (skip-syntax-backward " ")
		     (setq indent (1- (current-column))))
		    ((let ((s (mirah-parse-region (point) indent-point)))
		       (and (nth 2 s) (> (nth 2 s) 0)
			    (or (goto-char (cdr (nth 1 s))) t)))
		     (forward-word -1)
		     (setq indent (mirah-indent-size (current-column) (nth 2 state))))
		    (t
		     (setq indent (current-column))
		     (cond ((eq deep 'space))
			   (paren (setq indent (1- indent)))
			   (t (setq indent (mirah-indent-size (1- indent) 1))))))
	    (if (nth 3 state) (goto-char (nth 3 state))
	      (goto-char parse-start) (back-to-indentation))
	    (setq indent (mirah-indent-size (current-column) (nth 2 state))))
	  (and (eq (car (nth 1 state)) paren)
	       (mirah-deep-indent-paren-p (matching-paren paren))
	       (search-backward (char-to-string paren))
	       (setq indent (current-column)))))
       ((and (nth 2 state) (> (nth 2 state) 0)) ; in nest
	(if (null (cdr (nth 1 state)))
	    (error "invalid nest"))
	(goto-char (cdr (nth 1 state)))
	(forward-word -1)		; skip back a keyword
	(setq begin (point))
	(cond
	 ((looking-at "do\\>[^_]")	; iter block is a special case
	  (if (nth 3 state) (goto-char (nth 3 state))
	    (goto-char parse-start) (back-to-indentation))
	  (setq indent (mirah-indent-size (current-column) (nth 2 state))))
	 (t
	  (setq indent (+ (current-column) mirah-indent-level)))))
       
       ((and (nth 2 state) (< (nth 2 state) 0)) ; in negative nest
	(setq indent (mirah-indent-size (current-column) (nth 2 state)))))
      (when indent
	(goto-char indent-point)
	(end-of-line)
	(setq eol (point))
	(beginning-of-line)
	(cond
	 ((and (not (mirah-deep-indent-paren-p paren))
	       (re-search-forward mirah-negative eol t))
	  (and (not (eq ?_ (char-after (match-end 0))))
	       (setq indent (- indent mirah-indent-level))))
	 ((and
	   (save-excursion
	     (beginning-of-line)
	     (not (bobp)))
	   (or (mirah-deep-indent-paren-p t)
	       (null (car (nth 1 state)))))
	  ;; goto beginning of non-empty no-comment line
	  (let (end done)
	    (while (not done)
	      (skip-chars-backward " \t\n")
	      (setq end (point))
	      (beginning-of-line)
	      (if (re-search-forward "^\\s *#" end t)
		  (beginning-of-line)
		(setq done t))))
	  (setq bol (point))
	  (end-of-line)
	  ;; skip the comment at the end
	  (skip-chars-backward " \t")
	  (let (end (pos (point)))
	    (beginning-of-line)
	    (while (and (re-search-forward "#" pos t)
			(setq end (1- (point)))
			(or (mirah-special-char-p end)
			    (and (setq state (mirah-parse-region parse-start end))
				 (nth 0 state))))
	      (setq end nil))
	    (goto-char (or end pos))
	    (skip-chars-backward " \t")
	    (setq begin (if (nth 0 state) pos (cdr (nth 1 state))))
	    (setq state (mirah-parse-region parse-start (point))))
	  (or (bobp) (forward-char -1))
	  (and
	   (or (and (looking-at mirah-symbol-re)
		    (skip-chars-backward mirah-symbol-chars)
		    (looking-at (concat "\\<\\(" mirah-block-hanging-re "\\)\\>"))
		    (not (eq (point) (nth 3 state)))
		    (save-excursion
		      (goto-char (match-end 0))
		      (not (looking-at "[a-z_]"))))
	       (and (looking-at mirah-operator-re)
		    (not (mirah-special-char-p))
		    ;; operator at the end of line
		    (let ((c (char-after (point))))
		      (and
;; 		       (or (null begin)
;; 			   (save-excursion
;; 			     (goto-char begin)
;; 			     (skip-chars-forward " \t")
;; 			     (not (or (eolp) (looking-at "#")
;; 				      (and (eq (car (nth 1 state)) ?{)
;; 					   (looking-at "|"))))))
		       (or (not (eq ?/ c))
			   (null (nth 0 (mirah-parse-region (or begin parse-start) (point)))))
		       (or (not (eq ?| (char-after (point))))
			   (save-excursion
			     (or (eolp) (forward-char -1))
			     (cond
			      ((search-backward "|" nil t)
			       (skip-chars-backward " \t\n")
			       (and (not (eolp))
				    (progn
				      (forward-char -1)
				      (not (looking-at "{")))
				    (progn
				      (forward-word -1)
				      (not (looking-at "do\\>[^_]")))))
			      (t t))))
		       (not (eq ?, c))
		       (setq op-end t)))))
	   (setq indent
		 (cond
		  ((and
		    (null op-end)
		    (not (looking-at (concat "\\<\\(" mirah-block-hanging-re "\\)\\>")))
		    (eq (mirah-deep-indent-paren-p t) 'space)
		    (not (bobp)))
		   (widen)
		   (goto-char (or begin parse-start))
		   (skip-syntax-forward " ")
		   (current-column))
		  ((car (nth 1 state)) indent)
		  (t
		   (+ indent mirah-indent-level))))))))
      (goto-char indent-point)
      (beginning-of-line)
      (skip-syntax-forward " ")
      (if (looking-at "\\.[^.]")
	  (+ indent mirah-indent-level)
	indent))))

(defun mirah-electric-brace (arg)
  (interactive "P")
  (insert-char last-command-char 1)
  (mirah-indent-line t)
  (delete-char -1)
  (self-insert-command (prefix-numeric-value arg)))

(eval-when-compile
  (defmacro defun-region-command (func args &rest body)
    (let ((intr (car body)))
      (when (featurep 'xemacs)
	(if (stringp intr) (setq intr (cadr body)))
	(and (eq (car intr) 'interactive)
	     (setq intr (cdr intr))
	     (setcar intr (concat "_" (car intr)))))
      (cons 'defun (cons func (cons args body))))))

(defun-region-command mirah-beginning-of-defun (&optional arg)
  "Move backward to next beginning-of-defun.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (and (re-search-backward (concat "^\\(" mirah-block-beg-re "\\)\\b")
			   nil 'move (or arg 1))
       (progn (beginning-of-line) t)))

(defun mirah-beginning-of-indent ()
  (and (re-search-backward (concat "^\\(" mirah-indent-beg-re "\\)\\b")
			   nil 'move)
       (progn
	 (beginning-of-line)
	 t)))

(defun-region-command mirah-end-of-defun (&optional arg)
  "Move forward to next end of defun.
An end of a defun is found by moving forward from the beginning of one."
  (interactive "p")
  (and (re-search-forward (concat "^\\(" mirah-block-end-re "\\)\\($\\|\\b[^_]\\)")
			  nil 'move (or arg 1))
       (progn (beginning-of-line) t))
  (forward-line 1))

(defun mirah-move-to-block (n)
  (let (start pos done down)
    (setq start (mirah-calculate-indent))
    (setq down (looking-at (if (< n 0) mirah-block-end-re
			     (concat "\\<\\(" mirah-block-beg-re "\\)\\>"))))
    (while (and (not done) (not (if (< n 0) (bobp) (eobp))))
      (forward-line n)
      (cond
       ((looking-at "^\\s *$"))
       ((looking-at "^\\s *#"))
       ((and (> n 0) (looking-at "^=begin\\>"))
	(re-search-forward "^=end\\>"))
       ((and (< n 0) (looking-at "^=end\\>"))
	(re-search-backward "^=begin\\>"))
       (t
	(setq pos (current-indentation))
	(cond
	 ((< start pos)
	  (setq down t))
	 ((and down (= pos start))
	  (setq done t))
	 ((> start pos)
	  (setq done t)))))
      (if done
	  (save-excursion
	    (back-to-indentation)
	    (if (looking-at (concat "\\<\\(" mirah-block-mid-re "\\)\\>"))
		(setq done nil))))))
  (back-to-indentation))

(defun-region-command mirah-beginning-of-block (&optional arg)
  "Move backward to next beginning-of-block"
  (interactive "p")
  (mirah-move-to-block (- (or arg 1))))

(defun-region-command mirah-end-of-block (&optional arg)
  "Move forward to next beginning-of-block"
  (interactive "p")
  (mirah-move-to-block (or arg 1)))

(defun-region-command mirah-forward-sexp (&optional cnt)
  (interactive "p")
  (if (and (numberp cnt) (< cnt 0))
      (mirah-backward-sexp (- cnt))
    (let ((i (or cnt 1)))
      (condition-case nil
	  (while (> i 0)
	    (skip-syntax-forward " ")
	    (cond ((looking-at "\\?\\(\\\\[CM]-\\)*\\\\?\\S ")
		   (goto-char (match-end 0)))
		  ((progn
		     (skip-chars-forward ",.:;|&^~=!?\\+\\-\\*")
		     (looking-at "\\s("))
		   (goto-char (scan-sexps (point) 1)))
		  ((and (looking-at (concat "\\<\\(" mirah-block-beg-re "\\)\\>"))
			(not (eq (char-before (point)) ?.))
			(not (eq (char-before (point)) ?:)))
		   (mirah-end-of-block)
		   (forward-word 1))
		  ((looking-at "\\(\\$\\|@@?\\)?\\sw")
		   (while (progn
			    (while (progn (forward-word 1) (looking-at "_")))
			    (cond ((looking-at "::") (forward-char 2) t)
				  ((> (skip-chars-forward ".") 0))
				  ((looking-at "\\?\\|!\\(=[~=>]\\|[^~=]\\)")
				   (forward-char 1) nil)))))
		  ((let (state expr)
		     (while
			 (progn
			   (setq expr (or expr (mirah-expr-beg)
					  (looking-at "%\\sw?\\Sw\\|[\"'`/]")))
			   (nth 1 (setq state (apply 'mirah-parse-partial nil state))))
		       (setq expr t)
		       (skip-chars-forward "<"))
		     (not expr))))
	    (setq i (1- i)))
	((error) (forward-word 1)))
      i)))

(defun-region-command mirah-backward-sexp (&optional cnt)
  (interactive "p")
  (if (and (numberp cnt) (< cnt 0))
      (mirah-forward-sexp (- cnt))
    (let ((i (or cnt 1)))
      (condition-case nil
	  (while (> i 0)
	    (skip-chars-backward " \t\n,.:;|&^~=!?\\+\\-\\*")
	    (forward-char -1)
	    (cond ((looking-at "\\s)")
		   (goto-char (scan-sexps (1+ (point)) -1))
		   (case (char-before)
		     (?% (forward-char -1))
		     ('(?q ?Q ?w ?W ?r ?x)
		      (if (eq (char-before (1- (point))) ?%) (forward-char -2))))
		   nil)
		  ((looking-at "\\s\"\\|\\\\\\S_")
		   (let ((c (char-to-string (char-before (match-end 0)))))
		     (while (and (search-backward c)
				 (oddp (skip-chars-backward "\\")))))
		   nil)
		  ((looking-at "\\s.\\|\\s\\")
		   (if (mirah-special-char-p) (forward-char -1)))
		  ((looking-at "\\s(") nil)
		  (t
		   (forward-char 1)
		   (while (progn (forward-word -1)
				 (case (char-before)
				   (?_ t)
				   (?. (forward-char -1) t)
				   ((?$ ?@)
				    (forward-char -1)
				    (and (eq (char-before) (char-after)) (forward-char -1)))
				   (?:
				    (forward-char -1)
				    (eq (char-before) :)))))
		   (if (looking-at mirah-block-end-re)
		       (mirah-beginning-of-block))
		   nil))
	    (setq i (1- i)))
	((error)))
      i)))

(defun mirah-reindent-then-newline-and-indent ()
  (interactive "*")
  (newline)
  (save-excursion
    (end-of-line 0)
    (indent-according-to-mode)
    (delete-region (point) (progn (skip-chars-backward " \t") (point))))
  (indent-according-to-mode))

(fset 'mirah-encomment-region (symbol-function 'comment-region))

(defun mirah-decomment-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\([ \t]*\\)#" end t)
      (replace-match "\\1" nil nil)
      (save-excursion
	(mirah-indent-line)))))

(defun mirah-insert-end ()
  (interactive)
  (insert "end")
  (mirah-indent-line t)
  (end-of-line))

(defun mirah-mark-defun ()
  "Put mark at end of this Mirah function, point at beginning."
  (interactive)
  (push-mark (point))
  (mirah-end-of-defun)
  (push-mark (point) nil t)
  (mirah-beginning-of-defun)
  (re-search-backward "^\n" (- (point) 1) t))

(defun mirah-indent-exp (&optional shutup-p)
  "Indent each line in the balanced expression following point syntactically.
If optional SHUTUP-P is non-nil, no errors are signalled if no
balanced expression is found."
  (interactive "*P")
  (let ((here (point-marker)) start top column (nest t))
    (set-marker-insertion-type here t)
    (unwind-protect
	(progn
	  (beginning-of-line)
	  (setq start (point) top (current-indentation))
	  (while (and (not (eobp))
		      (progn
			(setq column (mirah-calculate-indent start))
			(cond ((> column top)
			       (setq nest t))
			      ((and (= column top) nest)
			       (setq nest nil) t))))
	    (mirah-indent-to column)
	    (beginning-of-line 2)))
      (goto-char here)
      (set-marker here nil))))

(defun mirah-add-log-current-method ()
  "Return current method string."
  (condition-case nil
      (save-excursion
	(let ((mlist nil) (indent 0))
	  ;; get current method (or class/module)
	  (if (re-search-backward
	       (concat "^[ \t]*\\(def\\|class\\|module\\)[ \t]+"
		       "\\(" 
		       ;; \\. for class method
			"\\(" mirah-symbol-re "\\|\\." "\\)" 
			"+\\)")
	       nil t)
	      (progn
		(setq mlist (list (match-string 2)))
		(goto-char (match-beginning 1))
		(setq indent (current-column))
		(beginning-of-line)))
	  ;; nest class/module
	  (while (and (> indent 0)
		      (re-search-backward
		       (concat
			"^[ \t]*\\(class\\|module\\)[ \t]+"
			"\\([A-Z]" mirah-symbol-re "+\\)")
		       nil t))
	    (goto-char (match-beginning 1))
	    (if (< (current-column) indent)
		(progn
		  (setq mlist (cons (match-string 2) mlist))
		  (setq indent (current-column))
		  (beginning-of-line))))
	  ;; generate string
	  (if (consp mlist)
	      (mapconcat (function identity) mlist "::")
	    nil)))))

(cond
 ((featurep 'font-lock)
  (or (boundp 'font-lock-variable-name-face)
      (setq font-lock-variable-name-face font-lock-type-face))

  (setq mirah-font-lock-syntactic-keywords
	'(
	  ;; #{ }, #$hoge, #@foo are not comments
	  ("\\(#\\)[{$@]" 1 (1 . nil))
	  ;; the last $', $", $` in the respective string is not variable
	  ;; the last ?', ?", ?` in the respective string is not ascii code
	  ("\\(^\\|[\[ \t\n<+\(,=]\\)\\(['\"`]\\)\\(\\\\.\\|\\2\\|[^'\"`\n\\\\]\\)*?\\\\?[?$]\\(\\2\\)"
	   (2 (7 . nil))
	   (4 (7 . nil)))
	  ;; $' $" $` .... are variables
	  ;; ?' ?" ?` are ascii codes
	  ("\\(^\\|[^\\\\]\\)\\(\\\\\\\\\\)*[?$]\\([#\"'`]\\)" 3 (1 . nil))
	  ;; regexps
	  ("\\(^\\|[=(,~?:;<>]\\|\\(^\\|\\s \\)\\(if\\|elsif\\|unless\\|while\\|until\\|when\\|and\\|or\\|&&\\|||\\)\\|g?sub!?\\|scan\\|split!?\\)\\s *\\(/\\)[^/\n\\\\]*\\(\\\\.[^/\n\\\\]*\\)*\\(/\\)"
	   (4 (7 . ?/))
	   (6 (7 . ?/)))
	  ("^\\(=\\)begin\\(\\s \\|$\\)" 1 (7 . nil))
	  ("^\\(=\\)end\\(\\s \\|$\\)" 1 (7 . nil))))

  (cond ((featurep 'xemacs)
	 (put 'mirah-mode 'font-lock-defaults
	      '((mirah-font-lock-keywords)
		nil nil nil
		beginning-of-line
		(font-lock-syntactic-keywords
		 . mirah-font-lock-syntactic-keywords))))
	(t
	 (add-hook 'mirah-mode-hook
	    '(lambda ()
	       (make-local-variable 'font-lock-defaults)
	       (make-local-variable 'font-lock-keywords)
	       (make-local-variable 'font-lock-syntax-table)
	       (make-local-variable 'font-lock-syntactic-keywords)
	       (setq font-lock-defaults '((mirah-font-lock-keywords) nil nil))
	       (setq font-lock-keywords mirah-font-lock-keywords)
	       (setq font-lock-syntax-table mirah-font-lock-syntax-table)
	       (setq font-lock-syntactic-keywords mirah-font-lock-syntactic-keywords)))))

  (defun mirah-font-lock-docs (limit)
    (if (re-search-forward "^=begin\\(\\s \\|$\\)" limit t)
	(let (beg)
	  (beginning-of-line)
	  (setq beg (point))
	  (forward-line 1)
	  (if (re-search-forward "^=end\\(\\s \\|$\\)" limit t)
	      (progn
		(set-match-data (list beg (point)))
		t)))))

  (defun mirah-font-lock-maybe-docs (limit)
    (let (beg)
      (save-excursion
	(if (and (re-search-backward "^=\\(begin\\|end\\)\\(\\s \\|$\\)" nil t)
		 (string= (match-string 1) "begin"))
	    (progn
	      (beginning-of-line)
	      (setq beg (point)))))
      (if (and beg (and (re-search-forward "^=\\(begin\\|end\\)\\(\\s \\|$\\)" nil t)
			(string= (match-string 1) "end")))
	  (progn
	    (set-match-data (list beg (point)))
	    t)
	nil)))

  (defvar mirah-font-lock-syntax-table
    (let* ((tbl (copy-syntax-table mirah-mode-syntax-table)))
      (modify-syntax-entry ?_ "w" tbl)
      tbl))

  (defun mirah-font-lock-here-docs (limit)
    (if (re-search-forward mirah-here-doc-beg-re limit t)
	(let (beg)
	  (beginning-of-line)
          (forward-line)
	  (setq beg (point))
	  (if (re-search-forward (mirah-here-doc-end-match) nil t)
	      (progn
		(set-match-data (list beg (point)))
		t)))))

  (defun mirah-font-lock-maybe-here-docs (limit)
    (let (beg)
      (save-excursion
	(if (re-search-backward mirah-here-doc-beg-re nil t)
	    (progn
	      (beginning-of-line)
              (forward-line)
	      (setq beg (point)))))
      (if (and beg
	       (let ((end-match (mirah-here-doc-end-match)))
                 (and (not (re-search-backward end-match beg t))
		      (re-search-forward end-match nil t))))
	  (progn
	    (set-match-data (list beg (point)))
	    t)
          nil)))

  (defvar mirah-font-lock-keywords
    (list
     ;; functions
     '("^\\s *def\\s +\\([^( \t\n]+\\)"
       1 font-lock-function-name-face)
     ;; keywords
     (cons (concat
	    "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b\\(defined\\?\\|\\("
	    (mapconcat
	     'identity
	     '("alias"
	       "and"
	       "begin"
	       "break"
	       "case"
	       "catch"
	       "class"
	       "def"
	       "do"
	       "elsif"
	       "else"
	       "fail"
	       "ensure"
	       "for"
	       "end"
	       "if"
	       "in"
	       "module"
	       "next"
	       "not"
	       "or"
	       "raise"
	       "redo"
	       "rescue"
	       "retry"
	       "return"
	       "then"
	       "throw"
	       "super"
	       "unless"
	       "undef"
	       "until"
	       "when"
	       "while"
	       "yield"
	       )
	     "\\|")
	    "\\)\\>\\)")
	   2)
     ;; variables
     '("\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b\\(nil\\|self\\|true\\|false\\)\\>"
       2 font-lock-variable-name-face)
     ;; variables
     '("\\(\\$\\([^a-zA-Z0-9 \n]\\|[0-9]\\)\\)\\W"
       1 font-lock-variable-name-face)
     '("\\(\\$\\|@\\|@@\\)\\(\\w\\|_\\)+"
       0 font-lock-variable-name-face)
     ;; embedded document
     '(mirah-font-lock-docs
       0 font-lock-comment-face t)
     '(mirah-font-lock-maybe-docs
       0 font-lock-comment-face t)
     ;; "here" document
     '(mirah-font-lock-here-docs
       0 font-lock-string-face t)
     '(mirah-font-lock-maybe-here-docs
       0 font-lock-string-face t)
     `(,mirah-here-doc-beg-re
       0 font-lock-string-face t)
     ;; general delimited string
     '("\\(^\\|[[ \t\n<+(,=]\\)\\(%[xrqQwW]?\\([^<[{(a-zA-Z0-9 \n]\\)[^\n\\\\]*\\(\\\\.[^\n\\\\]*\\)*\\(\\3\\)\\)"
       (2 font-lock-string-face))
     ;; constants
     '("\\(^\\|[^_]\\)\\b\\([A-Z]+\\(\\w\\|_\\)*\\)"
       2 font-lock-type-face)
     ;; symbols
     '("\\(^\\|[^:]\\)\\(:\\([-+~]@?\\|[/%&|^`]\\|\\*\\*?\\|<\\(<\\|=>?\\)?\\|>[>=]?\\|===?\\|=~\\|\\[\\]=?\\|\\(\\w\\|_\\)+\\([!?=]\\|\\b_*\\)\\|#{[^}\n\\\\]*\\(\\\\.[^}\n\\\\]*\\)*}\\)\\)"
       2 font-lock-reference-face)
     ;; expression expansion
     '("#\\({[^}\n\\\\]*\\(\\\\.[^}\n\\\\]*\\)*}\\|\\(\\$\\|@\\|@@\\)\\(\\w\\|_\\)+\\)"
       0 font-lock-variable-name-face t)
     ;; warn lower camel case
     ;'("\\<[a-z]+[a-z0-9]*[A-Z][A-Za-z0-9]*\\([!?]?\\|\\>\\)"
     ;  0 font-lock-warning-face)
     )
    "*Additional expressions to highlight in mirah mode."))

 ((featurep 'hilit19)
  (hilit-set-mode-patterns
   'mirah-mode
   '(("[^$\\?]\\(\"[^\\\"]*\\(\\\\\\(.\\|\n\\)[^\\\"]*\\)*\"\\)" 1 string)
     ("[^$\\?]\\('[^\\']*\\(\\\\\\(.\\|\n\\)[^\\']*\\)*'\\)" 1 string)
     ("[^$\\?]\\(`[^\\`]*\\(\\\\\\(.\\|\n\\)[^\\`]*\\)*`\\)" 1 string)
     ("^\\s *#.*$" nil comment)
     ("[^$@?\\]\\(#[^$@{\n].*$\\)" 1 comment)
     ("[^a-zA-Z_]\\(\\?\\(\\\\[CM]-\\)*.\\)" 1 string)
     ("^\\s *\\(require\\|load\\).*$" nil include)
     ("^\\s *\\(include\\|alias\\|undef\\).*$" nil decl)
     ("^\\s *\\<\\(class\\|def\\|module\\)\\>" "[)\n;]" defun)
     ("[^_]\\<\\(begin\\|case\\|else\\|elsif\\|end\\|ensure\\|for\\|if\\|unless\\|rescue\\|then\\|when\\|while\\|until\\|do\\|yield\\)\\>\\([^_]\\|$\\)" 1 defun)
     ("[^_]\\<\\(and\\|break\\|next\\|raise\\|fail\\|in\\|not\\|or\\|redo\\|retry\\|return\\|super\\|yield\\|catch\\|throw\\|self\\|nil\\)\\>\\([^_]\\|$\\)" 1 keyword)
     ("\\$\\(.\\|\\sw+\\)" nil type)
     ("[$@].[a-zA-Z_0-9]*" nil struct)
     ("^__END__" nil label))))
 )


(provide 'mirah-mode)
