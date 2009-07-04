;;; ani-fcsh.el --- Lets compile work without restarting fcsh and recognize flex errors.

;; Copyright (C) 2008 Anirudh Sasikumar
;; Author: Anirudh Sasikumar
;; URL: http://anirudhs.chaosnet.org/code/ani-fcsh.el

;; Based on code in compile.el distributed along with emacs 22.1:

;; Copyright (C) 1985, 1986, 1987, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
;;   2001, 2002, 2003, 2004, 2005, 2006, 2007  Free Software Foundation, Inc.

;; Authors: Roland McGrath <roland@gnu.org>,
;;	    Daniel Pfeiffer <occitan@esperanto.org>
;; Maintainer: FSF
;; Keywords: tools, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; When visiting the main MXML file, call fcsh-compile. Then compile
;; will invoke fcsh and run mxmlc. Subsequent compile's will send
;; compile 1 without killing existing fcsh

;;; Code:

(require 'compile)

(defvar *fcsh-compile-old-compilation-start* nil
  "Reference to actual compilation-start function that fcsh overrides")

(defvar *fcsh-path* "/usr/local/flex320/bin/fsch"
  "Path to run fcsh. This is usually sdks/bin/fcsh.")

(defvar *fcsh-compile-active* nil
  "true if `fcsh-compile' has been called.
`fcsh-compile' calls `fcsh-restore-compile' if this is true.")

(defvar *fcsh-mxmlc-command* "mxmlc"
  "The command to run first when fcsh is started.")

(defvar *fcsh-mxmlc-subsequent-command* "compile 1"
  "The command to run when compile is called after the first time.")

(defvar *fcsh-mxmlc-output-path* "..\\bin-debug\\"
  "Output path to the swf. This is passed as an argument to `*fcsh-mxmlc-command*'.
The swf's name is `*fcsh-mxml-file*' with mxml extension stripped off
and swf extension added.")

(defvar *fcsh-mxml-file* ""
  "The name of the mxml file to pass to the first and only mxmlc command.")

;; To let compile understad flex error / warning messages. This regexp
;; has been tested only on win.
(add-to-list 'compilation-error-regexp-alist 'flex)
(add-to-list 'compilation-error-regexp-alist-alist
	     '(flex "^\\(.*\\)(\\(.*\\)):.* \\(Error\\|Warnin\\(g\\)\\): .*$" 1 2 nil (4)))

(defun fcsh-compile ()
  "Call this function from a buffer visiting the MXML file you
want to compile.

It is best to have a mxmlfilename-config.xml to
pass arguments to mxmlc.

Once this function is called, compile
will invoke fcsh and not kill it until the compilation buffer is
manually killed. Subsequent compile and recompile will use the
same fcsh instance and pass compile 1 to it.

Use `fcsh-restore-compile' to restore compile's old behaviour.
See `*fcsh-compile-active*'."
  (interactive)
  (setq compile-command *fcsh-path*)
  (setq *fcsh-mxml-file* (buffer-name))
  (if (not *fcsh-compile-active*)
      (progn
	(setq *fcsh-compile-active* t)
	(fset '*fcsh-compile-old-compilation-start* (symbol-function 'compilation-start))
	(fset 'compilation-start 'fcsh-compilation-start)
	)
    )

  )

(defun fcsh-restore-compile ()
  "See `fcsh-compile', `*fcsh-compile-active*' for more info."
  (interactive)
  (if *fcsh-compile-active*
      (progn
	(setq *fcsh-compile-active* nil)
	(fset 'compilation-start '*fcsh-compile-old-compilation-start*)
	)
    ))

(defun fcsh-compilation-filter (proc string)
  "Process filter for compilation buffers.
Inserts the text, but uses `insert-before-markers'.

Also checks for (fcsh) string to see if the compilation has ended."
  (if (buffer-name (process-buffer proc))
      (with-current-buffer (process-buffer proc)
	(let ((inhibit-read-only t))
	  (save-excursion
	    (goto-char (process-mark proc))
	    (insert-before-markers string)
	    ;; change for bug reported by craig
	    (if (and (>= (length string) (length "(fcsh) "))
		     (string= (substring string (- (length string) (length "(fcsh) "))) "(fcsh) "))
		(progn
		  (compilation-handle-exit (process-status proc)
					   (process-exit-status proc)
					   "finished")
		  (setq compilation-in-progress (delq proc compilation-in-progress))
		  ))

	    (run-hooks 'compilation-filter-hook))))))


(defun fcsh-compilation-start (command &optional mode name-function highlight-regexp)
  "Same as compilation-start but instead of spawning a different
process each time, keeps on passing command to the existing fcsh
shell."
  (or mode (setq mode 'compilation-mode))
  (let* ((name-of-mode
	  (if (eq mode t)
	      (prog1 "compilation" (require 'comint))
	    (replace-regexp-in-string "-mode$" "" (symbol-name mode))))
	 (thisdir default-directory)
	 outwin outbuf)
    (with-current-buffer
	(setq outbuf
	      (get-buffer-create
	       (compilation-buffer-name name-of-mode mode name-function)))
      (let ((comp-proc (get-buffer-process (current-buffer))))	    ;;
      	(if comp-proc						    ;;
      	    (if (eq (process-status comp-proc) 'run)	    ;;
		(setq mode t);;
      	      )))
      (if (not (eq mode t))
	  (progn
	    (buffer-disable-undo (current-buffer))
	    ;; first transfer directory from where M-x compile was called
	    (setq default-directory thisdir)
	    )
	)
      ;; Make compilation buffer read-only.  The filter can still write it.
      ;; Clear out the compilation buffer.
      (if (not (eq mode t))
	  (let ((inhibit-read-only t)
		(default-directory thisdir))
	    ;; Then evaluate a cd command if any, but don't perform it yet, else start-command
	    ;; would do it again through the shell: (cd "..") AND sh -c "cd ..; make"
	    (cd (if (string-match "^\\s *cd\\(?:\\s +\\(\\S +?\\)\\)?\\s *[;&\n]" command)
		    (if (match-end 1)
			(substitute-env-vars (match-string 1 command))
		      "~")
		  default-directory))
	    (erase-buffer)
	    ;; Select the desired mode.
	    (if (not (eq mode t))
		(funcall mode)
	      (setq buffer-read-only nil)
	      (with-no-warnings (comint-mode))
	      (compilation-shell-minor-mode))
	    (if highlight-regexp
		(set (make-local-variable 'compilation-highlight-regexp)
		     highlight-regexp))
	    ;; Output a mode setter, for saving and later reloading this buffer.
	    (insert "-*- mode: " name-of-mode
		    "; default-directory: " (prin1-to-string default-directory)
		    " -*-\n"
		    (format "%s started at %s\n\n"
			    mode-name
			    (substring (current-time-string) 0 19))
		    command "\n")
	    (setq thisdir default-directory))
	)
      (set-buffer-modified-p nil))
    ;; If we're already in the compilation buffer, go to the end
    ;; of the buffer, so point will track the compilation output.
    (if (eq outbuf (current-buffer))
	(goto-char (point-max)))
    ;; Pop up the compilation buffer.
    (setq outwin (display-buffer outbuf nil t))
    (with-current-buffer outbuf
      (let ((process-environment
	     (append
	      compilation-environment
	      (if (if (boundp 'system-uses-terminfo) ; `if' for compiler warning
		      system-uses-terminfo)
		  (list "TERM=dumb" "TERMCAP="
			(format "COLUMNS=%d" (window-width)))
		(list "TERM=emacs"
		      (format "TERMCAP=emacs:co#%d:tc=unknown:"
			      (window-width))))
	      ;; Set the EMACS variable, but
	      ;; don't override users' setting of $EMACS.
	      (unless (getenv "EMACS")
		(list "EMACS=t"))
	      (list "INSIDE_EMACS=t")
	      (copy-sequence process-environment)))
	    fcsh-initial-contents
	    )

	(if (not (eq mode t))
	    (progn
	      (set (make-local-variable 'compilation-arguments)
		   (list command mode name-function highlight-regexp))
	      (set (make-local-variable 'revert-buffer-function)
		   'compilation-revert-buffer)

	      (set-window-start outwin (point-min))
	      (or (eq outwin (selected-window))
		  (set-window-point outwin (if compilation-scroll-output
					       (point)
					     (point-min))))

	      )

	  )

	;; The setup function is called before compilation-set-window-height
	;; so it can set the compilation-window-height buffer locally.
	(if compilation-process-setup-function
	    (funcall compilation-process-setup-function))
	(compilation-set-window-height outwin)
	(pop-to-buffer (current-buffer) t t)
	(goto-char (point-min))
	(next-line 2)
	(setq fcsh-initial-contents (buffer-substring (point-min) (point)))
	(setq buffer-read-only nil)

	(erase-buffer)
	(insert fcsh-initial-contents)
	(insert "\n(fcsh) ")
	(goto-char (point-max))
	(setq buffer-read-only t)
	;; Start the compilation.
	(if (fboundp 'start-process)
	    (let ((proc (if (eq mode t)
			    (get-buffer-process (current-buffer))
			  (start-process-shell-command (downcase mode-name)
						       outbuf command))))
	      ;; Make the buffer's mode line show process state.
	      (setq mode-line-process '(":%s"))
	      (if (not (eq mode t))
		  (progn
		    (set-process-sentinel proc 'compilation-sentinel)
		    (set-process-filter proc 'fcsh-compilation-filter)))

	      (if (not (eq mode t) )
		  (progn
		    (goto-char (point-max))
		    (setq buffer-read-only nil)
		    (insert (concat "\n" (concat *fcsh-mxmlc-command* " "
						 *fcsh-mxml-file* " -o "
						 *fcsh-mxmlc-output-path*
						 (file-name-sans-extension *fcsh-mxml-file*)
						 ".swf\n" )))
		    (setq buffer-read-only t)
		    (comint-send-string proc (concat *fcsh-mxmlc-command* " "
						     *fcsh-mxml-file* " -o "
						     *fcsh-mxmlc-output-path*
						     (file-name-sans-extension *fcsh-mxml-file*)
						     ".swf\n" ))
		    )
		)

	      (if (eq mode t)
		  (progn
		    (goto-char (point-max))
		    (setq buffer-read-only nil)
		    (insert (concat "\n" *fcsh-mxmlc-subsequent-command* "\n"))
		    (goto-char (point-max))
		    (setq buffer-read-only t)
		    (comint-send-string proc (concat *fcsh-mxmlc-subsequent-command* "\n"))
		    ))

	      (set-marker (process-mark proc) (point) outbuf)
	      (when compilation-disable-input
                (condition-case nil
                    (process-send-eof proc)
                  ;; The process may have exited already.
                  (error nil)))
	      (setq compilation-in-progress
		    (cons proc compilation-in-progress)))
	  ))
      ;; Now finally cd to where the shell started make/grep/...
      (setq default-directory thisdir))
    (if (buffer-local-value 'compilation-scroll-output outbuf)
	(save-selected-window
	  (select-window outwin)
	  (goto-char (point-max))))
    (other-window 1)
    ;; Make it so the next C-x ` will use this buffer.
    (setq next-error-last-buffer outbuf)))

