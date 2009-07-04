;;; org-inlinetask.el --- Tasks independent of outline hierarchy
;; Copyright (C) 2009 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.28e
;;
;; This file is not yet part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This module implements inline tasks in Org-mode.  Inline tasks are
;; tasks that have all the properties of normal outline nodes, including
;; the ability to store meta data like scheduling dates, TODO state, tags
;; and properties.  However, these nodes are treated specially by the
;; visibility cycling and export commands.
;;
;; Visibility cycling exempts these nodes from cycling. So whenever their
;; parent is opened, so are these tasks.  This will only work with
;; `org-cycle', so if you are also using orther commands to show/hide
;; entries, you will occasionally find these tasks to behave like
;; all other outline nodes, seemingly splitting the text of the parent
;; into children.
;;
;; Export commands do not treat these nodes as part of the sectioning
;; structure, but as a special inline text that is either removed, or
;; formatted in some special way.
;;
;; Special fontification of inline tasks, so that they can be immediately
;; recognized.  From the stars of the headline, only the first and the
;; last two will be visible, the others will be hidden using the
;; `org-hide' face.
;;
;; An inline task is identified solely by a minimum outline level, given
;; by the variable `org-inlinetask-min-level', default 15.
;;
;; Inline tasks are normally assumed to contain at most a time planning
;; line (DEADLINE etc) after it, and then any number of drawers, for
;; example LOGBOOK of PROPERTIES.  No empty lines are allowed.
;; If you need to have normal text as part of an inline task, you
;; can do so by adding an "END" headline with the same number of stars,
;; for example
;;
;;    **************** TODO some small task
;;                     DEADLINE: <2009-03-30 Mon>
;;                     :PROPERTIES:
;;                       :SOMETHING: or other
;;                     :END:
;;                     And here is some extra text
;;    **************** END

;;; Code

(defgroup org-inlinetask nil
  "Options concerning inline tasks in Org mode."
  :tag "Org Inline Tasks"
  :group 'org-structure)

(defcustom org-inlinetask-min-level 15
  "Minimum level a headline must have before it is treated as an inline task.
It is strongly recommended that you set `org-cycle-max-level' not at all,
or to a number smaller than this one.  In fact, when `org-cycle-max-level' is
not set, it will be assumed to be one less than the value of smaller than
the value of this variable."
  :group 'org-inlinetask
  :type 'boolean)

(defcustom org-inlinetask-export 'arrow+content
  "What should be done with inlinetasks upon export?
Possible values:

nil            Remove entirely, headline and \"content\"
arrow          Insert heading in bold, preceeded by an arrow
arrow+content  Insert arrow and headline, add content below in an
               #+begin_example box (ugly, but works for now)

The \"content\" of an inline task is the material below the planning
line and any drawers, up to a lines wit the same number of stars,
but containing only the word END."
  :group 'org-inlinetask
  :group 'org-export-general
  :type '(choice
	  (const :tag "Remove entirely" nil)
	  (const :tag "Headline preceeded by arrow" arrow)
	  (const :tag "Arrow, headline, + content" arrow+content)))

(defvar org-odd-levels-only)
(defvar org-keyword-time-regexp)
(defvar org-drawer-regexp)
(defvar org-complex-heading-regexp)
(defvar org-property-end-re)
(defun org-inlinetask-export-handler ()
  "Handle headlines with level larger or equal to `org-inlinetask-min-level'.
Either remove headline and meta data, or do special formatting."
  (goto-char (point-min))
  (let* ((nstars (if org-odd-levels-only
		     (1- (* 2 (or org-inlinetask-min-level 200)))
		   (or org-inlinetask-min-level 200)))
	 (re1 (format "^\\(\\*\\{%d,\\}\\) .*\n" nstars))
	 (re2 (concat "^[ \t]*" org-keyword-time-regexp))
	 headline beg end stars content)
    (while (re-search-forward re1 nil t)
      (setq headline (match-string 0)
	    stars (match-string 1)
	    content nil)
      (replace-match "")
      (while (looking-at re2)
	(delete-region (point) (1+ (point-at-eol))))
      (while (looking-at org-drawer-regexp)
	(setq beg (point))
	(if (re-search-forward org-property-end-re nil t)
	    (delete-region beg (1+ (match-end 0)))))
      (setq beg (point))
      (when (and (re-search-forward "^\\(\\*+\\) " nil t)
		 (= (length (match-string 1)) (length stars))
		 (progn (goto-char (match-end 0))
			(looking-at "END[ \t]*$")))
	(setq content (buffer-substring beg (1- (point-at-bol))))
	(delete-region beg (1+ (match-end 0))))
      (goto-char beg)
      (when (and org-inlinetask-export
		 (string-match org-complex-heading-regexp headline))
	(when (memq org-inlinetask-export '(arrow+content arrow))
	  (insert "\n\n\\Rightarrow\\Rightarrow\\Rightarrow *"
		  (if (match-end 2) (concat (match-string 2 headline) " ") "")
		  (match-string 4 headline) "*\n"))
	(when (and content (eq org-inlinetask-export 'arrow+content))
	  (insert "#+BEGIN_EXAMPLE\n" content "\n#+END_EXAMPLE\n"))
	(insert "\n")))))

(defun org-inlinetask-fontify (limit)
  "Fontify the inline tasks."
  (let* ((nstars (if org-odd-levels-only
		     (1- (* 2 (or org-inlinetask-min-level 200)))
		   (or org-inlinetask-min-level 200)))
	 (re (concat "^\\(\\*\\)\\(\\*\\{"
		    (format "%d" (- nstars 3))
		    ",\\}\\)\\(\\*\\* .*\\)")))
    (while (re-search-forward re limit t)
      (add-text-properties (match-beginning 1) (match-end 1)
			   '(face org-warning font-lock-fontified t))
      (add-text-properties (match-beginning 2) (match-end 2)
			   '(face org-hide font-lock-fontified t))
      (add-text-properties (match-beginning 3) (match-end 3)
			   '(face shadow font-lock-fontified t)))))

(eval-after-load "org-exp"
  '(add-hook 'org-export-preprocess-after-tree-selection-hook
	     'org-inlinetask-export-handler))
(eval-after-load "org"
  '(add-hook 'org-font-lock-hook 'org-inlinetask-fontify))

(provide 'org-inlinetask)

;;; org-inlinetask.el ends here

