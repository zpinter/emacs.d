;;; open-resource.el -- finds resources in a project directory
;;
;; Copyright (C) 2008 by Alexandru Nedelcu
;;
;; Author:	Alexandru Nedelcu
;; Status:	Tested with GNU Emacs 22.1.91.2
;; Keywords:	open-resorce, find, convenience
;; X-URL:       http://lexoft.eu/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting started:
;;
;; To use the functions provided by this package you should put this file into
;; your load-path and add the following line to your .emacs file:
;;    (require 'open-resource)
;;
;; You can also setup a shortcut to open-resource, something like:
;;     (global-set-key "\C-\M-r" 'open-resource)
;; which binds Control-Alt-r to the open-resource function.
;;
;; WARNING: if not customized, open-resource searches for all files in $HOME
;;          which may be slow, depending on the number of files you have.
;;
;; To customize the default search location, in emacs ...
;;     M-x customize-group <RET> open-resource <RET>
;; Then you can set your project's repository and an ignore list.
;;
;; Alternatively, if you dislike emacs's customization facility, you can
;; use these commands in your .emacs:
;;
;;   (setq
;;     open-resource-repository-directory
;;     "~/Projects")
;;   (setq
;;     open-resource-ignore-patterns
;;     (quote ("/target/" "~$" ".old$")))
;;
;; In the above example, we configured the default location to be:
;;    ~/Projects
;; and the ignore list to be files matching the following regexps:
;;    /target/
;;    ~$
;;    .old$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History
;;
;; Revision 0.1  Sat May 10 18:40:16 EEST 2008 anedelcu
;; First version of open-resource.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)

(defgroup open-resource nil
  "Finding resources (files, directories) with glob patterns."
  :group 'open-resource)

(defcustom open-resource-repository-directory "~/repos"
  "*Set this to your repository to find files in, without prompt."
  :group 'open-resource
  :type 'string)

(defcustom open-resource-ignore-patterns nil
  "List of patterns to ignore in search."
  :group 'open-resource
  :type '(repeat string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-file-in-directory (directory filepattern)
  (interactive "DDirectory: \nMFile pattern: ")

  (defun ignore-command (ignorelist)
    (if ignorelist
	(concat " | grep -v " (car ignorelist)
		(ignore-command (cdr ignorelist)))
      ""))

  (let ((b (switch-to-buffer "*find-files*")))
    (erase-buffer)
    (shell-command
     (message
      (concat "find " directory " -iname '*" filepattern "*'"
	      (ignore-command open-resource-ignore-patterns)
	      " | awk '{gsub(\"'`pwd`'\",\".\", $0); gsub(\"'$HOME'\",\"~\",$0); print $0}'"))
     b)
    (recentf-open-files
     (split-string (buffer-string) "\n"))))

(defun open-resource (filepattern)
  (interactive "MFile pattern: ")
  (if open-resource-repository-directory
      (find-file-in-directory open-resource-repository-directory filepattern)
    (message "Repository not defined. Use (customize-group open-resource).")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'open-resource)

