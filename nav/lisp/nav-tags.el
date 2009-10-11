;; nav-tags.el
;;
;;; License:
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


;; Make sure we can get tags for Python.
(condition-case err
    (require 'python)
  (error
   'error-setting-up-python-support))

(defcustom nav-tags-will-sort t
  "Whether to sort alphabetically in tags mode")

(defvar nav-tags-alist nil
  "Association list from tag names to positions")


(defun nav-tags-make-mode-map ()
  "Creates and returns a mode map with tags's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "w" 'nav-shrink-wrap)
    (define-key keymap "W" 'nav-set-width-to-default)
    (define-key keymap "q" 'nav-quit)
    (define-key keymap "r" 'nav-tags-refresh)
    (define-key keymap "s" 'nav-tags-sort)
    (define-key keymap "t" 'nav-tags-quit)
    (define-key keymap "u" 'nav-tags-quit)
    (define-key keymap "?" 'nav-tags-help-screen)
    (define-key keymap [S-down-mouse-3] 'nav-tags-quit)
    (define-key keymap [(tab)] 'forward-button)
    (define-key keymap [(shift tab)] 'backward-button)
    (define-key keymap [(down)] 'forward-button)
    (define-key keymap [(up)] 'backward-button)
    (define-key keymap [(control ?n)] 'forward-button)
    (define-key keymap [(control ?p)] 'backward-button)
    keymap))

(setq nav-tags-mode-map (nav-tags-make-mode-map))


(defun nav-tags-sort-by-name (item1 item2)
  (string-lessp (car item1) (car item2)))


(defun nav-marker-to-position (maybe-marker)
  "Converts a marker to a position, or just returns the arg unchanged."
  (if (markerp maybe-marker)
      (marker-position maybe-marker)
    maybe-marker))


(defun nav-tags-flatten (name-and-info)
  "Converts class tags into flat names of class methods."
  (let ((name (car name-and-info))
	(info (cdr name-and-info)))
    (if (and (string-match "^class [a-zA-Z0-9_]+$" name)
	     (listp info))
	(let ((class-pos (cdr (car info)))
	      (class-name (substring name (length "class ") (length name))))
	  (cons (cons name class-pos)
		(mapcar (lambda (method-name-and-pos)
			  (let ((method-name (car method-name-and-pos))
				(pos (cdr method-name-and-pos)))
			    (cons (concat class-name "." method-name)
				  pos)))
			(cddr name-and-info))))
      (list name-and-info))))


(defun nav-marker-to-pos-in-pair (name-and-maybe-marker)
  (let ((name (car name-and-maybe-marker))
	(maybe-marker (cdr name-and-maybe-marker)))
    (cons name (nav-marker-to-position maybe-marker))))


(defun nav-make-tags-alist ()
  "Builds the tags association list from the current buffer."
  (let* ((imenu-auto-rescan t)
	 (imenu-auto-rescan-maxout nav-max-int)
	 (alist (imenu--make-index-alist t))
	 ;; Maybe sort.
	 (alist (if nav-tags-will-sort
		    (sort alist 'nav-tags-sort-by-name)
		  alist))
	 (alists (mapcar 'nav-tags-flatten alist))
	 (alist (apply 'append alists))
	 (alist (mapcar 'nav-marker-to-pos-in-pair alist)))
    alist))


(defun nav-tags-refresh ()
  "Updates the Nav tags list."
  (interactive)
  (nav-tags-fetch-imenu nav-tags-filename))


(defun nav-tags-fetch-imenu (filename)
  "Generates and displays the tag index from selected file."
  (require 'imenu)
  (setq nav-tags-filename filename)
  (nav-open-file filename)
  (setq nav-tags-alist (nav-make-tags-alist))

  (select-window (nav-get-window nav-buffer-name))
  (nav-tags))


(defun nav-jump-to-tag-of-button (button)
  ;; For sorting?
  (select-window (nav-get-window nav-buffer-name))

  (let* ((tag (button-label button))
	 (num (cdr (assoc tag nav-tags-alist))))
    (select-window (nav-get-window nav-tags-filename))
    (goto-char num))

  ;; recenter-top-bottom is not defined in emacs 22.
  (when (functionp 'recenter-top-bottom)
      (recenter-top-bottom)))


(defun nav-tags-show-tags ()
  "Displays all functions in selected file."
  (interactive)
  (let ((inhibit-read-only t)
	(tags (nav-extract-function-tags nav-tags-alist)))
    (erase-buffer)
    (nav-insert-text nav-tags-filename nav-face-heading)
    (insert "\n")
    (dolist (tag tags)
      (let ((tag-name (car tag)))
	(insert-button tag-name
		       'action 'nav-jump-to-tag-of-button
		       'follow-link t
		       'face nav-button-face
		       'help-echo nil)
	(insert "\n")))
    (setq mode-line-format "nav: Tag list")
    (force-mode-line-update)
    (setq truncate-lines t)
    (goto-line 2)))


(defun nav-extract-function-tags (tags-alist)
  (nav-filter (lambda (name-and-info)
		(let ((name (car name-and-info))
		      (info (cdr name-and-info)))
		  (not (or (string= name "*Rescan*")
			   (string= name "Module variables")
			   (and (string= name "Types")
				(listp info))
			   (and (string= name "Variables")
				(listp info))))))
	      tags-alist))


(defun nav-tags-sort ()
  "Toggles sort to by name/position and re-displays tags"
  (interactive)
  (setq nav-tags-will-sort (not nav-tags-will-sort))
  (nav-tags-refresh))


(defun nav-tags-quit ()
  "Kill nav-tags."
  (interactive)
  (select-window (nav-get-window nav-buffer-name))
  (nav-mode))
  

(define-derived-mode nav-tags-mode fundamental-mode 
  "Nav-buf-mode is displaying and switching buffers."
  (setq mode-name "Nav tags")
  (use-local-map nav-tags-mode-map)
  (setq buffer-read-only t)
  (nav-tags-show-tags))


(defun nav-tags ()
  "Run nav-tags-mode on top of nav."
  (interactive)
  (nav-tags-mode))


(defun nav-tags-help-screen ()
  "Displays the help screen outside the Nav window."
  (interactive)
  (other-window 1)
  (get-buffer-create "nav-help")
  (switch-to-buffer "nav-help")
  (get-buffer "nav-help")
  (setq map (make-sparse-keymap))
  (use-local-map map)
  (define-key map [mouse-1] 'nav-help-screen-kill)
  (define-key map [mouse-3] 'nav-help-screen-kill) 
  (define-key map [mouse-2] 'nav-help-screen-kill) 
  (define-key map "q" 'nav-help-screen-kill)
  (setq display-hourglass nil
        buffer-undo-list t)  
  (insert "\
Help for Nav tags mode
======================

Key Bindings
============

Enter/Return: Jump to tag under cursor

q\t Quit Nav.
s\t Toggle sorting of tags into alphabetical order.
t\t Exit tags mode and go back to directory view (or Shift-Left-Mouse).
u\t Same as t. I.e., go up to view the file and other contents of the directory.
w\t Shrink-wrap Nav's window to fit the longest tag.
W\t Set the window width to its default value.
?\t Show this help screen.


                Press 'q' or click mouse to quit help

")
  (goto-line 1)
  (view-mode -1)
  (toggle-read-only 1))


(provide 'nav-tags)

;;; nav-tags.el ends here