;; nav-bufs.el
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


(defun nav-buffer-jump-button-action (button)
  (setq buf-name (button-label button))
  (other-window 1)
  (nav-buffer-jump buf-name))


(defun nav-buffer-jump (buf-name)
  "Jumps to selected buffer."
  (interactive)
  (get-buffer-create buf-name)
  (switch-to-buffer buf-name)
  (get-buffer buf-name))


(defun nav-bufs-make-mode-map ()
  "Creates and returns a mode map with bufs's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "1" 'nav-open-buf-other-window-1)
    (define-key keymap "2" 'nav-open-buf-other-window-2)
    (define-key keymap "5" (lambda nil (interactive) (nav-quickfile-jump 0)))   
    (define-key keymap "6" (lambda nil (interactive) (nav-quickfile-jump 1)))
    (define-key keymap "7" (lambda nil (interactive) (nav-quickfile-jump 2)))
    (define-key keymap "b" 'nav-bufs-quit)
    (define-key keymap "d" 'nav-bufs-delete)
    (define-key keymap "o" (lambda nil (interactive) (other-window 1)))
    (define-key keymap "r" 'nav-bufs-show-buffers)
    (define-key keymap "q" 'nav-quit)
    (define-key keymap "s" 'nav-shell)
    (define-key keymap "t" 'nav-bufs-expand-tags)
    (define-key keymap "u" 'nav-bufs-quit)
    (define-key keymap "w" 'nav-shrink-wrap)
    (define-key keymap "W" 'nav-set-width-to-default)
    (define-key keymap "?" 'nav-bufs-help-screen)
    (define-key keymap [mouse-3] 'nav-bufs-quit)
    (define-key keymap [S-down-mouse-3] 'nav-mouse-tags-expand)
    (define-key keymap [(tab)] 'forward-button)
    (define-key keymap [(shift tab)] 'backward-button)
    (define-key keymap [(down)] 'forward-button)
    (define-key keymap [(up)] 'backward-button)
    (define-key keymap [(control ?n)] 'forward-button)
    (define-key keymap [(control ?p)] 'backward-button)
    (define-key keymap [(control ?x) (control ?f)] 'find-file-other-window)
    keymap))

(setq nav-bufs-mode-map (nav-bufs-make-mode-map))


(defun nav-bufs-help-screen ()
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
Help for Nav bufs mode
======================

Key Bindings
============

Enter/Return: Jump to buffers under cursor.

1\t Open the selected buffer in the first other window.
2\t Open the selected buffer in the second other window.

5\t Jump to 1st quick file.
6\t Jump to 2nd quick file.
7\t Jump to 3rd quick file.

b\t Go back to Nav directory view (or Left-Mouse).
d\t Delete selected buffer.
o\t Switch to other window.
q\t Quit Nav.
r\t Refresh
s\t Launch shell.
u\t Go back to Nav directory view.
w\t Shrink-wrap Nav's window to fit the longest filename in the current directory.
W\t Set the window width to its default value.
?\t Show this help screen.


                Press 'q' or click mouse to quit help

")
  (goto-line 1)
  (view-mode -1)
  (toggle-read-only 1))


(defun nav-open-buf-other-window (k)
  (let ((filename (nav-get-cur-line-str))
        (dirname (nav-get-working-dir)))
    (other-window k)
    (nav-buffer-jump filename)))


(defun nav-bufs-delete ()
  "Deletes the chosen buffer."
  (interactive)
  (kill-buffer (nav-get-cur-line-str))
  (nav-bufs-show-buffers))


(defun nav-open-buf-other-window-1 ()
  "Opens the file under the cursor in the first other window.

This is equivalent to just pressing the [enter] key. 
See nav-open-file-other-window-2."
  (interactive)
  (nav-open-buf-other-window 1))


(defun nav-ensure-second-window-exists ()
  "Makes sure there is a second file-editing area on the right.

Jumps back to nav window when done."
  (when (= 2 (length (window-list)))
    (other-window 1)
    (if (eq nav-split-window-direction 'horizontal)
        (split-window-horizontally)
      (split-window-vertically))
    (select-window (nav-get-window nav-buffer-name))))


(defun nav-open-buf-other-window-2 ()
  "Opens the file under the cursor in the second other window.

If there is no second other window, Nav will create one."
  (interactive)
  (nav-ensure-second-window-exists)
  (nav-open-buf-other-window 2))


(defun nav-bufs-expand-tags ()
  "Show tags for highlighted buffer."
  (interactive)
  (let ((buffer (nav-get-cur-line-str)))
    (other-window 1)
    (switch-to-buffer buffer))
  (setq nav-tags-filename (buffer-name))
  (setq nav-tags-alist (nav-make-tags-alist))
  (select-window (nav-get-window nav-buffer-name))
  (nav-tags))


(defun nav-bufs-show-buffers ()
  "Displays current buffers and create buttons to switch too."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq blist (mapcar (function buffer-name) (buffer-list)))
    (nav-insert-text "Active Buffers:" nav-face-heading)
    (insert "\n")
    (let* ((active-bufs (nav-filter (lambda (b)
				      (not (string-match "^[ *]" b)))
				    blist))
	   (active-bufs (sort active-bufs 'string<)))
      (dolist (b active-bufs)
	(insert-text-button b :type 'buffer-jump-button)
	(insert "\n")))
    (insert "\n")
    (nav-insert-text "Scratch Buffers:" nav-face-heading)
    (insert "\n")
    (let* ((scratch-bufs (nav-filter (lambda (b)
				       (string-match "^\\*" b))
				     blist))
	   (scratch-bufs (sort scratch-bufs 'string<)))
      (dolist (b scratch-bufs)
	  (insert-text-button b :type 'buffer-jump-button)
	  (insert "\n")))
    (setq mode-line-format "nav: Buffer list")
    (force-mode-line-update))
  (setq truncate-lines t)
  (goto-line 2))


(defun nav-bufs-quit ()
  "Kill nav-bufs."
  (interactive)
  (select-window (nav-get-window nav-buffer-name))
  (nav-mode))


(define-derived-mode nav-bufs-mode fundamental-mode 
  "Nav-buf-mode is displaying and switching buffers."
  (setq mode-name "Nav buffers")
  (use-local-map nav-bufs-mode-map)
  (turn-on-font-lock)
  (setq buffer-read-only t)
  (nav-bufs-show-buffers))


(defun nav-bufs ()
  "Run nav-buf-mode on top of nav."
  (interactive)
  (select-window (nav-get-window nav-buffer-name))
  (nav-bufs-mode))


(provide 'nav-bufs)

;;; nav-bufs.el ends here