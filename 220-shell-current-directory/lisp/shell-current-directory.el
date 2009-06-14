;;; shell-current-directory.el --- create new shell based on buffer directory

;; Copyrght (C) 2001-2007 Daniel Polani

;; Author: Daniel Polani
;; Submitted by Terrence Brannon <metaperl@gmail.com>
;; Created: 24 Sep 2007
;; Version: 0.1
;; Keywords: shell, comint

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Change Log:
;; 20 Jun 2001 - submitted to gnu.emacs.sources - http://groups.google.com/group/gnu.emacs.sources/browse_thread/thread/a4a2b76ffc75e4eb/220183a660260db8?lnk=gst&q=daniel+polani&rnum=3#220183a660260db8
;; 24 Sep 2007 - submtted to the Emacs Lisp Package Archive

;;; Commentary
;; To use this, put shell-current-directory.el somewhere on your load-path.  
;; Then add this to your .emacs:
;;
;;    (require 'shell-current-directory)
;;
;; Then whenever you want to open a shell in the same directory as the buffer
;; you are visiting, simply type: M-x shell-current-directory



(defun directory-shell-buffer-name ()

  "The name of a shell buffer pertaining to DIR."

  (concat "*" 
  (file-name-nondirectory 
   (directory-file-name (expand-file-name default-directory))) 
  "-shell*"))



(defun directory-shell-buffer ()

  "Return a buffer with the current default directory a process.
This is hopefully a shell one - this is a q/d heuristic."
  
  (let ((buflist (buffer-list))
found
buffer
buffer-directory
bufproc
retval)
    (while (and (not found) buflist)
      (setq buffer (pop buflist))
      (setq buffer-directory
    (save-excursion 
      (set-buffer buffer)
      default-directory))

      (setq bufproc (get-buffer-process buffer))

      (if bufproc
  (if (and (string-match "^\\(bash\\|shell\\)\\(<[0-9]*>\\)?$" 
(process-name bufproc))
   (string= (expand-file-name default-directory)
    (expand-file-name buffer-directory)))
      (setq found t))))

    (if found
buffer
      nil)))

      
    
    

(defun shell-current-directory ()

  "Create a shell pertaining to the current directory."

  (interactive)
  (let ((current-shell-buffer (directory-shell-buffer))
original-shell-buffer)

    (if current-shell-buffer
(pop-to-buffer current-shell-buffer)

      ;; no current process buffer is active
      ;; if *shell* is already used, store it
      (if (buffer-live-p "*shell*")
(save-excursion
  (set-buffer "*shell*")
  (setq original-shell-buffer (rename-uniquely))))
      
      ;; and create a new shell process with the current directory

      (shell)
      (rename-buffer (directory-shell-buffer-name) t) ; unique
    
      (if original-shell-buffer ; there has been a standard
; *shell* buffer before,
; restore it

  (save-excursion
    (set-buffer original-shell-buffer)
    (rename-buffer "*shell*"))))))


(provide 'shell-current-directory)

;;; shell-current-directory ends here.

