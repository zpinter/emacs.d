(set 'saved-macros-path (concat zconfig-current-module-dir "/init.el"))

(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file saved-macros-path)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer


(fset 'find-collection
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 19 58 left 67108927 134217847 24 6 99 111 108 return 134217788 19 25 1] 0 "%d")) arg)))


(fset 'find-collection-usages
	(lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 19 58 left 67108927 134217847 134217848 114 103 return 25 1 117 115 101 95 99 111 108 108 101 99 116 105 111 110 32 return 42 46 114 98 return return 24 111 tab] 0 "%d")) arg)))

