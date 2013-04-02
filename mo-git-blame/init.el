(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

(global-set-key (kbd "C-c h") 'mo-git-blame-current)

;;git://github.com/mbunkus/mo-git-blame.git
