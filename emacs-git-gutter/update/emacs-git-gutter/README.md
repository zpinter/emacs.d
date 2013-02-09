# git-gutter.el

## Introduction
`git-gutter.el` is port of [GitGutter](https://github.com/jisaacks/GitGutter)
which is a plugin of Sublime Text2.


If you use fringe style(not linum style), please see [git-gutter-fringe](https://github.com/syohex/emacs-git-gutter-fringe)


## Screenshot

![git-gutter.el](https://github.com/syohex/emacs-git-gutter/raw/master/image/git-gutter1.png)


## Requirements

* Emacs 24 or higher

## Installation

You can install `git-gutter.el` from [MELPA](https://github.com/milkypostman/melpa.git) with package.el
(`M-x package-install git-gutter`).

And you can also install it with [el-get](https://github.com/dimitri/el-get).


## Similar Project

* [diff-hl](https://github.com/dgutov/diff-hl)

`diff-hl` has more features than `git-gutter.el`.


## Global Minor Mode and Minor Mode

`git-gutter.el` provides global minor-mode(`global-git-gutter-mode`) and minor-mode(`git-gutter-mode`).

If you want to use `git-gutter` for files in git repository.
You add following s-exp in your configuration file(`~/.emacs.d/init.el` or `~/.emacs`).

````elisp
(global-git-gutter-mode t)
````

Other case, you want to use `git-gutter` for some files, you can use `git-gutter-mode`.
Following example of enabling `git-gutter` for some mode.

````elisp
(add-hook 'ruby-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
````


## Basic Usage

`git-gutter.el` provides following commands.

Show changes from last commit

    M-x git-gutter

Clear changes

    M-x git-gutter:clear

Toggle git-gutter

    M-x git-gutter:toggle


## Sample Configuration

````elisp
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you enable git-gutter-mode for some modes
(add-hook 'ruby-mode-hook 'git-gutter-mode)

;; bind git-gutter toggle command
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
````


## Customize

You can pass `git diff` option to set `git-gutter:diff-option`.

````elisp
;; ignore all spaces
(setq git-gutter:diff-option "-w")
````

You can change the signs and those faces.

````elisp
(setq git-gutter:modified-sign "  ") ;; two space
(setq git-gutter:added-sign "++")    ;; multiple character is OK
(setq git-gutter:deleted-sign "--")

(set-face-background 'git-gutter:modified "purple") ;; background color
(set-face-foreground 'git-gutter:added "green")
(set-face-foreground 'git-gutter:deleted "red")
````

### Screenshot of above customization

![git-gutter-multichar](https://github.com/syohex/emacs-git-gutter/raw/master/image/git-gutter-multichar.png)


### Using full width characters

Emacs has `char-width` function which returns character width.
`git-gutter.el` uses it for calculating character length of the signs.
But `char-width` does not work for some full-width characters.
So you should explicitly specify window width, if you use full-width
character.

```` elisp
(setq git-gutter:window-width 2)
(setq git-gutter:modified-sign "☁")
(setq git-gutter:added-sign "☀")
(setq git-gutter:deleted-sign "☂")
````

### Screenshot of above customization
![git-gutter-fullwidth](https://github.com/syohex/emacs-git-gutter/raw/master/image/git-gutter-fullwidth.png)


## Implement your own git-gutter

You can create your own git-gutter to implement 2 functions.

### view function

View function view diff informations to current buffer.
View function takes list of diff informations(`diffinfos`). `diffinfos`
are list of plist(`diffinfo`).  `diffinfo` has following property.


property    | about
------------|-------------------------------------------------------
:type       | diff type('added, 'deleted, 'modified)
:start-line | line number of changed start
:end-line   | line number of changed end(Only 'added and 'modified)


Set view function variable `git-gutter:view-diff-function`.


### clear function

Clear function clears diff informations.
Clear function takes no arguments.

Set clear function variable `git-gutter:view-diff-function`.


## Sample implementation

* [git-gutter-fringe](https://github.com/syohex/emacs-git-gutter-fringe).
