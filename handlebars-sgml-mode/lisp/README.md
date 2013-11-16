# Handlebars Sgml Mode

* Git: http://github.com/jacott/handlebars-sgml-mode
* Author: Geoff Jacobsen
* Copyright (C) 1992, 1995-1996, 1998, 2001-2012 Free Software Foundation, Inc.
* License: GNU GPLv3

### Description

This package provides Handlebars support for Emacs sgml-mode (html).

It provides indenting and parsing of handlebars markup: `{{}}`, `{{#block}}{{/block}`, `{{!comment}}`, ...

Certain `sgml-mode` functions are overwritten to provide this
package. You can choose to always use the overwritten functions or
have the original functions called unless in `handlebars-html-minor-mode`

This code is a modification of the sgml-mode package. Original code
belongs to the Free Software Foundation.
(see sgml-mode.el in GNU Emacs)


### Compatibility

Other modes, such as rhtml-mode, also hack sgml-mode. In order to
co-exist with such modes this package should be required last;
unless the other packages also support invoking the original functions.

### Install and usage

This is an ELPA package available from either [Marmalade](http://marmalade-repo.org/) or
[MELPA](http://melpa.milkbox.net/). To
install the package use: `M-x install-package
handlebars-sgml-mode`. Otherwise copy `handlebars-sgml-mode.el` into your load-path.


```lisp
(require 'handlebars-sgml-mode)

;; decide how to use handlebars-html-mode
;; (handlebars-use-mode 'global) ;; always use handlebars-minor-mode
(handlebars-use-mode 'off)       ;; Never use handlebars-mode (the default)
;; (handlebars-use-mode 'minor)  ;; Only use if in 'handlebars-sgml-minor-mode

```

### Testing

```sh
$ cd tests
$ ./test.sh
$ # run ./test.sh quick for faster (less extensive) testing
```
