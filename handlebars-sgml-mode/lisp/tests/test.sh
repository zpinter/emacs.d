#!/bin/bash
set -e
test -e emacs-clean-test.el
rm -f test-results.txt
rm -rf test-dir/.elpa/handlebars-sgml-mode-*
if [ "$1" != "quick" ];then rm -rf test-dir;fi
emacs -nw -Q -l emacs-clean-test.el -f main ../handlebars-sgml-mode.el test-dir
echo -e "\n\n\nTest results:\n\n"
cat test-results.txt
