EMACS=emacs
BATCH=$(EMACS) -batch -q -no-site-file \
	-eval "(setq load-path (cons (expand-file-name \".\") load-path))" \
	-eval "(add-to-list 'load-path \"~/.emacs.d/helm\")" \
	-eval "(add-to-list 'load-path \"~/.emacs.d/elpa/magit-20120601/\")"

test:
	@$(BATCH) -l tests/test-helpers.el -l tests/helm-git-tests.el -f ert-run-tests-batch-and-exit
