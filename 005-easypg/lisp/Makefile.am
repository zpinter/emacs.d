AUTOMAKE_OPTIONS = no-dependencies

EXTRA_DIST = COMPILE epg-config.el epg.el epa.el epa-dired.el \
epa-file.el epa-mail.el epa-setup.el

CLEANFILES = auto-autoloads.el* custom-load.el *.elc
DISTCLEANFILES = epg-package-info.el

info_TEXINFOS = epa.texi

FLAGS ?= -batch -q -no-site-file

all-local: elc

elc:
	$(EMACS) $(FLAGS) -l COMPILE -f epg-compile

install-data-local: elc
	$(EMACS) $(FLAGS) -l COMPILE -f epg-install $(lispdir) # $(MAKE)

package:
	$(XEMACS) $(FLAGS) -l COMPILE -f epg-compile-package

install-package: package
	$(XEMACS) $(FLAGS) -l COMPILE -f epg-install-package \
		$(PACKAGEDIR) # $(MAKE)
	$(MAKE) infodir=$(PACKAGEDIR)/info install

compile-individually:
	@for i in `$(EMACS) $(FLAGS) -l COMPILE -f epg-examine`; do \
		echo $(EMACS) $(FLAGS) -l COMPILE \
			-f epg-compile-module $$i; \
		$(EMACS) $(FLAGS) -l COMPILE \
			-f epg-compile-module $$i; \
	done
