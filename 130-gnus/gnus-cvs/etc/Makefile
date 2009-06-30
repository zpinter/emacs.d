prefix = 
datarootdir = ${prefix}/share
datadir = ${datarootdir}
infodir = ${datarootdir}/info
srcdir = .
subdir = etc
top_srcdir = ..
lispdir = $(datadir)/emacs/site-lisp/gnus
etcdir = $(lispdir)/../../etc


EMACS=emacs
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
SHELL = /bin/sh

install:
	$(SHELL) $(top_srcdir)/mkinstalldirs "$(etcdir)"
	@cd $(srcdir) \
	&& for p in gnus-tut.txt; do \
	  echo "$(INSTALL_DATA) \"$$p $(etcdir)/$$p\""; \
	  $(INSTALL_DATA) $$p "$(etcdir)/$$p"; \
	done
	$(SHELL) $(top_srcdir)/mkinstalldirs "$(etcdir)/images/gnus" "$(etcdir)/images/mail"
	@cd $(srcdir) \
	&& for p in images/gnus/*.pbm images/gnus/*.png images/gnus/*.svg \
		    images/gnus/*.xbm images/gnus/*.xpm images/gnus/x-splash \
		    images/mail/*.pbm images/mail/*.png images/mail/*.svg \
		    images/mail/*.xbm images/mail/*.xpm \
		    images/*.pbm images/*.png images/*.svg images/*.xbm \
		    images/*.xpm; do \
	  if test -f $$p; then \
	    echo "$(INSTALL_DATA) $$p \"$(etcdir)/$$p\""; \
	    $(INSTALL_DATA) $$p "$(etcdir)/$$p"; \
	  fi; \
	done
	$(SHELL) $(top_srcdir)/mkinstalldirs "$(etcdir)/images/smilies" \
	  "$(etcdir)/images/smilies/grayscale" "$(etcdir)/images/smilies/medium"
	@cd $(srcdir) \
	&& for p in images/smilies/*.pbm images/smilies/*.png \
		    images/smilies/*.svg images/smilies/*.xbm \
		    images/smilies/*.xpm \
		    images/smilies/grayscale/*.pbm \
		    images/smilies/grayscale/*.png \
		    images/smilies/grayscale/*.svg \
		    images/smilies/grayscale/*.xbm \
		    images/smilies/grayscale/*.xpm \
		    images/smilies/medium/*.pbm images/smilies/medium/*.png \
		    images/smilies/medium/*.svg images/smilies/medium/*.xbm \
		    images/smilies/medium/*.xpm; do \
	  if test -f $$p; then \
	    echo "$(INSTALL_DATA) $$p \"$(etcdir)/$$p\""; \
	    $(INSTALL_DATA) $$p "$(etcdir)/$$p"; \
	  fi; \
	done

uninstall:
	rm -f "$(etcdir)/gnus-tut.txt"
	@cd $(srcdir) \
	&& for p in images/gnus/*.pbm images/gnus/*.png images/gnus/*.svg \
		    images/gnus/*.xbm images/gnus/*.xpm images/gnus/x-splash \
		    images/mail/*.pbm images/mail/*.png images/mail/*.svg \
		    images/mail/*.xbm images/mail/*.xpm \
		    images/*.pbm images/*.png images/*.svg images/*.xbm \
		    images/*.xpm; do \
	  if test -f $$p; then \
	    echo "rm -f \"$(etcdir)/$$p\""; \
	    rm -f "$(etcdir)/$$p"; \
	  fi; \
	done
	rmdir "$(etcdir)/images/gnus" 2> /dev/null || true
	@cd $(srcdir) \
	&& for p in images/smilies/*.pbm images/smilies/*.png \
		    images/smilies/*.svg images/smilies/*.xbm \
		    images/smilies/*.xpm \
		    images/smilies/grayscale/*.pbm \
		    images/smilies/grayscale/*.png \
		    images/smilies/grayscale/*.svg \
		    images/smilies/grayscale/*.xbm \
		    images/smilies/grayscale/*.xpm \
		    images/smilies/medium/*.pbm images/smilies/medium/*.png \
		    images/smilies/medium/*.svg images/smilies/medium/*.xbm \
		    images/smilies/medium/*.xpm; do \
	  if test -f $$p; then \
	    echo "rm -f \"$(etcdir)/$$p\""; \
	    rm -f "$(etcdir)/$$p"; \
	  fi; \
	done
	rmdir "$(etcdir)/images/smilies" 2> /dev/null || true
	rmdir "$(etcdir)/images" 2> /dev/null || true

Makefile: $(srcdir)/Makefile.in ../config.status
	cd .. \
	  && CONFIG_FILES=$(subdir)/$@ CONFIG_HEADERS= $(SHELL) ./config.status

clean:
	rm -f *~

distclean: clean
	rm -f Makefile

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
