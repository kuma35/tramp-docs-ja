# Makefile, does build info from texi
# Requires makeinfo, assumes it to be in the path...

INSTALL=install -m644
SINFO=/app/unido-i06/sun4_56/edit/emacs/20.4/share/emacs/site-info

all: rcp.info

clean:
	-rm -f *.info *~

rcp.info: rcp.texi
	makeinfo --footnote-style=end --no-split rcp.texi

site-info: rcp.info
	/usr/ucb/$(INSTALL) rcp.info $(SINFO)
