# Makefile, does build info from texi
# Requires makeinfo, assumes it to be in the path...
# Only works with TeXinfo 4.x.  (It may be sufficient to replace
# @command with @code to make it work with TeXinfo 3.x.)

INSTALL=install -m644
SINFO=/app/unido-i06/sun4_56/edit/emacs/20.5/share/emacs/site-info

all: rcp.info dvi rcp.html

clean:
	-rm -f *.info *.html *~

rcp.info: rcp.texi
	$(MAKEINFO) --footnote-style=end --no-split rcp.texi

dvi: rcp.dvi

rcp.dvi: rcp.texi
	texi2dvi rcp.texi

rcp.html: rcp.texi
	$(MAKEINFO) --html rcp.texi

site-info: rcp.info
	/usr/ucb/$(INSTALL) rcp.info $(SINFO)

install-html: rcp.html rcp_ja.html
	$(INSTALL) rcp.html rcp_ja.html $$HOME/public-html/emacs

# For Japanese manual
rcp_ja.info: rcp_ja.texi
	$(MAKEINFO) --footnote-style=end --no-split rcp_ja.texi

rcp_ja.html: rcp_ja.texi
	$(MAKEINFO) --html rcp_ja.texi
