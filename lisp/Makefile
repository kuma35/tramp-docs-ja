INSTALL=install -m644
FTPDIR=/home-local/ftp/pub/src/emacs
SLISP=/app/unido-i06/sun4_56/edit/emacs/20.4/share/emacs/site-lisp

all: rcp

rcp: rcp.elc

rcp.elc: rcp.el
	emacs -batch -f batch-byte-compile rcp.el

install: all
	$(INSTALL) rcp.el $(FTPDIR)

site-lisp: all
	/usr/ucb/$(INSTALL) rcp.el $(SLISP)
	cd $(SLISP) && emacs-20.4 -batch -f batch-byte-compile rcp.el

clean:
	$(RM) rcp.elc
