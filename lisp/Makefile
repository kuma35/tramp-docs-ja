INSTALL=install -m644
FTPDIR=/home-local/ftp/pub/src/emacs
SLISP=/app/unido-i06/sun4_56/edit/emacs/20.5/share/emacs/site-lisp

all: rcp

rcp: rcp.elc

format-spec.elc: format-spec.el
	$(EMACS) -batch -f batch-byte-comile format-spec.el

rcp.elc: rcp.el
	$(EMACS) -batch --eval "(add-to-list 'load-path \".\")" -f batch-byte-compile rcp.el

install: all
	$(INSTALL) rcp.el $(FTPDIR)

site-lisp: all
	/usr/ucb/$(INSTALL) rcp.el $(SLISP)
	cd $(SLISP) && emacs-20.5 -batch -f batch-byte-compile rcp.el

clean:
	$(RM) rcp.elc format-spec.elc
