INSTALL=install -m644
FTPDIR=/home-local/ftp/pub/src/emacs
SLISP=/app/unido-i06/sun4_56/edit/emacs/20.5/share/emacs/site-lisp

EMACS ?= emacs
EVAL = --eval "(add-to-list 'load-path \".\")" 

ifdef USE_CONTRIB
  EVAL += --eval "(add-to-list 'load-path \"./../contrib\")"
endif

EM = $(EMACS) -batch $(EVAL) -f batch-byte-compile 

all: prepversion tramp

prepversion:
	@echo "(defconst tramp-version \"$(VERSION)\"" > tramp-version.el
	@echo "  \"This version of tramp.\")" >> tramp-version.el
	@echo "" >> tramp-version.el
	@echo "(defun tramp-version (arg)" >> tramp-version.el
	@echo "  \"Print version number of tramp.el in minibuffer or current buffer.\"" >> tramp-version.el
	@echo "  (interactive \"P\")" >> tramp-version.el
	@echo "  (if arg (insert tramp-version) (message tramp-version)))" >> tramp-version.el
	@echo "" >> tramp-version.el
	@echo "(provide 'tramp-version)" >> tramp-version.el

tramp: tramp.elc tramp-vc.elc tramp-util.elc

install: all
	$(INSTALL) tramp.el $(FTPDIR)

site-lisp: all
	/usr/ucb/$(INSTALL) tramp.el $(SLISP)
	cd $(SLISP) && emacs-20.5 -batch -f batch-byte-compile tramp.el

clean:
	$(RM) *.elc tramp-version.el


%.elc: %.el
	$(EM) $<

.SUFFIXES: .elc .el
