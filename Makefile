# Makefile to build RCP, such as it is...
# requires GNU make and GNU tar.
# This should be improved.

DIRS	= lisp texi

EMACS	= emacs
MAKEINFO	= makeinfo

.PHONY: MANIFEST

all:
	for a in ${DIRS}; do						\
	    $(MAKE) -C $$a "EMACS=$(EMACS)" "MAKEINFO=$(MAKEINFO)" all;	\
	done

clean:
	rm -f MANIFEST rcp.tar.gz
	for a in ${DIRS}; do						\
	    $(MAKE) -C $$a "EMACS=$(EMACS)" "MAKEINFO=$(MAKEINFO)" clean; \
	done

MANIFEST:
	find . \( -name CVS -prune \) -o \( -name tmp -prune \)	\
		-o -type f \! -name "*~"			\
		-a \! -name "*.elc" -a \! -name "*.aux"		\
		-a \! -name "*.cp" -a \! -name "*.fn"		\
		-a \! -name "*.vr" -a \! -name "*.tp"		\
		-a \! -name "*.ky" -a \! -name "*.pg"		\
		-a \! -name "*.tmp" -a \! -name "*.log"		\
		-a \! -name "*.toc" -a \! -name "*,v"		\
		-a \! -name "*.tar.gz"				\
		-print > MANIFEST

tar: MANIFEST
	tar cvpfzT rcp.tar.gz MANIFEST

dist: tar
	install -m644 rcp.tar.gz /home-local/ftp/pub/src/emacs
	install -m644 lisp/rcp.el /home-local/ftp/pub/src/emacs
