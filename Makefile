# Makefile to build RCP, such as it is...
# requires GNU make and GNU tar.

DIRS	= lisp texi

EMACS	= emacs
MAKEINFO	= makeinfo

.PHONY: MANIFEST

all:
	for a in ${DIRS}; do						\
	    $(MAKE) -C $$a "EMACS=$(EMACS)" "MAKEINFO=$(MAKEINFO)" all;	\
	done

clean:
	for a in ${DIRS}; do							\
	    $(MAKE) -C $$a "EMACS=$(EMACS)" "MAKEINFO=$(MAKEINFO)" clean;	\
	done

MANIFEST:
	find . -type f \! -name "*~" -a \! -name "*.elc" \
		-a \! -name "*,v" -a \! -name "*.tar.gz" -print > MANIFEST

MANIFEST.src:
	find . -type f \! -name "*~" -a \! -name "*.elc" \
		-a \! -name "*.tar.gz" -print > MANIFEST.src

dist: MANIFEST MANIFEST.src
	tar cvpfzT rcp.tar.gz MANIFEST
	tar cvpfzT rcp-src.tar.gz MANIFEST.src
	install -m644 rcp.tar.gz /home-local/ftp/pub/src/emacs
	install -m644 rcp-src.tar.gz /home-local/ftp/pub/src/emacs
	install -m644 lisp/rcp.el /home-local/ftp/pub/src/emacs
