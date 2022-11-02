PROJ=$(HOME)/work/tramp-docs-ja
TEXI_PO=$(PROJ)/texi-po
TEXI_JA=$(PROJ)/texi-ja
TRAMP_HTML=tramp.html
INDEX_HTML=index.html
TARGET_DIR=$(PROJ)/docs
TARGET_HTML_DIR=$(TARGET_DIR)/htmldocs

all: $(TARGET_DIR)/$(INDEX_HTML) $(TARGET_HTML_DIR)/$(TRAMP_HTML)

$(TARGET_DIR)/$(INDEX_HTML) : $(TEXI_PO)/$(INDEX_HTML)
	cp -v $< $@

$(TARGET_HTML_DIR)/$(TRAMP_HTML) : $(TEXI_JA)/$(TRAMP_HTML)
	cp -v $< $@

.PHONY: all
