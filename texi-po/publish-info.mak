PROJ=$(HOME)/work/tramp-docs-ja
TRAMP_INFO=tramp
DIR_FILE=dir
TARGET_DIR=$(PROJ)/docs/info

all: $(TARGET_DIR)/$(TRAMP_INFO) $(TARGET_DIR)/$(DIR_FILE)

$(DIR_FILE) : $(TRAMP_INFO)
	install-info --dir-file=$(DIR_FILE) --info-file=$(TRAMP_INFO)

$(TARGET_DIR)/$(TRAMP_INFO) : $(TRAMP_INFO)
	cp -v $< $@

$(TARGET_DIR)/$(DIR_FILE) : $(DIR_FILE)
	cp -v $< $@

.PHONY: all
