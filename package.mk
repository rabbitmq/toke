STANDALONE_TEST_COMMANDS:=test_toke:test()

C_SOURCE_DIR:=$(PACKAGE_DIR)/c_src
LIBRARY:=$(C_SOURCE_DIR)/libtoke.so
C_SOURCE:=$(wildcard $(C_SOURCE_DIR)/*.c)
C_HEADERS:=$(wildcard $(C_SOURCE_DIR)/*.h)

CC ?= gcc
CFLAGS ?=
CC_OPTS:=-Wall -pedantic -std=c99 -O2 -shared -fpic -ltokyocabinet $(CFLAGS)

CONSTRUCT_APP_PREREQS:=$(LIBRARY)
define construct_app_commands
	mkdir -p $(APP_DIR)/priv
	cp $(LIBRARY) $(APP_DIR)/priv
endef

define package_rules

$(LIBRARY): $(C_SOURCE) $(C_HEADERS)
	$(CC) $(CC_OPTS) -o $$@ $(C_SOURCE)

$(PACKAGE_DIR)+clean::
	rm -rf $(LIBRARY)

# This is disgusting. Why can't I just depend on _and_ unpack
# $(EZ_FILE) ? Instead we have .done. targets to confuse matters...
# The reason for unpacking is that we can't dynamically load libraries
# that are within .ez files.
$(PACKAGE_DIR)+pre-run:: $(PACKAGE_DIR)/dist/.done.$(PACKAGE_VERSION)
	rm -rf $(PACKAGE_DIR)/dist/$(APP_NAME)-$(PACKAGE_VERSION)
	unzip $(PACKAGE_DIR)/dist/$(APP_NAME)-$(PACKAGE_VERSION).ez -d $(PACKAGE_DIR)/dist

$(PACKAGE_DIR)+pre-test:: $(PACKAGE_DIR)+pre-run

endef
