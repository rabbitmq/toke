#   The contents of this file are subject to the Mozilla Public License
#   Version 1.1 (the "License"); you may not use this file except in
#   compliance with the License. You may obtain a copy of the License at
#   http://www.mozilla.org/MPL/
#
#   Software distributed under the License is distributed on an "AS IS"
#   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
#   License for the specific language governing rights and limitations
#   under the License.
#
#   The Original Code is Toke.
#
#   The Initial Developers of the Original Code are LShift Ltd.
#
#   Portions created by LShift Ltd are Copyright (C) 2009 LShift Ltd.
#
#   All Rights Reserved.
#
#   Contributor(s): ______________________________________.
#

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

endef
