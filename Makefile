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
PRIV_DIR:=$(PACKAGE_DIR)/priv
LIBRARY:=$(PRIV_DIR)/libtoke.so
C_SOURCE:=$(wildcard $(C_SOURCE_DIR)/*.c)
C_HEADERS:=$(wildcard $(C_SOURCE_DIR)/*.h)
EXTRA_TARGETS:=$(LIBRARY)
EXTRA_PACKAGE_DIRS:=$(PRIV_DIR)

CC ?= gcc
CFLAGS ?=
CC_OPTS:=-Wall -pedantic -std=c99 -O2 -shared -fpic -ltokyocabinet $(CFLAGS)

$(LIBRARY): $(C_SOURCE) $(C_HEADERS) | $(PRIV_DIR)
	$(CC) $(CC_OPTS) -o $@ $<

$(PRIV_DIR):
	mkdir -p $@

$(PACKAGE_DIR)/clean_RM:=$(PRIV_DIR)
$(PACKAGE_DIR)/clean::
	rm -rf $($@_RM)

include ../include.mk
