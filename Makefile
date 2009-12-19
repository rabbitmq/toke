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

SOURCE_DIR:=src
EBIN_DIR:=ebin
ERL_SOURCE:=$(wildcard $(SOURCE_DIR)/*.erl)
BEAM_TARGETS:=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(ERL_SOURCE))
LIBRARY:=$(EBIN_DIR)/libtoke.so
C_SOURCE:=$(wildcard $(SOURCE_DIR)/*.c)
C_HEADERS:=$(wildcard $(SOURCE_DIR)/*.h)
TARGETS:=$(BEAM_TARGETS) $(LIBRARY)

ERLC ?= erlc
ERL ?= erl
ERLC_OPTS:=-o $(EBIN_DIR) -Wall -v
ERL_OPTS:=-pa $(EBIN_DIR) +K true +A30

CC ?= gcc
CFLAGS ?=
CC_OPTS:=-Wall -O2 -shared -fpic -I $(SOURCE_DIR) -ltokyocabinet $(CFLAGS)

all: $(EBIN_DIR) $(TARGETS)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl 
	$(ERLC) $(ERLC_OPTS) $<

$(LIBRARY): $(C_SOURCE) $(C_HEADERS)
	$(CC) $(CC_OPTS) -o $@ $<

$(EBIN_DIR):
	mkdir -p $(EBIN_DIR)

clean:
	rm -f $(EBIN_DIR)/*.beam
	rm -f $(LIBRARY)

run: all
	$(ERL) $(ERL_OPTS)
