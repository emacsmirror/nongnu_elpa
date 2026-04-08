CC = gcc
CFLAGS += -fPIC -Wall -Wno-pointer-sign -Wno-unused-function -I.

UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
LDFLAGS += -dynamiclib -undefined dynamic_lookup
SOEXT = .dylib
else
LDFLAGS += -shared -Wl,--no-undefined
SOEXT = .so
endif

MBED_FLAGS ?= $(shell pkg-config --cflags --libs mbedcrypto 2>/dev/null)

SRCS = jabber-omemo-core.c picomemo/omemo.c picomemo/hacl.c
TARGET = jabber-omemo-core$(SOEXT)
INSTALL_DIR ?= $(if $(wildcard ../lisp),../lisp,..)

all: $(INSTALL_DIR)/$(TARGET)

picomemo/omemo.c:
	git -C .. submodule update --init src/picomemo

$(TARGET): $(SRCS) picomemo/omemo.h picomemo/hacl.h
ifeq ($(MBED_FLAGS),)
	$(error libmbedcrypto not found. Install libmbedtls-dev or set MBED_FLAGS manually)
endif
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(SRCS) $(MBED_FLAGS)

$(INSTALL_DIR)/$(TARGET): $(TARGET)
	cp $< $@

clean:
	rm -f $(TARGET) $(INSTALL_DIR)/$(TARGET)

.PHONY: all clean
