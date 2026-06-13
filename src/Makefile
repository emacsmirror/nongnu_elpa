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

MBED_MIN_VERSION = 3.0.0
MBED_CHECK = pkg-config --atleast-version=$(MBED_MIN_VERSION) mbedcrypto 2>/dev/null
MBED_FLAGS ?= $(shell $(MBED_CHECK) && pkg-config --cflags --libs mbedcrypto 2>/dev/null)

SRCS = jabber-omemo-core.c picomemo/omemo.c picomemo/hacl.c picomemo/mbedtls.c
TARGET = jabber-omemo-core$(SOEXT)
INSTALL_DIR ?= $(if $(wildcard ../lisp),../lisp,..)

all: $(INSTALL_DIR)/$(TARGET)

$(TARGET): $(SRCS) picomemo/omemo.h picomemo/driver.h
ifeq ($(MBED_FLAGS),)
	$(error libmbedcrypto >= $(MBED_MIN_VERSION) not found. Install libmbedtls-dev or set MBED_FLAGS manually)
endif
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(SRCS) $(MBED_FLAGS)

$(INSTALL_DIR)/$(TARGET): $(TARGET)
	cp $< $@

clean:
	rm -f $(TARGET) $(INSTALL_DIR)/$(TARGET)

.PHONY: all clean
