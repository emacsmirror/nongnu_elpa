CC = gcc
CFLAGS += -fPIC -Wall -Wno-pointer-sign -Wno-unused-function -I.

MBED_FLAGS ?= $(shell pkg-config --cflags --libs mbedtls mbedcrypto)

SRCS = jabber-omemo-core.c picomemo/omemo.c picomemo/hacl.c
TARGET = jabber-omemo-core.so
INSTALL_DIR = ../lisp

all: $(INSTALL_DIR)/$(TARGET)

picomemo/omemo.c:
	git -C .. submodule update --init src/picomemo

$(TARGET): $(SRCS) picomemo/omemo.h picomemo/hacl.h
	$(CC) -shared $(CFLAGS) -o $@ $(SRCS) $(MBED_FLAGS)

$(INSTALL_DIR)/$(TARGET): $(TARGET)
	cp $< $@

clean:
	rm -f $(TARGET) $(INSTALL_DIR)/$(TARGET)

.PHONY: all clean
