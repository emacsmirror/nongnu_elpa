CC ?= cc
CFLAGS += -fPIC -Wall -Wno-pointer-sign -Wno-unused-function -I.

UNAME := $(shell uname)
ifeq ($(UNAME),Darwin)
LDFLAGS += -dynamiclib -undefined dynamic_lookup
SOEXT = .dylib
else
LDFLAGS += -shared -Wl,--no-undefined
SOEXT = .so
endif

# OpenBSD's compiler driver omits libc when linking shared libraries.
ifeq ($(UNAME),OpenBSD)
LDLIBS += -lc
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
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(SRCS) $(MBED_FLAGS) $(LDLIBS)

$(INSTALL_DIR)/$(TARGET): $(TARGET)
	cp $< $@

# Standalone fault injection; never linked into the Emacs module.
TEST_TARGET = omemo-c-test
$(TEST_TARGET): ../tests/omemo-c-test.c $(filter-out jabber-omemo-core.c,$(SRCS)) picomemo/omemo.h picomemo/driver.h
	$(CC) $(CFLAGS) -UNDEBUG $(TEST_CFLAGS) -o $@ $< $(filter-out jabber-omemo-core.c,$(SRCS)) $(MBED_FLAGS) -Wl,--wrap=mbedtls_aes_crypt_cbc

RANDOM_TESTS = omemo-random-test omemo-random-openbsd-test
# Linux headers can simulate Android's syscall macro, not its native ABI.
ifeq ($(UNAME),Linux)
RANDOM_TESTS += omemo-random-android-test
endif
$(RANDOM_TESTS): ../tests/omemo-random-test.c $(SRCS) picomemo/omemo.h picomemo/driver.h
	$(CC) $(CFLAGS) -UNDEBUG $(TEST_CFLAGS) \
	  $(if $(filter omemo-random-openbsd-test,$@),-DTEST_OPENBSD) \
	  $(if $(filter omemo-random-android-test,$@),-DTEST_ANDROID) \
	  -o $@ $< $(filter-out jabber-omemo-core.c,$(SRCS)) $(MBED_FLAGS)

check-random: $(RANDOM_TESTS)
	@set -e; for test in $(RANDOM_TESTS); do ./$$test; done

check: $(TEST_TARGET) check-random
	./$(TEST_TARGET) encrypt
	./$(TEST_TARGET) store

.PHONY: check check-random

clean:
	rm -f $(TARGET) $(INSTALL_DIR)/$(TARGET) $(TEST_TARGET) $(RANDOM_TESTS)

.PHONY: all clean
