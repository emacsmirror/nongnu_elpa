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
# MBED_STATIC=1 links libmbedcrypto.a into the module, leaving only
# libc as a runtime dependency.  Use it when the build environment's
# library directory is not available to the Emacs that loads the
# module, e.g. a Nix development shell whose store paths can be
# garbage-collected, or an Emacs built against a different libc.
ifneq ($(MBED_STATIC),)
MBED_ARCHIVE = $(shell pkg-config --variable=libdir mbedcrypto 2>/dev/null)/libmbedcrypto.a
ifneq ($(UNAME),Darwin)
# Keep the archive's symbols private to the module.
MBED_HIDE = -Wl,--exclude-libs,libmbedcrypto.a
endif
MBED_FLAGS ?= $(shell test -f $(MBED_ARCHIVE) && $(MBED_CHECK) \
	&& pkg-config --cflags mbedcrypto 2>/dev/null && echo $(MBED_ARCHIVE) $(MBED_HIDE))
endif
MBED_FLAGS ?= $(shell $(MBED_CHECK) && pkg-config --cflags --libs mbedcrypto 2>/dev/null)

SRCS = jabber-omemo-core.c picomemo/omemo.c picomemo/hacl.c picomemo/mbedtls.c
TARGET = jabber-omemo-core$(SOEXT)
INSTALL_DIR ?= $(if $(wildcard ../lisp),../lisp,..)

all: $(INSTALL_DIR)/$(TARGET)

$(TARGET): $(SRCS) picomemo/omemo.h picomemo/driver.h
ifeq ($(MBED_FLAGS),)
	$(error libmbedcrypto >= $(MBED_MIN_VERSION) not found$(if $(MBED_STATIC), (MBED_STATIC needs libmbedcrypto.a)). Install libmbedtls-dev or set MBED_FLAGS manually)
endif
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $(SRCS) $(MBED_FLAGS) $(LDLIBS)

$(INSTALL_DIR)/$(TARGET): $(TARGET)
	cp $< $@

# Standalone fault injection; never linked into the Emacs module.
TEST_TARGET = omemo-c-test
# Rename only the backend call, not the real implementation in libmbedcrypto.
# This uses the C preprocessor instead of GNU ld's nonportable --wrap option.
CBC_TEST_OBJECT = omemo-c-test-mbedtls.o
$(CBC_TEST_OBJECT): picomemo/mbedtls.c picomemo/omemo.h picomemo/driver.h
	$(CC) $(CFLAGS) -UNDEBUG $(TEST_CFLAGS) $(MBED_FLAGS) \
	  -Dmbedtls_aes_crypt_cbc=__wrap_mbedtls_aes_crypt_cbc -c -o $@ $<

$(TEST_TARGET): ../tests/omemo-c-test.c $(filter-out jabber-omemo-core.c picomemo/mbedtls.c,$(SRCS)) $(CBC_TEST_OBJECT) picomemo/omemo.h picomemo/driver.h
	$(CC) $(CFLAGS) -UNDEBUG $(TEST_CFLAGS) \
	  -D__real_mbedtls_aes_crypt_cbc=mbedtls_aes_crypt_cbc -o $@ $< \
	  $(filter-out jabber-omemo-core.c picomemo/mbedtls.c,$(SRCS)) \
	  $(CBC_TEST_OBJECT) $(MBED_FLAGS) $(LDLIBS)

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
	rm -f $(TARGET) $(INSTALL_DIR)/$(TARGET) $(TEST_TARGET) $(CBC_TEST_OBJECT) $(RANDOM_TESTS)

.PHONY: all clean
