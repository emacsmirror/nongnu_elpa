/* Regression for the module's real RNG callback (not picomemo's weak fallback).
 * SPDX-License-Identifier: GPL-3.0-or-later
 * TEST_OPENBSD and TEST_ANDROID simulate APIs, not native OS builds.
 */
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <emacs-module.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <mbedtls/gcm.h>
#include "picomemo/omemo.h"
#if defined(__ANDROID__) || defined(TEST_ANDROID)
#include <stdarg.h>
#include <sys/syscall.h>
#elif !defined(__OpenBSD__)
#include <sys/random.h>
#endif

static int calls;
static int failure;
static size_t requested;

static ssize_t test_getrandom(void *p, size_t n, unsigned int flags)
{
    assert(flags == 0);
    calls++;
    requested = n;
    if (failure == 1) {
        errno = EIO;
        return -1;
    }
    memset(p, 0xa5, n);
    return failure == 2 ? (ssize_t)n - 1 : (ssize_t)n;
}

static void test_arc4random_buf(void *p, size_t n)
{
    calls++;
    requested = n;
    memset(p, 0xa5, n);
}

/* Retain the incoming patch's API semantics as a mutation-test oracle. */
static int test_getentropy(void *p, size_t n)
{
    if (n > 256) {
        errno = EIO;
        return -1;
    }
    test_arc4random_buf(p, n);
    return 0;
}

/* All host headers are loaded before selecting a simulated platform branch. */
#if defined(TEST_OPENBSD) && !defined(__OpenBSD__)
#define __OpenBSD__ 1
#endif
#if defined(TEST_ANDROID) && !defined(__ANDROID__)
#define __ANDROID__ 1
#endif
#if defined(__ANDROID__)
/* Production Android expands getrandom to syscall; intercept that boundary. */
static long test_syscall(long number, ...)
{
    assert(number == SYS_getrandom);
    va_list args;
    va_start(args, number);
    void *p = va_arg(args, void *);
    size_t n = va_arg(args, size_t);
    int flags = va_arg(args, int);
    va_end(args);
    return test_getrandom(p, n, flags);
}
#define syscall test_syscall
#else
#define getrandom test_getrandom
#endif
#define arc4random_buf test_arc4random_buf
#define getentropy test_getentropy
#ifndef OMEMO_CORE_SOURCE
#define OMEMO_CORE_SOURCE "../src/jabber-omemo-core.c"
#endif
#include OMEMO_CORE_SOURCE

int main(void)
{
    unsigned char buf[602];
    const size_t lengths[] = {0, 1, 32, 64, 257, 600};
    for (size_t i = 0; i < sizeof(lengths) / sizeof(lengths[0]); i++) {
        size_t n = lengths[i];
        memset(buf, 0x5a, sizeof(buf));
        calls = 0;
        assert(omemoRandom(buf + 1, n) == 0);
        assert(calls == 1 && requested == n);
        assert(buf[0] == 0x5a && buf[n + 1] == 0x5a);
        for (size_t j = 1; j <= n; j++)
            assert(buf[j] == 0xa5);
    }
#if defined(__OpenBSD__)
    /* A mistaken getrandom path must not pass the success checks above. */
    failure = 1;
    assert(omemoRandom(buf, 32) == 0);
    puts("OpenBSD API simulation: full fills and zero-success passed");
#else
    for (failure = 1; failure <= 2; failure++) {
        assert(omemoRandom(buf, 32) != 0);
        uint8_t key[32], iv[12], plaintext[1] = {0}, ciphertext[1];
        assert(omemoEncryptMessage(ciphertext, key, iv, plaintext, 1) != 0);
    }
#if defined(__ANDROID__)
    puts("Android syscall API simulation: full fills, errors and short reads passed");
#else
    puts("getrandom API: full fills, errors and short-read propagation passed");
#endif
#endif
    return 0;
}
