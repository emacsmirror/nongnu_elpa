/*
 * jabber-omemo-core.c -- Emacs dynamic module wrapping picomemo
 *
 * Exposes OMEMO 0.3 (eu.siacs.conversations.axolotl) store lifecycle,
 * bundle extraction, key rotation, and message encrypt/decrypt to Elisp.
 * Also provides AES-256-GCM encrypt/decrypt for XEP-0454 media sharing.
 *
 * Copyright 2026 Thanos Apollo
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * picomemo is ISC-licensed; see src/picomemo/LICENSE.
 */

#include <emacs-module.h>
#include <stdlib.h>
#include <string.h>
#include <sys/random.h>
#if defined(__ANDROID__)
#include <unistd.h>
#include <sys/syscall.h>
#define getrandom(buf,buflen,flags) syscall(SYS_getrandom,buf,buflen,flags)
#endif

#include <mbedtls/gcm.h>

#include "picomemo/omemo.h"

int plugin_is_GPL_compatible;

/*  picomemo callbacks  */

int omemoRandom(void *p, size_t n)
{
    return getrandom(p, n, 0) != (ssize_t)n;
}

/* Skipped-message-key registry.

   picomemo hands skipped ratchet keys to the embedder through
   omemoStoreMessageKey and asks for them back in omemoLoadMessageKey.
   Both fire synchronously inside omemoDecryptKey, where no emacs_env
   is available, so keys live in a malloc'd per-session list here.
   The list is serialized with the native session so skipped keys
   survive restarts without separate database writes. */

struct skipped_key {
    uint32_t nr;
    uint8_t dh[32];
    uint8_t mk[32];
};

struct native_session {
    struct omemoSession session;
    struct skipped_key *keys;
    size_t count, cap;
    size_t decrypt_new_count;
};

/* Upper bound on retained skipped keys per session; a peer jumping
   further ahead than this in one ratchet aborts the decrypt with
   OMEMO_ESTORE instead of allocating without limit. */
#define SKIPPED_KEYS_MAX 1000

#define SESSION_MAGIC "JOMEMO\0\1"
#define SESSION_MAGIC_SIZE 8
#define SESSION_VERSION 1
#define SESSION_HEADER_SIZE 20
#define SKIPPED_KEY_SIZE 68

static void
skipped_clear(void *ptr, size_t size)
{
    volatile unsigned char *p = ptr;
    while (size--)
        *p++ = 0;
}

static struct native_session *
native_session(struct omemoSession *session)
{
    return (struct native_session *)session;
}

static void
skipped_drop(struct native_session *session)
{
    if (session->keys) {
        skipped_clear(session->keys,
                      session->cap * sizeof(struct skipped_key));
        free(session->keys);
    }
    session->keys = NULL;
    session->count = session->cap = 0;
}

static int
skipped_add(struct native_session *session, uint32_t nr,
            const uint8_t *dh, const uint8_t *mk)
{
    if (session->count == SKIPPED_KEYS_MAX) {
        skipped_clear(&session->keys[0], sizeof session->keys[0]);
        memmove(&session->keys[0], &session->keys[1],
                (session->count - 1) * sizeof session->keys[0]);
        session->count--;
    }
    if (session->count == session->cap) {
        size_t ncap = session->cap ? session->cap * 2 : 16;
        if (ncap > SKIPPED_KEYS_MAX)
            ncap = SKIPPED_KEYS_MAX;
        struct skipped_key *n = realloc(session->keys, ncap * sizeof *n);
        if (!n)
            return 1;
        memset(n + session->cap, 0,
               (ncap - session->cap) * sizeof *n);
        session->keys = n;
        session->cap = ncap;
    }
    session->keys[session->count].nr = nr;
    memcpy(session->keys[session->count].dh, dh, 32);
    memcpy(session->keys[session->count].mk, mk, 32);
    session->count++;
    return 0;
}

int omemoLoadMessageKey(struct omemoSession *s, struct omemoMessageKey *k)
{
    struct native_session *session = native_session(s);
    for (size_t i = 0; i < session->count; i++) {
        struct skipped_key *sk = &session->keys[i];
        if (sk->nr == k->nr && !memcmp(sk->dh, k->dh, 32)) {
            memcpy(k->mk, sk->mk, 32);
            return 0;
        }
    }
    return 1; /* not found */
}

int omemoRemoveMessageKey(struct omemoSession *s,
                          const struct omemoMessageKey *k)
{
    struct native_session *session = native_session(s);
    for (size_t i = 0; i < session->count; i++) {
        struct skipped_key *sk = &session->keys[i];
        if (sk->nr == k->nr && !memcmp(sk->dh, k->dh, 32)) {
            skipped_clear(sk, sizeof *sk);
            memmove(sk, sk + 1,
                    (session->count - i - 1) * sizeof *sk);
            skipped_clear(&session->keys[session->count - 1],
                          sizeof(struct skipped_key));
            session->count--;
            return 0;
        }
    }
    return OMEMO_ESTORE;
}

int omemoStoreMessageKey(struct omemoSession *s,
                         const struct omemoMessageKey *k, uint64_t n)
{
    (void)n;
    struct native_session *session = native_session(s);
    if (session->decrypt_new_count >= SKIPPED_KEYS_MAX ||
        skipped_add(session, k->nr, k->dh, k->mk))
        return OMEMO_ESTORE;
    session->decrypt_new_count++;
    return 0;
}

/*  Emacs helpers  */

static emacs_value Qnil_v, Qt_v;
static emacs_value Qjabber_omemo_error;
static emacs_value Qidentity_key, Qsigned_pre_key, Qsigned_pre_key_id;
static emacs_value Qsignature, Qpre_keys;
static emacs_value Qkey, Qiv, Qciphertext;
static emacs_value Qdata, Qpre_key_p;

static void
bind_function(emacs_env *env, const char *name, emacs_value func)
{
    emacs_value sym = env->intern(env, name);
    emacs_value args[] = { sym, func };
    env->funcall(env, env->intern(env, "defalias"), 2, args);
}

static void
provide(emacs_env *env, const char *feature)
{
    emacs_value sym = env->intern(env, feature);
    emacs_value args[] = { sym };
    env->funcall(env, env->intern(env, "provide"), 1, args);
}

static void
signal_error(emacs_env *env, int code, const char *msg)
{
    emacs_value message = env->make_string(env, msg, strlen(msg));
    emacs_value data = env->funcall(env, env->intern(env, "list"),
                                    1, &message);
    emacs_value errsym = Qjabber_omemo_error;
    env->non_local_exit_signal(env, errsym, data);
    (void)code;
}

static emacs_value
make_unibyte(emacs_env *env, const uint8_t *buf, size_t len)
{
    return env->make_unibyte_string(env, (const char *)buf, len);
}

static int
extract_unibyte(emacs_env *env, emacs_value arg,
                uint8_t *buf, size_t bufsize, size_t *outlen)
{
    ptrdiff_t len = (ptrdiff_t)bufsize;
    if (!env->copy_string_contents(env, arg, (char *)buf, &len))
        return -1;
    /* copy_string_contents appends a NUL; actual length is len-1. */
    if (outlen)
        *outlen = (size_t)(len - 1);
    return 0;
}

static int
extract_exact_unibyte(emacs_env *env, emacs_value arg, uint8_t *buf,
                      size_t expected, const char *message)
{
    ptrdiff_t len = 0;
    env->copy_string_contents(env, arg, NULL, &len);
    if (env->non_local_exit_check(env))
        return -1;
    if (len != (ptrdiff_t)expected + 1) {
        signal_error(env, OMEMO_ESTORE, message);
        return -1;
    }
    return extract_unibyte(env, arg, buf, expected + 1, NULL);
}

/*  Finalizers for user-ptr  */

static void
free_store(void *ptr)
{
    skipped_clear(ptr, sizeof(struct omemoStore));
    free(ptr);
}

static void
free_session(void *ptr)
{
    struct native_session *session = ptr;
    skipped_drop(session);
    skipped_clear(&session->session, sizeof session->session);
    free(ptr);
}

static void *
checked_user_ptr(emacs_env *env, emacs_value value,
                 void (*expected)(void *), const char *message)
{
    void (*finalizer)(void *) = env->get_user_finalizer(env, value);
    if (env->non_local_exit_check(env))
        return NULL;
    if (finalizer != expected) {
        signal_error(env, OMEMO_ESTORE, message);
        return NULL;
    }
    return env->get_user_ptr(env, value);
}

static struct omemoStore *
extract_store(emacs_env *env, emacs_value value)
{
    return checked_user_ptr(env, value, free_store,
                            "expected an OMEMO store pointer");
}

static struct native_session *
extract_session(emacs_env *env, emacs_value value)
{
    return checked_user_ptr(env, value, free_session,
                            "expected an OMEMO session pointer");
}

static uint32_t
read_u32(const uint8_t *p)
{
    return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
           ((uint32_t)p[2] << 8) | p[3];
}

static void
write_u32(uint8_t *p, uint32_t value)
{
    p[0] = value >> 24;
    p[1] = value >> 16;
    p[2] = value >> 8;
    p[3] = value;
}

static bool
session_envelope_p(const uint8_t *blob, size_t len)
{
    return len >= SESSION_MAGIC_SIZE &&
           !memcmp(blob, SESSION_MAGIC, SESSION_MAGIC_SIZE);
}

/*  jabber-omemo--setup-store  */

static emacs_value
F_setup_store(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
              void *data)
{
    (void)nargs; (void)args; (void)data;

    struct omemoStore store;
    int rc = omemoSetupStore(&store);
    if (rc) {
        signal_error(env, rc, "omemoSetupStore failed");
        return Qnil_v;
    }

    size_t sz = omemoGetSerializedStoreSize(&store);
    uint8_t *buf = malloc(sz);
    if (!buf) {
        skipped_clear(&store, sizeof store);
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    omemoSerializeStore(buf, &store);

    emacs_value result = make_unibyte(env, buf, sz);
    skipped_clear(buf, sz);
    skipped_clear(&store, sizeof store);
    free(buf);
    return result;
}

/*  jabber-omemo--deserialize-store  */

static emacs_value
F_deserialize_store(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                    void *data)
{
    (void)nargs; (void)data;

    /* Get size of the blob. */
    ptrdiff_t bloblen = 0;
    env->copy_string_contents(env, args[0], NULL, &bloblen);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    uint8_t *blob = malloc((size_t)bloblen);
    if (!blob) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    env->copy_string_contents(env, args[0], (char *)blob, &bloblen);
    if (env->non_local_exit_check(env)) {
        skipped_clear(blob, (size_t)bloblen);
        free(blob);
        return Qnil_v;
    }
    /* actual data length is bloblen-1 (NUL terminator) */
    size_t datalen = (size_t)(bloblen - 1);

    struct omemoStore *store = calloc(1, sizeof(*store));
    if (!store) {
        skipped_clear(blob, datalen);
        free(blob);
        signal_error(env, -1, "calloc failed");
        return Qnil_v;
    }

    int rc = omemoDeserializeStore(blob, datalen, store);
    skipped_clear(blob, datalen);
    free(blob);
    if (rc) {
        skipped_clear(store, sizeof *store);
        free(store);
        signal_error(env, rc, "omemoDeserializeStore failed");
        return Qnil_v;
    }

    return env->make_user_ptr(env, free_store, store);
}

/*  jabber-omemo--serialize-store  */

static emacs_value
F_serialize_store(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                  void *data)
{
    (void)nargs; (void)data;

    struct omemoStore *store = extract_store(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    size_t sz = omemoGetSerializedStoreSize(store);
    uint8_t *buf = malloc(sz);
    if (!buf) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    omemoSerializeStore(buf, store);

    emacs_value result = make_unibyte(env, buf, sz);
    skipped_clear(buf, sz);
    free(buf);
    return result;
}

/*  jabber-omemo--get-bundle  */

static emacs_value
F_get_bundle(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
             void *data)
{
    (void)nargs; (void)data;

    struct omemoStore *store = extract_store(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    /* Serialize identity key */
    omemoSerializedKey ik;
    omemoSerializeKey(ik, store->identity.pub);

    /* Serialize signed pre-key */
    omemoSerializedKey spk;
    omemoSerializeKey(spk, store->cursignedprekey.kp.pub);

    /* Build list of (id . key) pairs for pre-keys */
    emacs_value Qcons = env->intern(env, "cons");
    emacs_value Qlist = env->intern(env, "list");

    /* Count valid pre-keys first */
    int npk = 0;
    for (int i = 0; i < OMEMO_NUMPREKEYS; i++) {
        /* A zeroed pre-key has id=0 and zeroed key pair; skip it */
        uint8_t zero[32] = {0};
        if (store->prekeys[i].id == 0 &&
            memcmp(store->prekeys[i].kp.pub, zero, 32) == 0)
            continue;
        npk++;
    }

    /* Build pre-keys list backwards for efficiency */
    emacs_value prekey_list = Qnil_v;
    emacs_value Qnreverse = env->intern(env, "nreverse");
    for (int i = 0; i < OMEMO_NUMPREKEYS; i++) {
        uint8_t zero[32] = {0};
        if (store->prekeys[i].id == 0 &&
            memcmp(store->prekeys[i].kp.pub, zero, 32) == 0)
            continue;

        omemoSerializedKey pk;
        omemoSerializeKey(pk, store->prekeys[i].kp.pub);

        emacs_value pair_args[2];
        pair_args[0] = env->make_integer(env, store->prekeys[i].id);
        pair_args[1] = make_unibyte(env, pk, sizeof(pk));
        emacs_value pair = env->funcall(env, Qcons, 2, pair_args);

        emacs_value cons_args[2] = { pair, prekey_list };
        prekey_list = env->funcall(env, Qcons, 2, cons_args);
    }
    emacs_value rev_args[] = { prekey_list };
    prekey_list = env->funcall(env, Qnreverse, 1, rev_args);

    emacs_value plist_args[10];
    plist_args[0] = Qidentity_key;
    plist_args[1] = make_unibyte(env, ik, sizeof(ik));
    plist_args[2] = Qsigned_pre_key;
    plist_args[3] = make_unibyte(env, spk, sizeof(spk));
    plist_args[4] = Qsigned_pre_key_id;
    plist_args[5] = env->make_integer(env, store->cursignedprekey.id);
    plist_args[6] = Qsignature;
    plist_args[7] = make_unibyte(env, store->cursignedprekey.sig,
                                 sizeof(store->cursignedprekey.sig));
    plist_args[8] = Qpre_keys;
    plist_args[9] = prekey_list;

    return env->funcall(env, Qlist, 10, plist_args);
}

/*  jabber-omemo--rotate-signed-pre-key  */

static emacs_value
F_rotate_signed_pre_key(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                        void *data)
{
    (void)nargs; (void)data;

    struct omemoStore *store = extract_store(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    int rc = omemoRotateSignedPreKey(store);
    if (rc) {
        signal_error(env, rc, "omemoRotateSignedPreKey failed");
        return Qnil_v;
    }
    return Qnil_v;
}

/*  jabber-omemo--refill-pre-keys  */

static emacs_value
F_refill_pre_keys(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                  void *data)
{
    (void)nargs; (void)data;

    struct omemoStore *store = extract_store(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    int rc = omemoRefillPreKeys(store);
    if (rc) {
        signal_error(env, rc, "omemoRefillPreKeys failed");
        return Qnil_v;
    }
    return Qnil_v;
}

/*  jabber-omemo--remove-pre-key  */

static emacs_value
F_remove_pre_key(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                 void *data)
{
    (void)nargs; (void)data;

    struct omemoStore *store = extract_store(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    intmax_t id = env->extract_integer(env, args[1]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    if (id <= 0)
        return Qnil_v;

    for (int i = 0; i < OMEMO_NUMPREKEYS; i++) {
        if (store->prekeys[i].id == (uint32_t)id) {
            memset(&store->prekeys[i], 0, sizeof(struct omemoPreKey));
            return Qt_v;
        }
    }
    return Qnil_v;
}

/*  jabber-omemo--used-pre-key-id  */

static emacs_value
F_used_pre_key_id(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                  void *data)
{
    (void)nargs; (void)data;

    struct native_session *native = extract_session(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    return env->make_integer(env, native->session.usedpk_id);
}

/*  jabber-omemo--encrypt-message  */

static emacs_value
F_encrypt_message(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                  void *data)
{
    (void)nargs; (void)data;

    /* Get plaintext size */
    ptrdiff_t ptlen = 0;
    env->copy_string_contents(env, args[0], NULL, &ptlen);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    uint8_t *plaintext = malloc((size_t)ptlen);
    if (!plaintext) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    env->copy_string_contents(env, args[0], (char *)plaintext, &ptlen);
    if (env->non_local_exit_check(env)) {
        free(plaintext);
        return Qnil_v;
    }
    size_t msglen = (size_t)(ptlen - 1);

    uint8_t *ciphertext = malloc(msglen);
    if (!ciphertext) {
        free(plaintext);
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }

    uint8_t key[32];
    uint8_t iv[12];

    int rc = omemoEncryptMessage(ciphertext, key, iv, plaintext, msglen);
    free(plaintext);
    if (rc) {
        free(ciphertext);
        signal_error(env, rc, "omemoEncryptMessage failed");
        return Qnil_v;
    }

    emacs_value Qlist = env->intern(env, "list");
    emacs_value plist_args[6];
    plist_args[0] = Qkey;
    plist_args[1] = make_unibyte(env, key, 32);
    plist_args[2] = Qiv;
    plist_args[3] = make_unibyte(env, iv, 12);
    plist_args[4] = Qciphertext;
    plist_args[5] = make_unibyte(env, ciphertext, msglen);

    free(ciphertext);
    return env->funcall(env, Qlist, 6, plist_args);
}

/*  jabber-omemo--decrypt-message  */

static emacs_value
F_decrypt_message(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                  void *data)
{
    (void)nargs; (void)data;

    /* Extract key (32 bytes) */
    uint8_t key[33];
    if (extract_exact_unibyte(env, args[0], key, 32,
                              "message key must be exactly 32 bytes"))
        return Qnil_v;

    /* Extract IV (12 bytes) */
    uint8_t iv[13];
    if (extract_exact_unibyte(env, args[1], iv, 12,
                              "message IV must be exactly 12 bytes"))
        return Qnil_v;

    /* Extract ciphertext */
    ptrdiff_t ctlen_raw = 0;
    env->copy_string_contents(env, args[2], NULL, &ctlen_raw);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    uint8_t *ciphertext = malloc((size_t)ctlen_raw);
    if (!ciphertext) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    env->copy_string_contents(env, args[2], (char *)ciphertext, &ctlen_raw);
    if (env->non_local_exit_check(env)) {
        free(ciphertext);
        return Qnil_v;
    }
    size_t ctlen = (size_t)(ctlen_raw - 1);

    uint8_t *plaintext = malloc(ctlen);
    if (!plaintext) {
        free(ciphertext);
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }

    int rc = omemoDecryptMessage(plaintext, key, 32, iv, ciphertext,
                                 ctlen);
    free(ciphertext);
    if (rc) {
        free(plaintext);
        signal_error(env, rc, "omemoDecryptMessage failed");
        return Qnil_v;
    }

    emacs_value result = make_unibyte(env, plaintext, ctlen);
    free(plaintext);
    return result;
}

/*  jabber-omemo--make-session  */

static emacs_value
F_make_session(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
               void *data)
{
    (void)nargs; (void)args; (void)data;

    struct native_session *session = calloc(1, sizeof(*session));
    if (!session) {
        signal_error(env, -1, "calloc failed");
        return Qnil_v;
    }

    return env->make_user_ptr(env, free_session, session);
}

/*  jabber-omemo--initiate-session  */

static emacs_value
F_initiate_session(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                   void *data)
{
    (void)nargs; (void)data;

    struct omemoStore *store = extract_store(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    /* Extract signature (64 bytes) */
    uint8_t sig[65];
    if (extract_exact_unibyte(env, args[1], sig, 64,
                              "signature must be exactly 64 bytes"))
        return Qnil_v;

    /* Extract signed pre-key (33 bytes) */
    uint8_t spk[34];
    if (extract_exact_unibyte(env, args[2], spk, 33,
                              "signed pre-key must be exactly 33 bytes"))
        return Qnil_v;

    /* Extract identity key (33 bytes) */
    uint8_t ik[34];
    if (extract_exact_unibyte(env, args[3], ik, 33,
                              "identity key must be exactly 33 bytes"))
        return Qnil_v;

    /* Extract pre-key (33 bytes) */
    uint8_t pk[34];
    if (extract_exact_unibyte(env, args[4], pk, 33,
                              "pre-key must be exactly 33 bytes"))
        return Qnil_v;

    uint32_t spk_id = (uint32_t)env->extract_integer(env, args[5]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    uint32_t pk_id = (uint32_t)env->extract_integer(env, args[6]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    struct native_session *session = calloc(1, sizeof(*session));
    if (!session) {
        signal_error(env, -1, "calloc failed");
        return Qnil_v;
    }

    int rc = omemoInitiateSession(&session->session, store, sig, spk, ik, pk,
                                  spk_id, pk_id);
    if (rc) {
        free(session);
        signal_error(env, rc, "omemoInitiateSession failed");
        return Qnil_v;
    }

    return env->make_user_ptr(env, free_session, session);
}

/*  jabber-omemo--serialize-session  */

static emacs_value
F_serialize_session(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                    void *data)
{
    (void)nargs; (void)data;

    struct native_session *session = extract_session(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    size_t raw_size = omemoGetSerializedSessionSize(&session->session);
    if (session->count > SKIPPED_KEYS_MAX ||
        raw_size > UINT32_MAX ||
        session->count > (SIZE_MAX - SESSION_HEADER_SIZE - raw_size) /
                         SKIPPED_KEY_SIZE) {
        signal_error(env, OMEMO_ESTORE, "session is too large to serialize");
        return Qnil_v;
    }
    size_t sz = SESSION_HEADER_SIZE + raw_size +
                session->count * SKIPPED_KEY_SIZE;
    uint8_t *buf = malloc(sz);
    if (!buf) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    memcpy(buf, SESSION_MAGIC, SESSION_MAGIC_SIZE);
    write_u32(buf + 8, SESSION_VERSION);
    write_u32(buf + 12, (uint32_t)raw_size);
    write_u32(buf + 16, (uint32_t)session->count);
    omemoSerializeSession(buf + SESSION_HEADER_SIZE, &session->session);
    uint8_t *p = buf + SESSION_HEADER_SIZE + raw_size;
    for (size_t i = 0; i < session->count; i++, p += SKIPPED_KEY_SIZE) {
        write_u32(p, session->keys[i].nr);
        memcpy(p + 4, session->keys[i].dh, 32);
        memcpy(p + 36, session->keys[i].mk, 32);
    }

    emacs_value result = make_unibyte(env, buf, sz);
    skipped_clear(buf, sz);
    free(buf);
    return result;
}

/*  jabber-omemo--deserialize-session  */

static emacs_value
F_deserialize_session(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                      void *data)
{
    (void)nargs; (void)data;

    ptrdiff_t bloblen = 0;
    env->copy_string_contents(env, args[0], NULL, &bloblen);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    uint8_t *blob = malloc((size_t)bloblen);
    if (!blob) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    env->copy_string_contents(env, args[0], (char *)blob, &bloblen);
    if (env->non_local_exit_check(env)) {
        free(blob);
        return Qnil_v;
    }
    size_t datalen = (size_t)(bloblen - 1);

    struct native_session *session = calloc(1, sizeof(*session));
    if (!session) {
        free(blob);
        signal_error(env, -1, "calloc failed");
        return Qnil_v;
    }

    const uint8_t *raw = blob;
    size_t raw_size = datalen;
    uint32_t key_count = 0;
    if (session_envelope_p(blob, datalen)) {
        if (datalen < SESSION_HEADER_SIZE || read_u32(blob + 8) != SESSION_VERSION) {
            skipped_clear(blob, datalen);
            free(blob);
            free(session);
            signal_error(env, OMEMO_ECORRUPT, "invalid session envelope");
            return Qnil_v;
        }
        raw_size = read_u32(blob + 12);
        key_count = read_u32(blob + 16);
        if (key_count > SKIPPED_KEYS_MAX ||
            raw_size > datalen - SESSION_HEADER_SIZE ||
            key_count > (SIZE_MAX - SESSION_HEADER_SIZE - raw_size) /
                        SKIPPED_KEY_SIZE ||
            SESSION_HEADER_SIZE + raw_size +
                (size_t)key_count * SKIPPED_KEY_SIZE != datalen) {
            skipped_clear(blob, datalen);
            free(blob);
            free(session);
            signal_error(env, OMEMO_ECORRUPT, "malformed session envelope");
            return Qnil_v;
        }
        raw = blob + SESSION_HEADER_SIZE;
    }
    int rc = omemoDeserializeSession(raw, raw_size, &session->session);
    if (!rc && key_count) {
        const uint8_t *p = raw + raw_size;
        for (uint32_t i = 0; i < key_count; i++, p += SKIPPED_KEY_SIZE)
            if (skipped_add(session, read_u32(p), p + 4, p + 36)) {
                rc = OMEMO_ESTORE;
                break;
            }
    }
    skipped_clear(blob, datalen);
    free(blob);
    if (rc) {
        skipped_drop(session);
        free(session);
        signal_error(env, rc, "omemoDeserializeSession failed");
        return Qnil_v;
    }

    return env->make_user_ptr(env, free_session, session);
}

/*  jabber-omemo--legacy-session-blob-p  */

static emacs_value
F_legacy_session_blob_p(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                        void *data)
{
    (void)nargs; (void)data;

    ptrdiff_t bloblen = 0;
    env->copy_string_contents(env, args[0], NULL, &bloblen);
    if (env->non_local_exit_check(env))
        return Qnil_v;
    uint8_t *blob = malloc((size_t)bloblen);
    if (!blob) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    if (!env->copy_string_contents(env, args[0], (char *)blob, &bloblen)) {
        free(blob);
        return Qnil_v;
    }
    emacs_value result = session_envelope_p(blob, (size_t)bloblen - 1)
                         ? Qnil_v : Qt_v;
    skipped_clear(blob, (size_t)bloblen - 1);
    free(blob);
    return result;
}

/*  jabber-omemo--encrypt-key  */

static emacs_value
F_encrypt_key(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
              void *data)
{
    (void)nargs; (void)data;

    struct native_session *native = extract_session(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    /* Extract plaintext key */
    uint8_t keybuf[OMEMO_KEYSIZE + 1];
    size_t keylen;
    if (extract_unibyte(env, args[1], keybuf, sizeof(keybuf), &keylen))
        return Qnil_v;

    struct omemoKeyMessage msg;
    memset(&msg, 0, sizeof(msg));

    int rc = omemoEncryptKey(&native->session, &msg, keybuf, keylen);
    if (rc) {
        signal_error(env, rc, "omemoEncryptKey failed");
        return Qnil_v;
    }

    emacs_value Qlist = env->intern(env, "list");
    emacs_value plist_args[4];
    plist_args[0] = Qdata;
    plist_args[1] = make_unibyte(env, msg.p, msg.n);
    plist_args[2] = Qpre_key_p;
    plist_args[3] = msg.isprekey ? Qt_v : Qnil_v;

    return env->funcall(env, Qlist, 4, plist_args);
}

/*  jabber-omemo--decrypt-key  */

static emacs_value
F_decrypt_key(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
              void *data)
{
    (void)nargs; (void)data;

    struct native_session *native = extract_session(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    struct omemoStore *store = extract_store(env, args[1]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    bool isprekey = env->is_not_nil(env, args[2]);

    /* Extract encrypted message */
    ptrdiff_t msglen_raw = 0;
    env->copy_string_contents(env, args[3], NULL, &msglen_raw);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    uint8_t *msgbuf = malloc((size_t)msglen_raw);
    if (!msgbuf) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    env->copy_string_contents(env, args[3], (char *)msgbuf, &msglen_raw);
    if (env->non_local_exit_check(env)) {
        free(msgbuf);
        return Qnil_v;
    }
    size_t msglen = (size_t)(msglen_raw - 1);

    uint8_t key[OMEMO_KEYSIZE];
    size_t keyn = sizeof(key);

    size_t old_count = native->count;
    struct skipped_key *old_keys = NULL;
    if (old_count) {
        old_keys = malloc(old_count * sizeof *old_keys);
        if (!old_keys) {
            free(msgbuf);
            signal_error(env, OMEMO_ESTORE, "cannot snapshot skipped keys");
            return Qnil_v;
        }
        memcpy(old_keys, native->keys, old_count * sizeof *old_keys);
    }
    native->decrypt_new_count = 0;
    int rc = omemoDecryptKey(&native->session, store, key, &keyn,
                             isprekey, msgbuf, msglen);
    native->decrypt_new_count = 0;
    free(msgbuf);
    if (rc) {
        skipped_drop(native);
        native->keys = old_keys;
        native->count = native->cap = old_count;
        signal_error(env, rc, "omemoDecryptKey failed");
        return Qnil_v;
    }
    if (old_keys) {
        skipped_clear(old_keys, old_count * sizeof *old_keys);
        free(old_keys);
    }

    return make_unibyte(env, key, keyn);
}

/*  jabber-omemo--session-skipped-keys  */

static emacs_value
F_session_skipped_keys(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                       void *data)
{
    (void)nargs; (void)data;

    struct native_session *session = extract_session(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    if (!session->count)
        return Qnil_v;
    size_t count = session->count;
    struct skipped_key *snapshot = malloc(count * sizeof *snapshot);
    if (!snapshot && count) {
        signal_error(env, OMEMO_ESTORE, "cannot snapshot skipped keys");
        return Qnil_v;
    }
    memcpy(snapshot, session->keys, count * sizeof *snapshot);

    emacs_value Qlist = env->intern(env, "list");
    emacs_value Qcons = env->intern(env, "cons");
    emacs_value result = Qnil_v;
    for (size_t i = count; i > 0; i--) {
        struct skipped_key *sk = &snapshot[i - 1];
        emacs_value entry_args[] = {
            env->make_integer(env, sk->nr),
            make_unibyte(env, sk->dh, 32),
            make_unibyte(env, sk->mk, 32),
        };
        emacs_value entry = env->funcall(env, Qlist, 3, entry_args);
        emacs_value cons_args[] = { entry, result };
        result = env->funcall(env, Qcons, 2, cons_args);
    }
    skipped_clear(snapshot, count * sizeof *snapshot);
    free(snapshot);
    return result;
}

/*  jabber-omemo--session-set-skipped-keys  */

struct skipped_key_symbols {
    emacs_value car, cdr, consp, integerp, stringp;
};

static int
intern_skipped_key_symbols(emacs_env *env, struct skipped_key_symbols *symbols)
{
    symbols->car = env->intern(env, "car");
    if (env->non_local_exit_check(env))
        return -1;
    symbols->cdr = env->intern(env, "cdr");
    if (env->non_local_exit_check(env))
        return -1;
    symbols->consp = env->intern(env, "consp");
    if (env->non_local_exit_check(env))
        return -1;
    symbols->integerp = env->intern(env, "integerp");
    if (env->non_local_exit_check(env))
        return -1;
    symbols->stringp = env->intern(env, "stringp");
    return env->non_local_exit_check(env) ? -1 : 0;
}

static int
emacs_truth(emacs_env *env, emacs_value value, bool *result)
{
    *result = env->is_not_nil(env, value);
    return env->non_local_exit_check(env) ? -1 : 0;
}

static int
take_cons(emacs_env *env, const struct skipped_key_symbols *symbols,
          emacs_value *list, emacs_value *value, const char *message)
{
    emacs_value proper = env->funcall(env, symbols->consp, 1, list);
    if (env->non_local_exit_check(env))
        return -1;
    bool is_cons;
    if (emacs_truth(env, proper, &is_cons))
        return -1;
    if (!is_cons) {
        signal_error(env, OMEMO_EPARAM, message);
        return -1;
    }
    *value = env->funcall(env, symbols->car, 1, list);
    if (env->non_local_exit_check(env))
        return -1;
    *list = env->funcall(env, symbols->cdr, 1, list);
    return env->non_local_exit_check(env) ? -1 : 0;
}

static int
require_type(emacs_env *env, emacs_value predicate, emacs_value value,
             const char *message)
{
    emacs_value valid = env->funcall(env, predicate, 1, &value);
    if (env->non_local_exit_check(env))
        return -1;
    bool matches;
    if (emacs_truth(env, valid, &matches))
        return -1;
    if (!matches) {
        signal_error(env, OMEMO_EPARAM, message);
        return -1;
    }
    return 0;
}

static int
parse_skipped_key(emacs_env *env,
                  const struct skipped_key_symbols *symbols,
                  emacs_value entry, struct skipped_key *key)
{
    emacs_value nr, dh, mk;
    if (take_cons(env, symbols, &entry, &nr,
                  "malformed skipped key entry") ||
        take_cons(env, symbols, &entry, &dh,
                  "malformed skipped key entry") ||
        take_cons(env, symbols, &entry, &mk,
                  "malformed skipped key entry"))
        return -1;
    bool has_tail;
    if (emacs_truth(env, entry, &has_tail))
        return -1;
    if (has_tail) {
        signal_error(env, OMEMO_EPARAM, "malformed skipped key entry");
        return -1;
    }
    if (require_type(env, symbols->integerp, nr,
                     "skipped key message number must be an integer") ||
        require_type(env, symbols->stringp, dh,
                     "skipped dh key must be a string") ||
        require_type(env, symbols->stringp, mk,
                     "skipped message key must be a string"))
        return -1;

    intmax_t number = env->extract_integer(env, nr);
    if (env->non_local_exit_check(env))
        return -1;
    if (number < 0 || number > UINT32_MAX) {
        signal_error(env, OMEMO_EPARAM,
                     "skipped key message number is out of range");
        return -1;
    }
    uint8_t dh_buf[33], mk_buf[33];
    if (extract_exact_unibyte(env, dh, dh_buf, 32,
                              "skipped dh key must be exactly 32 bytes") ||
        extract_exact_unibyte(env, mk, mk_buf, 32,
                              "skipped message key must be exactly 32 bytes"))
        return -1;
    key->nr = (uint32_t)number;
    memcpy(key->dh, dh_buf, sizeof key->dh);
    memcpy(key->mk, mk_buf, sizeof key->mk);
    return 0;
}

static emacs_value
F_session_set_skipped_keys(emacs_env *env, ptrdiff_t nargs,
                           emacs_value *args, void *data)
{
    (void)nargs; (void)data;

    struct native_session *session = extract_session(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    struct skipped_key_symbols symbols;
    if (intern_skipped_key_symbols(env, &symbols))
        return Qnil_v;
    struct native_session temporary = {0};
    emacs_value l = args[1];

    for (;;) {
        bool more;
        if (emacs_truth(env, l, &more))
            goto fail;
        if (!more)
            break;
        emacs_value entry;
        struct skipped_key key;
        if (take_cons(env, &symbols, &l, &entry,
                      "skipped keys must be a proper list") ||
            parse_skipped_key(env, &symbols, entry, &key))
            goto fail;
        if (skipped_add(&temporary, key.nr, key.dh, key.mk)) {
            signal_error(env, OMEMO_ESTORE, "cannot store skipped key");
            goto fail;
        }
    }

    skipped_drop(session);
    session->keys = temporary.keys;
    session->count = temporary.count;
    session->cap = temporary.cap;
    return Qnil_v;

fail:
    skipped_drop(&temporary);
    return Qnil_v;
}

/*  jabber-omemo--heartbeat  */

static emacs_value
F_heartbeat(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
            void *data)
{
    (void)nargs; (void)data;

    struct native_session *native = extract_session(env, args[0]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    struct omemoStore *store = extract_store(env, args[1]);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    struct omemoKeyMessage msg;
    memset(&msg, 0, sizeof(msg));

    int rc = omemoHeartbeat(&native->session, store, &msg);
    if (rc) {
        signal_error(env, rc, "omemoHeartbeat failed");
        return Qnil_v;
    }

    if (msg.n == 0)
        return Qnil_v;

    return make_unibyte(env, msg.p, msg.n);
}

/*  jabber-omemo--aesgcm-decrypt  */

static emacs_value
F_aesgcm_decrypt(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                 void *data)
{
    (void)nargs; (void)data;

    /* Extract 32-byte key */
    uint8_t key[33];
    size_t keylen;
    if (extract_unibyte(env, args[0], key, sizeof(key), &keylen))
        return Qnil_v;
    if (keylen != 32) {
        signal_error(env, -1, "aesgcm key must be exactly 32 bytes");
        return Qnil_v;
    }

    /* Extract 12-byte IV */
    uint8_t iv[13];
    size_t ivlen;
    if (extract_unibyte(env, args[1], iv, sizeof(iv), &ivlen))
        return Qnil_v;
    if (ivlen != 12) {
        signal_error(env, -1, "aesgcm IV must be exactly 12 bytes");
        return Qnil_v;
    }

    /* Extract ciphertext + 16-byte GCM auth tag */
    ptrdiff_t ct_raw = 0;
    env->copy_string_contents(env, args[2], NULL, &ct_raw);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    uint8_t *ctbuf = malloc((size_t)ct_raw);
    if (!ctbuf) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    env->copy_string_contents(env, args[2], (char *)ctbuf, &ct_raw);
    if (env->non_local_exit_check(env)) {
        free(ctbuf);
        return Qnil_v;
    }
    size_t total = (size_t)(ct_raw - 1);
    if (total < 16) {
        free(ctbuf);
        signal_error(env, -1, "aesgcm ciphertext too short (need >= 16 bytes for tag)");
        return Qnil_v;
    }

    size_t ct_len = total - 16;
    const uint8_t *tag = ctbuf + ct_len;

    uint8_t *plaintext = malloc(ct_len);
    if (!plaintext) {
        free(ctbuf);
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }

    mbedtls_gcm_context ctx;
    mbedtls_gcm_init(&ctx);
    int rc = mbedtls_gcm_setkey(&ctx, MBEDTLS_CIPHER_ID_AES, key, 256);
    if (!rc)
        rc = mbedtls_gcm_auth_decrypt(&ctx, ct_len, iv, 12, NULL, 0,
                                       tag, 16, ctbuf, plaintext);
    mbedtls_gcm_free(&ctx);

    if (rc) {
        free(ctbuf);
        free(plaintext);
        signal_error(env, rc, "AES-256-GCM decryption failed");
        return Qnil_v;
    }

    emacs_value result = make_unibyte(env, plaintext, ct_len);
    free(ctbuf);
    free(plaintext);
    return result;
}

/*  jabber-omemo--aesgcm-encrypt  */

static emacs_value
F_aesgcm_encrypt(emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                 void *data)
{
    (void)nargs; (void)data;

    /* Extract plaintext */
    ptrdiff_t pt_raw = 0;
    env->copy_string_contents(env, args[0], NULL, &pt_raw);
    if (env->non_local_exit_check(env))
        return Qnil_v;

    uint8_t *ptbuf = malloc((size_t)pt_raw);
    if (!ptbuf) {
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }
    env->copy_string_contents(env, args[0], (char *)ptbuf, &pt_raw);
    if (env->non_local_exit_check(env)) {
        free(ptbuf);
        return Qnil_v;
    }
    size_t pt_len = (size_t)(pt_raw - 1);

    /* Generate random 32-byte key and 12-byte IV */
    uint8_t key[32];
    uint8_t iv[12];
    if (getrandom(key, sizeof(key), 0) != sizeof(key)) {
        free(ptbuf);
        signal_error(env, -1, "getrandom failed for key");
        return Qnil_v;
    }
    if (getrandom(iv, sizeof(iv), 0) != sizeof(iv)) {
        free(ptbuf);
        signal_error(env, -1, "getrandom failed for IV");
        return Qnil_v;
    }

    /* Allocate output: ciphertext + 16-byte GCM auth tag */
    uint8_t *outbuf = malloc(pt_len + 16);
    if (!outbuf) {
        free(ptbuf);
        signal_error(env, -1, "malloc failed");
        return Qnil_v;
    }

    mbedtls_gcm_context ctx;
    mbedtls_gcm_init(&ctx);
    int rc = mbedtls_gcm_setkey(&ctx, MBEDTLS_CIPHER_ID_AES, key, 256);
    if (!rc)
        rc = mbedtls_gcm_crypt_and_tag(&ctx, MBEDTLS_GCM_ENCRYPT,
                                        pt_len, iv, 12, NULL, 0,
                                        ptbuf, outbuf, 16, outbuf + pt_len);
    mbedtls_gcm_free(&ctx);
    free(ptbuf);

    if (rc) {
        free(outbuf);
        signal_error(env, rc, "AES-256-GCM encryption failed");
        return Qnil_v;
    }

    emacs_value Qlist = env->intern(env, "list");
    emacs_value plist_args[6];
    plist_args[0] = Qkey;
    plist_args[1] = make_unibyte(env, key, 32);
    plist_args[2] = Qiv;
    plist_args[3] = make_unibyte(env, iv, 12);
    plist_args[4] = Qciphertext;
    plist_args[5] = make_unibyte(env, outbuf, pt_len + 16);

    free(outbuf);
    return env->funcall(env, Qlist, 6, plist_args);
}

/*  Module init  */

int
emacs_module_init(struct emacs_runtime *runtime)
{
    if (runtime->size < sizeof(*runtime))
        return 1;

    emacs_env *env = runtime->get_environment(runtime);
    if (env->size < sizeof(*env))
        return 2;

    /* Cache symbols as global references so they survive GC */
#define GLOBAL_SYM(var, name) \
    var = env->make_global_ref(env, env->intern(env, name))

    GLOBAL_SYM(Qnil_v, "nil");
    GLOBAL_SYM(Qt_v, "t");

    GLOBAL_SYM(Qidentity_key,      ":identity-key");
    GLOBAL_SYM(Qsigned_pre_key,    ":signed-pre-key");
    GLOBAL_SYM(Qsigned_pre_key_id, ":signed-pre-key-id");
    GLOBAL_SYM(Qsignature,         ":signature");
    GLOBAL_SYM(Qpre_keys,          ":pre-keys");
    GLOBAL_SYM(Qkey,               ":key");
    GLOBAL_SYM(Qiv,                ":iv");
    GLOBAL_SYM(Qciphertext,        ":ciphertext");
    GLOBAL_SYM(Qdata,              ":data");
    GLOBAL_SYM(Qpre_key_p,         ":pre-key-p");

#undef GLOBAL_SYM

    /* Define error symbol */
    Qjabber_omemo_error = env->make_global_ref(
        env, env->intern(env, "jabber-omemo-error"));
    {
        emacs_value args[2];
        args[0] = Qjabber_omemo_error;
        args[1] = env->make_string(env, "OMEMO error", 11);
        env->funcall(env, env->intern(env, "define-error"), 2, args);
    }

    /* Bind functions */

#define DEFUN(lname, cfunc, minargs, maxargs, doc)                      \
    bind_function(env, lname,                                           \
                  env->make_function(env, minargs, maxargs, cfunc,      \
                                     doc, NULL))

    DEFUN("jabber-omemo--setup-store", F_setup_store, 0, 0,
          "Generate a new OMEMO device store.\n"
          "Returns a serialized store as a unibyte string.");

    DEFUN("jabber-omemo--deserialize-store", F_deserialize_store, 1, 1,
          "Deserialize BLOB into an OMEMO store object.\n"
          "Returns a user-ptr; freed automatically by GC.");

    DEFUN("jabber-omemo--serialize-store", F_serialize_store, 1, 1,
          "Serialize STORE-PTR back to a unibyte string.");

    DEFUN("jabber-omemo--get-bundle", F_get_bundle, 1, 1,
          "Extract the public bundle from STORE-PTR.\n"
          "Returns a plist with keys :identity-key, :signed-pre-key,\n"
          ":signed-pre-key-id, :signature, :pre-keys.");

    DEFUN("jabber-omemo--rotate-signed-pre-key",
          F_rotate_signed_pre_key, 1, 1,
          "Rotate the signed pre-key in STORE-PTR.\n"
          "Mutates the store; caller must re-serialize.");

    DEFUN("jabber-omemo--refill-pre-keys", F_refill_pre_keys, 1, 1,
          "Refill removed pre-keys in STORE-PTR.\n"
          "Mutates the store; caller must re-serialize.");

    DEFUN("jabber-omemo--remove-pre-key", F_remove_pre_key, 2, 2,
          "Remove one-time pre-key ID from STORE-PTR (XEP-0384).\n"
          "Zeroes the matching slot; jabber-omemo--refill-pre-keys\n"
          "regenerates zeroed slots.  Mutates the store; caller must\n"
          "re-serialize.  Returns non-nil when a slot was removed.");

    DEFUN("jabber-omemo--used-pre-key-id", F_used_pre_key_id, 1, 1,
          "Return the one-time pre-key id consumed by SESSION-PTR.\n"
          "Non-zero only after a fresh session decrypted a pre-key\n"
          "message; the value persists in the serialized session.");

    DEFUN("jabber-omemo--encrypt-message", F_encrypt_message, 1, 1,
          "Encrypt PLAINTEXT (a unibyte string) with OMEMO 0.3.\n"
          "Returns a plist (:key KEY :iv IV :ciphertext CT),\n"
          "all unibyte strings.");

    DEFUN("jabber-omemo--decrypt-message", F_decrypt_message, 3, 3,
          "Decrypt an OMEMO 0.3 message.\n"
          "KEY is a unibyte string (>= 32 bytes: 16 AES + auth tag).\n"
          "IV is a 12-byte unibyte string.\n"
          "CIPHERTEXT is the encrypted payload.\n"
          "Returns the plaintext as a unibyte string.");

    DEFUN("jabber-omemo--make-session", F_make_session, 0, 0,
          "Allocate an empty OMEMO session.\n"
          "Returns a session user-ptr; freed automatically by GC.\n"
          "Use for the receiving side of a pre-key message.");

    DEFUN("jabber-omemo--initiate-session", F_initiate_session, 7, 7,
          "Initiate an OMEMO session with a remote device's bundle.\n"
          "STORE-PTR is the local OMEMO store.\n"
          "SIGNATURE is a 64-byte unibyte string.\n"
          "SIGNED-PRE-KEY, IDENTITY-KEY, PRE-KEY are 33-byte unibyte strings.\n"
          "SPK-ID and PK-ID are integer key IDs.\n"
          "Returns a session user-ptr; freed automatically by GC.");

    DEFUN("jabber-omemo--serialize-session", F_serialize_session, 1, 1,
          "Serialize SESSION-PTR to a unibyte string.");

    DEFUN("jabber-omemo--deserialize-session", F_deserialize_session, 1, 1,
          "Deserialize BLOB into an OMEMO session object.\n"
          "Returns a session user-ptr; freed automatically by GC.");

    DEFUN("jabber-omemo--legacy-session-blob-p",
          F_legacy_session_blob_p, 1, 1,
          "Return non-nil when BLOB uses the legacy raw session format.");

    DEFUN("jabber-omemo--encrypt-key", F_encrypt_key, 2, 2,
          "Encrypt KEY for a recipient using SESSION-PTR.\n"
          "KEY is a unibyte string (the message encryption key).\n"
          "Returns a plist (:data BYTES :pre-key-p BOOL).");

    DEFUN("jabber-omemo--decrypt-key", F_decrypt_key, 4, 4,
          "Decrypt an encrypted key message.\n"
          "SESSION-PTR is the session with the sender.\n"
          "STORE-PTR is the local OMEMO store.\n"
          "PRE-KEY-P is non-nil if this is a pre-key message.\n"
          "MSG is the encrypted key message as a unibyte string.\n"
          "Returns the decrypted key as a unibyte string.");

    DEFUN("jabber-omemo--session-skipped-keys", F_session_skipped_keys, 1, 1,
          "Return SESSION-PTR's in-memory skipped message keys.\n"
          "Each element is (NR DH MK) with NR an integer and DH/MK\n"
          "32-byte unibyte strings.");

    DEFUN("jabber-omemo--session-set-skipped-keys",
          F_session_set_skipped_keys, 2, 2,
          "Replace SESSION-PTR's in-memory skipped message keys with KEYS.\n"
          "KEYS is a list of (NR DH MK) entries as returned by\n"
          "jabber-omemo--session-skipped-keys.");

    DEFUN("jabber-omemo--heartbeat", F_heartbeat, 2, 2,
          "Check if a heartbeat message is needed after decryption.\n"
          "SESSION-PTR is the session to check.\n"
          "STORE-PTR is the local OMEMO store.\n"
          "Returns heartbeat message bytes or nil.");

    DEFUN("jabber-omemo--aesgcm-decrypt", F_aesgcm_decrypt, 3, 3,
          "Decrypt ciphertext using AES-256-GCM (for aesgcm:// URLs).\n"
          "KEY is a 32-byte unibyte string.\n"
          "IV is a 12-byte unibyte string.\n"
          "CIPHERTEXT-WITH-TAG has the 16-byte GCM auth tag appended.\n"
          "Returns the decrypted plaintext as a unibyte string.");

    DEFUN("jabber-omemo--aesgcm-encrypt", F_aesgcm_encrypt, 1, 1,
          "Encrypt PLAINTEXT using AES-256-GCM (for aesgcm:// URLs).\n"
          "Generates a random 32-byte key and 12-byte IV internally.\n"
          "Returns a plist (:key KEY :iv IV :ciphertext CT-WITH-TAG),\n"
          "all unibyte strings.  The last 16 bytes of CT-WITH-TAG are\n"
          "the GCM auth tag.");

#undef DEFUN

    provide(env, "jabber-omemo-core");
    return 0;
}
