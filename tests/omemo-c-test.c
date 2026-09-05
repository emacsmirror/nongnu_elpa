/* Local picomemo regressions.  Synthetic stores only; never print material.
 * The CBC fault is linked only into this standalone test executable. */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mbedtls/aes.h>
#include "picomemo/omemo.h"

static int fail_cbc;
int __real_mbedtls_aes_crypt_cbc(mbedtls_aes_context *, int, size_t,
                               unsigned char *, const unsigned char *,
                               unsigned char *);
int __wrap_mbedtls_aes_crypt_cbc(mbedtls_aes_context *ctx, int mode, size_t n,
                               unsigned char *iv, const unsigned char *src,
                               unsigned char *dst) {
  if (fail_cbc) {
    /* A failed backend may already have written partial output. */
    memset(dst, 0xa5, n);
    return MBEDTLS_ERR_AES_INVALID_INPUT_LENGTH;
  }
  return __real_mbedtls_aes_crypt_cbc(ctx, mode, n, iv, src, dst);
}

static void encrypt_failure(void) {
  struct omemoStore alice, bob;
  struct omemoSession sender = {0}, receiver = {0}, before;
  omemoSerializedKey spk, ik, pk;
  struct omemoKeyMessage msg, empty = {0};
  uint8_t key[32] = {0}, out[32];
  assert(!omemoSetupStore(&alice));
  assert(!omemoSetupStore(&bob));
  omemoSerializeKey(spk, bob.cursignedprekey.kp.pub);
  omemoSerializeKey(ik, bob.identity.pub);
  omemoSerializeKey(pk, bob.prekeys[0].kp.pub);
  assert(!omemoInitiateSession(&sender, &alice, bob.cursignedprekey.sig,
                              spk, ik, pk, bob.cursignedprekey.id,
                              bob.prekeys[0].id));
  /* Check both the first message and a subsequent chain advance. */
  for (int i = 0; i < 2; i++) {
    memcpy(&before, &sender, sizeof before);
    memset(&msg, 0xa5, sizeof msg);
    fail_cbc = 1;
    int rc = omemoEncryptKey(&sender, &msg, key, sizeof key);
    fail_cbc = 0;
    assert(rc == OMEMO_ECRYPTO);
    assert(!memcmp(&before, &sender, sizeof sender));
    assert(!memcmp(&empty, &msg, sizeof msg));
    assert(!omemoEncryptKey(&sender, &msg, key, sizeof key));
    size_t n = sizeof out;
    assert(!omemoDecryptKey(&receiver, &bob, out, &n,
                           msg.isprekey, msg.p, msg.n));
    assert(n == sizeof key && !memcmp(out, key, n));
  }
}

static void store_roundtrip(void) {
  struct omemoStore store, restored = {0};
  assert(!omemoSetupStore(&store));
  for (int i = 0; i < 28; i++) {
    memset(&store.prekeys[0], 0, sizeof store.prekeys[0]);
    assert(!omemoRefillPreKeys(&store));
  }
  assert(store.pkcounter == 128);
  size_t n = omemoGetSerializedStoreSize(&store);
  uint8_t *blob = malloc(n), *copy = malloc(n);
  assert(blob && copy);
  omemoSerializeStore(blob, &store);
  assert(!omemoDeserializeStore(blob, n, &restored));
  assert(omemoGetSerializedStoreSize(&restored) == n);
  omemoSerializeStore(copy, &restored);
  assert(!memcmp(blob, copy, n));
  /* A repeated entry cannot extend past the enclosing input. */
  assert(omemoDeserializeStore(blob, n - 1, &restored) == OMEMO_EPROTOBUF);
  /* The identity private key still has a fixed schema length of 32. */
  assert(blob[2] == 0x12 && blob[3] == 32);
  blob[3] = 31;
  assert(omemoDeserializeStore(blob, n, &restored) == OMEMO_EPROTOBUF);
  free(copy);
  free(blob);
}

int main(int argc, char **argv) {
  assert(argc == 2);
  if (!strcmp(argv[1], "encrypt"))
    encrypt_failure();
  else {
    assert(!strcmp(argv[1], "store"));
    store_roundtrip();
  }
  puts("picomemo regression passed");
  return 0;
}
