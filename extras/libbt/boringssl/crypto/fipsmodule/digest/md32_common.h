// Copyright 1999-2016 The OpenSSL Project Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef OPENSSL_HEADER_DIGEST_MD32_COMMON_H
#define OPENSSL_HEADER_DIGEST_MD32_COMMON_H

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <memory.h>

#define OPENSSL_memcpy(dest, src, num) memcpy(dest, src, num)
#define OPENSSL_memset(ptr, value, num) memset(ptr, value, num)

#if defined(__cplusplus)
extern "C" {
#endif

// 32/64 help macro
#if __LP64__ || __LLP64__
#	define OPENSSL_64_BIT
#else // __ILP32__
#	define OPENSSL_32_BIT
#endif


// crypto_word_t is the type that most constant-time functions use. Ideally we
// would like it to be |size_t|, but NaCl builds in 64-bit mode with 32-bit
// pointers, which means that |size_t| can be 32 bits when |BN_ULONG| is 64
// bits. Since we want to be able to do constant-time operations on a
// |BN_ULONG|, |crypto_word_t| is defined as an unsigned value with the native
// word length.
#if defined(OPENSSL_64_BIT)
typedef uint64_t crypto_word_t;
#elif defined(OPENSSL_32_BIT)
typedef uint32_t crypto_word_t;
#else
#error "Must define either OPENSSL_32_BIT or OPENSSL_64_BIT"
#endif

// Endianness conversions.

#if defined(__GNUC__) && __GNUC__ >= 2
static inline uint16_t CRYPTO_bswap2(uint16_t x) {
  return __builtin_bswap16(x);
}

static inline uint32_t CRYPTO_bswap4(uint32_t x) {
  return __builtin_bswap32(x);
}

static inline uint64_t CRYPTO_bswap8(uint64_t x) {
  return __builtin_bswap64(x);
}
#elif defined(_MSC_VER)
OPENSSL_MSVC_PRAGMA(warning(push, 3))
#include <stdlib.h>
OPENSSL_MSVC_PRAGMA(warning(pop))
#pragma intrinsic(_byteswap_uint64, _byteswap_ulong, _byteswap_ushort)
static inline uint16_t CRYPTO_bswap2(uint16_t x) { return _byteswap_ushort(x); }

static inline uint32_t CRYPTO_bswap4(uint32_t x) { return _byteswap_ulong(x); }

static inline uint64_t CRYPTO_bswap8(uint64_t x) { return _byteswap_uint64(x); }
#else
static inline uint16_t CRYPTO_bswap2(uint16_t x) { return (x >> 8) | (x << 8); }

static inline uint32_t CRYPTO_bswap4(uint32_t x) {
  x = (x >> 16) | (x << 16);
  x = ((x & 0xff00ff00) >> 8) | ((x & 0x00ff00ff) << 8);
  return x;
}

static inline uint64_t CRYPTO_bswap8(uint64_t x) {
  return CRYPTO_bswap4(x >> 32) | (((uint64_t)CRYPTO_bswap4(x)) << 32);
}
#endif

// Loads and stores.
//
// The following functions load and store sized integers with the specified
// endianness. They use |memcpy|, and so avoid alignment or strict aliasing
// requirements on the input and output pointers.

static inline uint16_t CRYPTO_load_u16_be(const void *in) {
  uint16_t v;
  OPENSSL_memcpy(&v, in, sizeof(v));
  return CRYPTO_bswap2(v);
}

static inline void CRYPTO_store_u16_be(void *out, uint16_t v) {
  v = CRYPTO_bswap2(v);
  OPENSSL_memcpy(out, &v, sizeof(v));
}

static inline uint32_t CRYPTO_load_u32_le(const void *in) {
  uint32_t v;
  OPENSSL_memcpy(&v, in, sizeof(v));
  return v;
}

static inline void CRYPTO_store_u32_le(void *out, uint32_t v) {
  OPENSSL_memcpy(out, &v, sizeof(v));
}

static inline uint32_t CRYPTO_load_u32_be(const void *in) {
  uint32_t v;
  OPENSSL_memcpy(&v, in, sizeof(v));
  return CRYPTO_bswap4(v);
}

static inline void CRYPTO_store_u32_be(void *out, uint32_t v) {
  v = CRYPTO_bswap4(v);
  OPENSSL_memcpy(out, &v, sizeof(v));
}

static inline uint64_t CRYPTO_load_u64_le(const void *in) {
  uint64_t v;
  OPENSSL_memcpy(&v, in, sizeof(v));
  return v;
}

static inline void CRYPTO_store_u64_le(void *out, uint64_t v) {
  OPENSSL_memcpy(out, &v, sizeof(v));
}

static inline uint64_t CRYPTO_load_u64_be(const void *ptr) {
  uint64_t ret;
  OPENSSL_memcpy(&ret, ptr, sizeof(ret));
  return CRYPTO_bswap8(ret);
}

static inline void CRYPTO_store_u64_be(void *out, uint64_t v) {
  v = CRYPTO_bswap8(v);
  OPENSSL_memcpy(out, &v, sizeof(v));
}

static inline crypto_word_t CRYPTO_load_word_le(const void *in) {
  crypto_word_t v;
  OPENSSL_memcpy(&v, in, sizeof(v));
  return v;
}

static inline void CRYPTO_store_word_le(void *out, crypto_word_t v) {
  OPENSSL_memcpy(out, &v, sizeof(v));
}

static inline crypto_word_t CRYPTO_load_word_be(const void *in) {
  crypto_word_t v;
  OPENSSL_memcpy(&v, in, sizeof(v));
#if defined(OPENSSL_64_BIT)
  static_assert(sizeof(v) == 8, "crypto_word_t has unexpected size");
  return CRYPTO_bswap8(v);
#else
  static_assert(sizeof(v) == 4, "crypto_word_t has unexpected size");
  return CRYPTO_bswap4(v);
#endif
}

// This is a generic 32-bit "collector" for message digest algorithms. It
// collects input character stream into chunks of 32-bit values and invokes the
// block function that performs the actual hash calculations.
//
// To make use of this mechanism, the hash context should be defined with the
// following parameters.
//
//     typedef struct <name>_state_st {
//       uint32_t h[<chaining length> / sizeof(uint32_t)];
//       uint32_t Nl, Nh;
//       uint8_t data[<block size>];
//       unsigned num;
//       ...
//     } <NAME>_CTX;
//
// <chaining length> is the output length of the hash in bytes, before
// any truncation (e.g. 64 for SHA-224 and SHA-256, 128 for SHA-384 and
// SHA-512).
//
// |h| is the hash state and is updated by a function of type
// |crypto_md32_block_func|. |data| is the partial unprocessed block and has
// |num| bytes. |Nl| and |Nh| maintain the number of bits processed so far.

// A crypto_md32_block_func should incorporate |num_blocks| of input from |data|
// into |state|. It is assumed the caller has sized |state| and |data| for the
// hash function.
typedef void (*crypto_md32_block_func)(uint32_t *state, const uint8_t *data,
                                       size_t num_blocks);

// crypto_md32_update adds |len| bytes from |in| to the digest. |data| must be a
// buffer of length |block_size| with the first |*num| bytes containing a
// partial block. This function combines the partial block with |in| and
// incorporates any complete blocks into the digest state |h|. It then updates
// |data| and |*num| with the new partial block and updates |*Nh| and |*Nl| with
// the data consumed.
static inline void crypto_md32_update(crypto_md32_block_func block_func,
                                      uint32_t *h, uint8_t *data,
                                      size_t block_size, unsigned *num,
                                      uint32_t *Nh, uint32_t *Nl,
                                      const uint8_t *in, size_t len) {
  if (len == 0) {
    return;
  }

  uint32_t l = *Nl + (((uint32_t)len) << 3);
  if (l < *Nl) {
    // Handle carries.
    (*Nh)++;
  }
  *Nh += (uint32_t)(len >> 29);
  *Nl = l;

  size_t n = *num;
  if (n != 0) {
    if (len >= block_size || len + n >= block_size) {
      OPENSSL_memcpy(data + n, in, block_size - n);
      block_func(h, data, 1);
      n = block_size - n;
      in += n;
      len -= n;
      *num = 0;
      // Keep |data| zeroed when unused.
      OPENSSL_memset(data, 0, block_size);
    } else {
      OPENSSL_memcpy(data + n, in, len);
      *num += (unsigned)len;
      return;
    }
  }

  n = len / block_size;
  if (n > 0) {
    block_func(h, in, n);
    n *= block_size;
    in += n;
    len -= n;
  }

  if (len != 0) {
    *num = (unsigned)len;
    OPENSSL_memcpy(data, in, len);
  }
}

// crypto_md32_final incorporates the partial block and trailing length into the
// digest state |h|. The trailing length is encoded in little-endian if
// |is_big_endian| is zero and big-endian otherwise. |data| must be a buffer of
// length |block_size| with the first |*num| bytes containing a partial block.
// |Nh| and |Nl| contain the total number of bits processed. On return, this
// function clears the partial block in |data| and
// |*num|.
//
// This function does not serialize |h| into a final digest. This is the
// responsibility of the caller.
static inline void crypto_md32_final(crypto_md32_block_func block_func,
                                     uint32_t *h, uint8_t *data,
                                     size_t block_size, unsigned *num,
                                     uint32_t Nh, uint32_t Nl,
                                     int is_big_endian) {
  // |data| always has room for at least one byte. A full block would have
  // been consumed.
  size_t n = *num;
  assert(n < block_size);
  data[n] = 0x80;
  n++;

  // Fill the block with zeros if there isn't room for a 64-bit length.
  if (n > block_size - 8) {
    OPENSSL_memset(data + n, 0, block_size - n);
    n = 0;
    block_func(h, data, 1);
  }
  OPENSSL_memset(data + n, 0, block_size - 8 - n);

  // Append a 64-bit length to the block and process it.
  if (is_big_endian) {
    CRYPTO_store_u32_be(data + block_size - 8, Nh);
    CRYPTO_store_u32_be(data + block_size - 4, Nl);
  } else {
    CRYPTO_store_u32_le(data + block_size - 8, Nl);
    CRYPTO_store_u32_le(data + block_size - 4, Nh);
  }
  block_func(h, data, 1);
  *num = 0;
  OPENSSL_memset(data, 0, block_size);
}

// Bit rotation functions.
//
// Note these functions use |(-shift) & 31|, etc., because shifting by the bit
// width is undefined. Both Clang and GCC recognize this pattern as a rotation,
// but MSVC does not. Instead, we call MSVC's built-in functions.

static inline uint32_t CRYPTO_rotl_u32(uint32_t value, int shift) {
#if defined(_MSC_VER)
  return _rotl(value, shift);
#else
  return (value << shift) | (value >> ((-shift) & 31));
#endif
}

static inline uint32_t CRYPTO_rotr_u32(uint32_t value, int shift) {
#if defined(_MSC_VER)
  return _rotr(value, shift);
#else
  return (value >> shift) | (value << ((-shift) & 31));
#endif
}

static inline uint64_t CRYPTO_rotl_u64(uint64_t value, int shift) {
#if defined(_MSC_VER)
  return _rotl64(value, shift);
#else
  return (value << shift) | (value >> ((-shift) & 63));
#endif
}

static inline uint64_t CRYPTO_rotr_u64(uint64_t value, int shift) {
#if defined(_MSC_VER)
  return _rotr64(value, shift);
#else
  return (value >> shift) | (value << ((-shift) & 63));
#endif
}

#if defined(__cplusplus)
}  // extern C
#endif

#endif  // OPENSSL_HEADER_DIGEST_MD32_COMMON_H
