/**
 * \file
 * \brief header specifying the interface of libnuma
 *
 * This is derived from:
 *
 * Linux man pages "numa"
 * libnuma from http://oss.sgi.com/projects/libnuma/
 *
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */

#ifndef __BITMAP_H
#define __BITMAP_H 1

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Bitmap implementation
 *
 * - the bitmap is zero indexed
 */

///< bitmap structure public declaration
struct bitmap;

typedef int32_t bitmap_bit_t;

///< value returned if the bits are invalid.
#define BITMAP_BIT_NONE ((bitmap_bit_t)-1)

///< bitmap list separator
#define BITMAP_LIST_SEPARATOR ','

/* allocation and free */
struct bitmap *bitmap_alloc(uint32_t n);
void bitmap_free(struct bitmap *bm);

/* intput/output */
size_t bitmap_format(char *outbuf, size_t length, struct bitmap *bm, uint8_t hex);
size_t bitmap_parse(struct bitmap *outbm, char *inbuf, size_t length, uint8_t hex);
errval_t bitmap_serialize(void *dest, size_t length, const struct bitmap *bm);
errval_t bitmap_deserialize(struct bitmap *bm, const void *src, size_t length);

/* bitmap meta information queries */
uint32_t bitmap_get_nbytes(const struct bitmap *bm);
uint32_t bitmap_get_nbits(const struct bitmap *bm);
uint32_t bitmap_get_weight(const struct bitmap *bm);
void *bitmap_raw(struct bitmap *bm);

/* bitmap queries */
bool bitmap_is_bit_set(const struct bitmap *bm, bitmap_bit_t i);
bool bitmap_is_bit_clear(const struct bitmap *bm, bitmap_bit_t i);
bool bitmap_is_all_set(const struct bitmap *bm);
bool bitmap_is_all_clear(const struct bitmap *bm);
bitmap_bit_t bitmap_get_first(const struct bitmap *bm);
bitmap_bit_t bitmap_get_next(const struct bitmap *bm, bitmap_bit_t i);
bitmap_bit_t bitmap_get_prev(const struct bitmap *bm, bitmap_bit_t i);
bitmap_bit_t bitmap_get_last(const struct bitmap *bm);

/* Bitmap Manipulations */
void bitmap_set_bit(struct bitmap *bm, bitmap_bit_t i);
void bitmap_clear_bit(struct bitmap *bm, bitmap_bit_t i);
void bitmap_set_all(struct bitmap *bm);
void bitmap_clear_all(struct bitmap *bm);
void bitmap_set_range(struct bitmap *bm, bitmap_bit_t from, bitmap_bit_t to);
void bitmap_clear_range(struct bitmap *bm, bitmap_bit_t from, bitmap_bit_t tp);
void bitmap_keep_range(struct bitmap *bm, uint32_t i, uint32_t j);

void bitmap_copy(struct bitmap *to, const struct bitmap *from);

/* bitmap comparisons */
bool bitmap_equal(const struct bitmap *bm1, const struct bitmap *bm2);
bool bitmap_subset(const struct bitmap *bm1, const struct bitmap *bm2);
bool bitmap_disjoint(const struct bitmap *bm1, const struct bitmap *bm2);
bool bitmap_intersects(const struct bitmap *bm1, const struct bitmap *bm2);

/* bitmap operations */
void bitmap_complement(struct bitmap *bm);
void bitmap_shift_right(struct bitmap *bm, bitmap_bit_t n);
void bitmap_shift_left(struct bitmap *bm, bitmap_bit_t n);
void bitmap_and(struct bitmap *dst, const struct bitmap *src);
void bitmap_nand(struct bitmap *dst, const  struct bitmap *src);
void bitmap_or(struct bitmap *dst, const  struct bitmap *src);
void bitmap_xor(struct bitmap *dst, const  struct bitmap *src);

/* debug operations */
void bitmap_dump(const struct bitmap *bm);

#ifdef __cplusplus
}
#endif

#endif /* __BITMAP_H */
