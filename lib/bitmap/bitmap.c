/**
 * \file
 * \brief General Numa functions
 *
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <bitmap.h>

///< the bitmap data type to store the bitmap elements
typedef uint32_t bitmap_data_t;

///< helper macro for
#define BITMAP_BITS_PER_ELEMENT (8 * sizeof(bitmap_data_t))

///< helper macro for determining the number of
#define BITMAP_DATA_SIZE(nbits) \
     ((nbits + (BITMAP_BITS_PER_ELEMENT-1)) / BITMAP_BITS_PER_ELEMENT)



/**
 * \brief bitmap struct internal representation
 */
struct bitmap
{
    uint32_t       nbits;   ///< the number of bits this bitmap has
    uint32_t       weight;  ///< caches the number of set bits
    uint32_t       first;   ///< caches the position of the first bit
    uint32_t       last;    ///< caches the number of the last bit
    bitmap_data_t *data;    ///< stores the bit map
};

/*
 * =============================================================================
 * bitmap allocation and free
 * =============================================================================
 */

/**
 * \brief allocates a new bitmap of a given size
 *
 * \param nbits size of the bitmap
 *
 * \return pointer to the allocated bitmap on success
 *         NULL on failure
 */
struct bitmap *bitmap_alloc(uint32_t nbits)
{
    struct bitmap *bm = calloc(1, sizeof(*bm) + BITMAP_DATA_SIZE(nbits));
    if (bm == NULL) {
        return 0;
    }

    bm->nbits = nbits;
    bm->data = (bitmap_data_t *)(bm + 1);

    return bm;
}

/**
 * \brief frees the resources of a bitmap
 *
 * \param bm    bitmap to be freed
 */
void bitmap_free(struct bitmap *bm)
{
    if (bm) {
        free(bm);
    }
}

/*
 * =============================================================================
 * bitmap intput and output
 * =============================================================================
 */

/**
 * \brief formats the contents of the bitmap into the outbut buffer
 *
 * \param outbuf    buffer to hold the output data
 * \param length    length of the output buffer
 * \param bm        bitmap to format
 * \param hex       flag to enable hexadecimal formatted output
 *
 * \return number of bytes written into the buffer
 */
size_t bitmap_format(char *outbuf, size_t length, struct bitmap *bm, uint8_t hex)
{
    assert(!"NYI");
    return 0;
}

/**
 * \brief parses an input string and stores it into the bitmap
 *
 * \param outbm     bitmap to store the parsed data into
 * \param inbuf     input string buffer
 * \param length    length of the input string buffer
 * \param hex       parse input as hex
 *
 * \return number of bytes parsed from the input string
 */
size_t bitmap_parse(struct bitmap *outbm, char *inbuf, size_t length, uint8_t hex)
{
    assert(!"NYI");
    return 0;
}

/**
 * \brief serializes the bitmap into a buffer
 *
 * \param dest      buffer to store the serialized bitmap
 * \param length    lenght ouf the buffer
 * \param bm        bitmap to serialize
 *
 * \return
 */
errval_t bitmap_serialize(void *dest, size_t length, const struct bitmap *bm)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/**
 * \brief deserializes a bitmap form a buffer
 *
 * \param bm        bitmap to store the serialized data
 * \param src       source buffer to read the serialized data from
 * \param length    length of the source buffer
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t bitmap_deserialize(struct bitmap *bm, const void *src, size_t length)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/*
 * =============================================================================
 * meta information queries
 * =============================================================================
 */

/**
 * \brief returns the number of bytes in the bitmap data representation
 *
 * \param bm    bitmap to determine the bytes for
 *
 * \return size of the bitmap in bytes
 */
uint32_t bitmap_get_nbytes(const struct bitmap *bm)
{
    return (bitmap_bit_t)(BITMAP_DATA_SIZE(bm->nbits) * sizeof(bitmap_data_t));
}

/**
 * \brief gets the number of bits of this bitmap
 *
 * \param bm    the bitmap
 *
 * \return size of the bitmap in bits
 */
uint32_t bitmap_get_nbits(const struct bitmap *bm)
{
    return bm->nbits;
}

/**
 * \brief gets the weight of the bit map i.e. number of set bits
 *
 * \param bm    the bitmap
 *
 * \return number of set bits
 */
uint32_t bitmap_get_weight(const struct bitmap *bm)
{
    return bm->weight;
}

/**
 * \brief returns a pointer to the raw bitmap
 *
 * \param bm    the bitmap
 *
 * \returns Pointer to the raw bitmap
 */
void *bitmap_raw(struct bitmap *bm)
{
    return bm->data;
}

/*
 * =============================================================================
 * bitmap queries
 * =============================================================================
 */

/**
 * \brief ckecks if a given bit is set
 *
 * \param bm    bitmap to check
 * \param i     index of the bit
 *
 * \return TRUE iff the bit is set
 *         FALSE otherwise (also if bit index exceeds bitmap size)
 */
bool bitmap_is_bit_set(const struct bitmap *bm, bitmap_bit_t i)
{
    if (i < bm->nbits) {
        bitmap_data_t data = bm->data[i/BITMAP_BITS_PER_ELEMENT];
        return (data >> (i % BITMAP_BITS_PER_ELEMENT)) & 1;
    } else {
        return 0;
    }
}

/**
 * \brief ckecks if a given bit is cleare
 *
 * \param bm    bitmap to check
 * \param i     index of the bit
 *
 * \return TRUE iff the bit is clear
 *         FALSE otherwise
 */
bool bitmap_is_bit_clear(const struct bitmap *bm, bitmap_bit_t i)
{
    return !bitmap_is_bit_set(bm, i);
}

/**
 * \brief checks if all bits are set
 *
 * \param bm    bitmap to check
 *
 * \return TRUE iff all bits are set
 *         FALSE otherwise
 */
bool bitmap_is_all_set(const struct bitmap *bm)
{
    return (bm->weight == bm->nbits);
}

/**
 * \brief checks if all bits are clear
 *
 * \param bm    bitmap to check
 *
 * \return TRUE iff all bits are clear
 *         FALSE otherwise
 */
bool bitmap_is_all_clear(const struct bitmap *bm)
{
    return (bm->weight == 0);
}

/**
 * \brief returns the bit number of the first set
 *
 * \param bm    bitmap to get the first set bit
 *
 * \returns index of the first bit
 *          BITMAP_BIT_NONE if there is no bit set
 */
bitmap_bit_t bitmap_get_first(const struct bitmap *bm)
{
    if (bm->weight) {
        return bm->first;
    }
    return BITMAP_BIT_NONE;
}


/**
 * \brief returns the bit number of the last set
 *
 * \param bm    bitmap to get the last set bit
 *
 * \returns index of the last bit
 *          BITMAP_BIT_NONE if there is no bit set
 */
bitmap_bit_t bitmap_get_last(const struct bitmap *bm)
{
    if (bm->weight) {
        return bm->last;
    }
    return BITMAP_BIT_NONE;
}

/**
 * \brief gets the index of the next bit set
 *
 * \param bm    the bitmap to check
 * \param i     bit to start checking from (not inclusive)
 *
 * \return  index of the next set bit
 *          BITMAP_BIT_NONE if none is set
 */
bitmap_bit_t bitmap_get_next(const struct bitmap *bm, bitmap_bit_t i)
{
    for (bitmap_bit_t k = i+1; k < bm->nbits; ++k) {
        if (bitmap_is_bit_set(bm, k)) {
            return k;
        }
    }
    return BITMAP_BIT_NONE;
}

/**
 * \brief gets the index of the previous bit set
 *
 * \param bm    the bitmap to check
 * \param i     index of the bit to check
 *
 * \return  index of the next set bit
 *          BITMAP_BIT_NONE if none is set
 */
bitmap_bit_t bitmap_get_prev(const struct bitmap *bm, bitmap_bit_t i)
{
    if (i < bm->nbits) {
        for (bitmap_bit_t k = i-1; k >= 0; --k) {
            if (bitmap_is_bit_set(bm, k)) {
                return k;
            }
        }
    }

    return BITMAP_BIT_NONE;
}

/*
 * =============================================================================
 * Bitmap Manipulations
 * =============================================================================
 */

/**
 * \brief sets a bit in the bitmap
 *
 * \param bm    bitmap to set the bit
 * \param i     index of the bit to set
 */
void bitmap_set_bit(struct bitmap *bm, bitmap_bit_t i)
{
    if (i < bm->nbits) {
        bitmap_data_t data = bm->data[i/BITMAP_BITS_PER_ELEMENT];
        bitmap_data_t mask = (1UL << (i % BITMAP_BITS_PER_ELEMENT));
        if (!(mask & data)) {
            /* bit was cleared so set it */
            data |= (1UL << (i % BITMAP_BITS_PER_ELEMENT));
            bm->data[i/BITMAP_BITS_PER_ELEMENT] = data;
            if (bm->weight) {
                if (bm->last < i) {
                    bm->last = i;
                } else if (bm->first > i) {
                    bm->first = i;
                }
            } else {
                bm->first = i;
                bm->last = i;
            }
            bm->weight++;
        }
    }
}

/**
 * \brief clears a bit in the bitmap
 *
 * \param bm    bitmap to set the bit
 * \param i     index of the bit to clear
 */
void bitmap_clear_bit(struct bitmap *bm, bitmap_bit_t i)
{
    if (i < bm->nbits) {
        bitmap_data_t data = bm->data[i/BITMAP_BITS_PER_ELEMENT];
        bitmap_data_t mask = 1UL << (i % BITMAP_BITS_PER_ELEMENT);
        if (data & mask) {
            /* bit was set so clear it */
            bm->weight--;
            data &= ~(mask);
            bm->data[i/BITMAP_BITS_PER_ELEMENT] = data;
            if (bm->weight) {
                if (bm->last == i) {
                    bm->last = bitmap_get_prev(bm, i);
                } else if (bm->first == i) {
                    bm->first = bitmap_get_next(bm, i);
                }
            } else {
                bm->first = BITMAP_BIT_NONE;
                bm->last = BITMAP_BIT_NONE;
            }
        }
    }
}

/**
 * \brief sets all the bits in the bitmap
 *
 * \param bm    bitmap to set all bits
 */
void bitmap_set_all(struct bitmap *bm)
{
    memset(bm->data, 0xFF, bitmap_get_nbytes(bm));
    bm->weight = bm->nbits;
    bm->last = bm->nbits - 1;
    bm->first = 0;
}

/**
 * \brief clears all the bits in the bitmap
 *
 * \param bm    bitmap to clear all bits
 */
void bitmap_clear_all(struct bitmap *bm)
{
    memset(bm->data, 0, bitmap_get_nbytes(bm));
    bm->weight = 0;
    bm->first = BITMAP_BIT_NONE;
    bm->last = BITMAP_BIT_NONE;
}

/**
 * \brief sets a range of bits in the bitmap
 *
 * \param bm    bitmap to set the range
 * \param from  start of the range (including)
 * \param to    end of the range (including)
 */
void bitmap_set_range(struct bitmap *bm, bitmap_bit_t from, bitmap_bit_t to)
{
    if (to > bm->nbits) {
        to = bm->nbits - 1;
    }
    for (bitmap_bit_t i = from; i <= to; ++i) {
        bitmap_set_bit(bm, i);
    }
}

/**
 * \brief sets a range of bits in the bitmap
 *
 * \param bm    bitmap to set the range
 * \param from  start of the range (including)
 * \param to    end of the range (including)
 */
void bitmap_clear_range(struct bitmap *bm, bitmap_bit_t from, bitmap_bit_t to)
{
    if (to > bm->nbits) {
        to = bm->nbits - 1;
    }
    for (bitmap_bit_t i = from; i <= to; ++i) {
        bitmap_clear_bit(bm, i);
    }
}

/**
 * \brief clears the bitmap except for the given range
 *
 * \param bm    bitmap to set the range
 * \param from  start of the range (including)
 * \param to    end of the range (including)
 */
void bitmap_keep_range(struct bitmap *bm, uint32_t from, uint32_t to)
{
    if (from != 0) {
        bitmap_clear_range(bm, 0, from - 1);
    }

    if (to != bm->nbits - 1) {
        bitmap_clear_range(bm, to, bm->nbits);
    }
}

/**
 * \brief copies one bitmap onto another
 *
 * \param to    bitmap to copy into
 * \param from  bitmap to copy from
 *
 * only the minimum number of bits in both bitmaps are copied. If the source is
 * smaller than the destination remaining bits are cleared
 */
void bitmap_copy(struct bitmap *to, const struct bitmap *from)
{
    bitmap_clear_all(to);
    bitmap_bit_t nbytes;
    if (to->nbits < from->nbits) {
        nbytes = bitmap_get_nbytes(to);
    } else {
        nbytes = bitmap_get_nbytes(from);
    }
    memcpy(to->data, from->data, nbytes);

    /* TODO: set the meta information */
    assert(!"NYI? setting meta information");
}


/*
 * =============================================================================
 * Bitmap Comparisons
 * =============================================================================
 */

/**
 * \brief determines if two bitmaps are equal
 *
 * \param bm1   the first bitmap
 * \param bm2   the second bitmap
 *
 * \return TRUE if the two bitmaps are equal
 *         FALSE otherwise
 */
bool bitmap_equal(const struct bitmap *bm1, const struct bitmap *bm2)
{
    /* the pointers are equal */
    if (bm1 == bm2) {
        return 1;
    }
    /* the sizes do not match, not equal */
    if (bm1->nbits != bm2->nbits) {
        return 0;
    }

    bitmap_bit_t dst_bytes = BITMAP_DATA_SIZE(bm1->nbits);
    bitmap_bit_t src_bytes = BITMAP_DATA_SIZE(bm2->nbits);

    for (bitmap_bit_t i = 0; i < dst_bytes; ++i) {
        if (i < src_bytes) {
            if (bm1->data[i] != bm2->data[i]) {
                return 0;
            }
        }
    }

    return 1;
}

/**
 * \brief determines if one bitmask is a subset of the other
 *
 * \param haystack   the original bitmap
 * \param needle     the potential subset bitmap
 *
 * \return TRUE if the second bitmap is a subset of the other
 *         FALSE otherwise
 */
bool bitmap_subset(const struct bitmap *haystack, const struct bitmap *needle)
{
    assert(!"NYI");
    return 0;
}

/**
 * \brief determines if two bitmaps are disjoint i.e. not sharing a set bit
 *
 * \param bm1   the first bitmap
 * \param bm2   the second bitmap
 *
 * \return TRUE if the two bitmaps are disjoint
 *         FALSE otherwise
 */
bool bitmap_disjoint(const struct bitmap *bm1, const struct bitmap *bm2)
{
    bitmap_bit_t dst_bytes = BITMAP_DATA_SIZE(bm1->nbits);
    bitmap_bit_t src_bytes = BITMAP_DATA_SIZE(bm2->nbits);

    for (bitmap_bit_t i = 0; i < dst_bytes; ++i) {
        if (i < src_bytes) {
            if (bm1->data[i] & bm2->data[i]) {
                return 0;
            }
        }
    }

    return 1;
}

/**
 * \brief determines if two bitmaps are intersecting i.e. sharing a set bit
 *
 * \param bm1   the first bitmap
 * \param bm2   the second bitmap
 *
 * \return TRUE if the two bitmaps are equal
 *         FALSE otherwise
 */
bool bitmap_intersects(const struct bitmap *bm1, const struct bitmap *bm2)
{
    bitmap_bit_t dst_bytes = BITMAP_DATA_SIZE(bm1->nbits);
    bitmap_bit_t src_bytes = BITMAP_DATA_SIZE(bm2->nbits);

    for (bitmap_bit_t i = 0; i < dst_bytes; ++i) {
        if (i < src_bytes) {
            if (bm1->data[i] & bm2->data[i]) {
                return 1;
            }
        }
    }

    return 0;
}

/*
 * =============================================================================
 * Bitmap Operations
 * =============================================================================
 */

/**
 * \brief computes the complement to the bitmap
 *
 * \param bm the bitmap
 */
void bitmap_complement(struct bitmap *bm)
{
    bitmap_bit_t dst_bytes = BITMAP_DATA_SIZE(bm->nbits) - 1;

    for (bitmap_bit_t i = 0; i < dst_bytes; ++i) {
        bm->data[i] = ~bm->data[i];
    }

    for (bitmap_bit_t i = (dst_bytes * sizeof(bitmap_data_t)); i < bm->nbits; ++i) {
        if (bitmap_is_bit_set(bm, i)) {
            bitmap_clear_bit(bm, i);
        } else {
            bitmap_set_bit(bm, i);
        }
    }
}

/**
 * \brief performs a right shift operation on the bitmap
 *
 * \param bm    the bitmap
 * \param n     number of bits to shift
 */
void bitmap_shift_right(struct bitmap *bm, bitmap_bit_t n)
{
    assert(!"NYI");
}

/**
 * \brief performs a left shift operation on the bitmap
 *
 * \param bm    the bitmap
 * \param n     number of bits to shift
 */
void bitmap_shift_left(struct bitmap *bm, bitmap_bit_t n)
{
    assert(!"NYI");
}

/**
 * \brief performs a logical AND operation between two bitmaps
 *
 * \param dst   destination to store the new bitmap
 * \param src   source bitmap
 *
 * dst = dst AND src
 */
void bitmap_and(struct bitmap *dst, const struct bitmap *src)
{
    bitmap_bit_t dst_bytes = BITMAP_DATA_SIZE(dst->nbits);
    bitmap_bit_t src_bytes = BITMAP_DATA_SIZE(dst->nbits);

    for (bitmap_bit_t i = 0; i < dst_bytes; ++i) {
        if (i < src_bytes) {
            dst->data[i] = ~(dst->data[i] & src->data[i]);
        } else {
            dst->data[i] = 0;
        }
    }
}

/**
 * \brief performs a logical NAND operation between two bitmaps
 *
 * \param dst   destination to store the new bitmap
 * \param src   source bitmap
 *
 * dst = dst NAND src
 */
void bitmap_nand(struct bitmap *dst, const  struct bitmap *src)
{
    bitmap_bit_t dst_bytes = BITMAP_DATA_SIZE(dst->nbits);
    bitmap_bit_t src_bytes = BITMAP_DATA_SIZE(dst->nbits);

    for (bitmap_bit_t i = 0; i < dst_bytes; ++i) {
        if (i < src_bytes) {
            dst->data[i] = ~(dst->data[i] & src->data[i]);
        } else {
            dst->data[i] = ~((bitmap_data_t)0);
        }
    }
}

/**
 * \brief performs a logical OR operation between two bitmaps
 *
 * \param dst   destination to store the new bitmap
 * \param src   source bitmap
 *
 * dst = dst OR src
 */
void bitmap_or(struct bitmap *dst, const  struct bitmap *src)
{
    bitmap_bit_t dst_bytes = BITMAP_DATA_SIZE(dst->nbits);
    bitmap_bit_t src_bytes = BITMAP_DATA_SIZE(dst->nbits);

    for (bitmap_bit_t i = 0; i < dst_bytes; ++i) {
        if (i < src_bytes) {
            dst->data[i] = (dst->data[i] | src->data[i]);
        }
    }
}

/**
 * \brief performs a logical AND operation between two bitmaps
 *
 * \param dst   destination to store the new bitmap
 * \param src   source bitmap
 *
 * dst = dst XOR src
 */
void bitmap_xor(struct bitmap *dst, const  struct bitmap *src)
{
    bitmap_bit_t dst_bytes = BITMAP_DATA_SIZE(dst->nbits);
    bitmap_bit_t src_bytes = BITMAP_DATA_SIZE(dst->nbits);

    for (bitmap_bit_t i = 0; i < dst_bytes; ++i) {
        if (i < src_bytes) {
            dst->data[i] = (dst->data[i] ^ src->data[i]);
        }
    }
}

/*
 * =============================================================================
 * Bitmap Debug
 * =============================================================================
 */

void bitmap_dump(const struct bitmap *bm)
{
    uint8_t *data = (uint8_t *)(bm->data);

    debug_printf("dumping contents of bitmap %p\n", bm);
    debug_printf("----------------------------------\n");
    for (uint32_t i = 0; i < bitmap_get_nbytes(bm); i += 4) {
        debug_printf("bytes %u-%u:  [%02x] [%02x] [%02x] [%02x]\n",
                     i, i+3, data[i], data[i+1], data[i+2], data[i+3]);
    }
    debug_printf("----------------------------------\n");
}
