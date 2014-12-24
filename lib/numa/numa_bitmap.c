/**
 * \file
 * \brief Bitmap manipulation
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

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>

#include <numa.h>
#include <numa_internal.h>
#include <bitmap.h>


/**
 * \brief parses a character string list of nodes into a bit mask.
 *
 * \param string character string to parse
 *
 * \returns NUMA bitmask on SUCCESS
 *          NULL if the string is invalid
 *
 * The string is a comma-separated list of node numbers or node ranges
 * Examples: 1-5,7,10 !4-5 +0-3
 *
 * If the string length is zero, then the numa_no_nodes_ptr is returned
 */
struct bitmap *numa_parse_nodestring(char *string)
{
    assert(!"NYI");
    return NULL;
}

/**
 * \brief parses a character string list of cpus into a bit mask.
 *
 * \param string character string to parse
 *
 * \returns NUMA bitmask on SUCCESS
 *          NULL if the string is invalid
 *
 * The string is a comma-separated list of cpu numbers or cpu ranges
 * Examples: 1-5,7,10 !4-5 +0-3
 */
struct bitmap *numa_parse_cpustring(char *string)
{
    assert(!"NYI");
    return NULL;
}

/**
 * \brief allocating a bitmap to hold all the configured CPUs
 * @return
 */
struct bitmap *numa_allocate_cpumask(void)
{
    return bitmap_alloc(numa_topology.num_cores);
}

/**
 * \brief frees a previously allocated CPU bitmask
 *
 * \param cpumask pointer to a previously allocated CPU bitmask
 */
void numa_free_cpumask(struct bitmap *cpumask)
{
    bitmap_free(cpumask);
}

/**
 * \brief allocates a bit mask to represent the nodes in the system
 *
 * \returns pointer to a new bitmask
 *          NULL on failure
 */
struct bitmap *numa_allocate_nodemask(void)
{
    return bitmap_alloc(NUMA_MAX_NUMNODES);
}

/**
 * \brief frees a previously allocated node bitmask
 *
 * \param nodemask pointer to a previously allocated node bitmask
 */
void numa_free_nodemask(struct bitmap *nodemask)
{
    bitmap_free(nodemask);
}

/**
 * \brief allocates a bitmask structure and its associated bit mask
 *
 * \param n the number of bits
 *
 * \returns pointer to the bitmask
 *          NULL on error
 */
struct bitmap *numa_bitmask_alloc(unsigned int n)
{
    return bitmap_alloc(n);
}

/**
 * \brief sets all bits in the bit mask to 0.
 *
 * \param bmp   pointer to the bitmap
 *
 * \returns pointer to the cleared bit map
 */
struct bitmap *numa_bitmask_clearall(struct bitmap *bmp)
{
    bitmap_clear_all(bmp);
    return bmp;
}

/**
 * \brief clears the n-th bit of a bitmask
 *
 * \param bmp   the bitmask
 * \param n     the bit to clear
 *
 * \returns pointer to the bitmask
 */
struct bitmap *numa_bitmask_clearbit(struct bitmap *bmp, unsigned int n)
{
    bitmap_clear_bit(bmp, n);
    return bmp;
}

/**
 * \brief checks if two bitmasks are equal
 *
 * \param bmp1  bitmask 1
 * \param bmp2  bitmask 2
 *
 * \return TRUE if the bitmasks are equal
 *         FALSE if the are distinct
 */
bool numa_bitmask_equal(const struct bitmap *bmp1, const struct bitmap *bmp2)
{
    return bitmap_equal(bmp1, bmp2);
}

/**
 * \brief frees the memory of a bitmask
 *
 * \param bmp the bitmask to be freed
 */
void numa_bitmask_free(struct bitmap *bmp)
{
    bitmap_free(bmp);
}

/**
 * \brief checks if the n-th bit is set in the bitmask
 *
 * \param bmp   the bitmap
 * \param n     which bit to check
 *
 * \returns TRUE if the n-th bit is set
 *          FALSE otherwise
 */
bool numa_bitmask_isbitset(const struct bitmap *bmp, unsigned int n)
{
    return bitmap_is_bit_set(bmp, n);
}

/**
 * \brief returns the size (in bytes) of the bit mask
 *
 * \param bmp   the bitmask
 *
 * \returns the size of the memory in bytes rounded up to a multiple of wordsize
 */
size_t numa_bitmask_nbytes(struct bitmap *bmp)
{
    return bitmap_get_nbytes(bmp);
}

/**
 * \brief sets all bits of a bitmask to 1
 *
 * \param bmp the bitmask
 *
 * \returns the bitmask
 */
struct bitmap *numa_bitmask_setall(struct bitmap *bmp)
{
    bitmap_set_all(bmp);
    return bmp;
}

/**
 * \brief sets the n-th bit of a bitmask to 1
 *
 * \param bmp   the bitmask
 * \param n     which bit to activate
 *
 * \returns the bitmask
 */
struct bitmap *numa_bitmask_setbit(struct bitmap *bmp, unsigned int n)
{
    bitmap_set_bit(bmp, n);
    return bmp;
}

/**
 * \brief copies the bitmask to a nodemask
 *
 * \param bmp       the bitmask to copy
 * \param nodemask  the destination nodemask
 *
 * If the two areas differ in size, the copy is truncated to the size of the
 * receiving field or zero-filled.
 */
void copy_bitmask_to_nodemask(struct bitmap *bmp, nodemask_t *nodemask)
{
    assert(!"NYI*");
}

/**
 * \brief copies the contents of a nodemask into the bitmask
 *
 * \param nodemask  node mask to copy from
 * \param bmp       bitmap to copy into
 *
 * If the two areas differ in size, the copy is truncated to the size of the
 * receiving field or zero-filled.
 */
void copy_nodemask_to_bitmask(nodemask_t *nodemask, struct bitmap *bmp)
{
    assert(!"NYI*");
}


/**
 * \brief copies one bitmask into another
 *
 * \param bmpfrom   the source bitmask
 * \param bmpto     the destination bitmask
 *
 * If the two areas differ in size, the copy is truncated to the size of the
 * receiving field or zero-filled.
 */
void copy_bitmask_to_bitmask(struct bitmap *bmpfrom, struct bitmap *bmpto)
{
    bitmap_copy(bmpto, bmpfrom);
}

/**
 * \brief returns a count of the bits that are set in the body of the bitmask
 *
 * \param bmp   the bitmask to count the set bits
 *
 * \return number of set bits in this bitmask
 */
uint32_t numa_bitmask_weight(const struct bitmap *bmp)
{
    return bitmap_get_weight(bmp);
}

