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
struct numa_bm *numa_parse_nodestring(char *string)
{

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
struct numa_bm *numa_parse_cpustring(char *string)
{

}

struct numa_bm *numa_allocate_cpumask();

/**
 * \brief frees a previously allocated CPU bitmask
 *
 * \param cpumask pointer to a previously allocated CPU bitmask
 */
void numa_free_cpumask(struct numa_bm *cpumask);

/**
 * \brief allocates a bit mask to represent the nodes in the system
 *
 * \returns pointer to a new bitmask
 *          NULL on failure
 */
struct numa_bm *numa_allocate_nodemask();

/**
 * \brief frees a previously allocated node bitmask
 *
 * \param nodemask pointer to a previously allocated node bitmask
 */
void numa_free_nodemask(struct numa_bm *nodemask);

/**
 * \brief allocates a bitmask structure and its associated bit mask
 *
 * \param n the number of bits
 *
 * \returns pointer to the bitmask
 *          NULL on error
 */
struct numa_bm *numa_bitmask_alloc(unsigned int n);

/**
 * \brief sets all bits in the bit mask to 0.
 *
 * \param bmp   pointer to the bitmap
 *
 * \returns pointer to the cleared bit map
 */
struct numa_bm *numa_bitmask_clearall(struct numa_bm *bmp);

/**
 * \brief clears the n-th bit of a bitmask
 *
 * \param bmp   the bitmask
 * \param n     the bit to clear
 *
 * \returns pointer to the bitmask
 */
struct numa_bm *numa_bitmask_clearbit(struct numa_bm *bmp, unsigned int n);

/**
 * \brief checks if two bitmasks are equal
 *
 * \param bmp1  bitmask 1
 * \param bmp2  bitmask 2
 *
 * \return TRUE if the bitmasks are equal
 *         FALSE if the are distinct
 */
bool numa_bitmask_equal(const struct numa_bm *bmp1, const struct numa_bm *bmp2);

/**
 * \brief frees the memory of a bitmask
 *
 * \param bmp the bitmask to be freed
 */
void numa_bitmask_free(struct numa_bm *bmp);

/**
 * \brief checks if the n-th bit is set in the bitmask
 *
 * \param bmp   the bitmap
 * \param n     which bit to check
 *
 * \returns TRUE if the n-th bit is set
 *          FALSE otherwise
 */
bool numa_bitmask_isbitset(const struct numa_bm *bmp, unsigned int n);

/**
 * \brief returns the size (in bytes) of the bit mask
 *
 * \param bmp   the bitmask
 *
 * \returns the size of the memory in bytes rounded up to a multiple of wordsize
 */
size_t numa_bitmask_nbytes(struct numa_bm *bmp);

/**
 * \brief sets all bits of a bitmask to 1
 *
 * \param bmp the bitmask
 *
 * \returns the bitmask
 */
struct numa_bm *numa_bitmask_setall(struct numa_bm *bmp);

/**
 * \brief sets the n-th bit of a bitmask to 1
 *
 * \param bmp   the bitmask
 * \param n     which bit to activate
 *
 * \returns the bitmask
 */
struct numa_bm *numa_bitmask_setbit(struct numa_bm *bmp, unsigned int n);

/**
 * \brief copies the bitmask to a nodemask
 *
 * \param bmp       the bitmask to copy
 * \param nodemask  the destination nodemask
 *
 * If the two areas differ in size, the copy is truncated to the size of the
 * receiving field or zero-filled.
 */
void copy_bitmask_to_nodemask(struct numa_bm *bmp, nodemask_t *nodemask);

/**
 * \brief copies the contents of a nodemask into the bitmask
 *
 * \param nodemask  node mask to copy from
 * \param bmp       bitmap to copy into
 *
 * If the two areas differ in size, the copy is truncated to the size of the
 * receiving field or zero-filled.
 */
void copy_nodemask_to_bitmask(nodemask_t *nodemask, struct numa_bm *bmp);

/**
 * \brief copies one bitmask into another
 *
 * \param bmpfrom   the source bitmask
 * \param bmpto     the destination bitmask
 *
 * If the two areas differ in size, the copy is truncated to the size of the
 * receiving field or zero-filled.
 */
void copy_bitmask_to_bitmask(struct numa_bm *bmpfrom, struct numa_bm *bmpto);

/**
 * \brief returns a count of the bits that are set in the body of the bitmask
 *
 * \param bmp   the bitmask to count the set bits
 *
 * \return number of set bits in this bitmask
 */
uint32_t numa_bitmask_weight(const struct numa_bm *bmp);

