/**
 * \file
 * \brief Arch specific definitions, can be included by others.
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_ARMV8_BARRELFISH_KPI_PAGING_H
#define TARGET_ARMV8_BARRELFISH_KPI_PAGING_H

#ifndef __ASSEMBLER__
typedef uint64_t paging_x86_64_flags_t;
#endif

/** The system's base page size is 4kB */
#define ARMv8_BASE_PAGE_BITS                  12
#define ARMv8_BASE_PAGE_SIZE                  (1<<ARMv8_BASE_PAGE_BITS)
#define ARMv8_BASE_PAGE_MASK                  (ARMv8_BASE_PAGE_SIZE - 1)
#define ARMv8_BASE_PAGE_OFFSET(a)             ((a) & ARMv8_BASE_PAGE_MASK)

/** The system's large page size is 2MB */
#define ARMv8_LARGE_PAGE_BITS                  21
#define ARMv8_LARGE_PAGE_SIZE                  (1<<ARMv8_LARGE_PAGE_BITS)
#define ARMv8_LARGE_PAGE_MASK                  (ARMv8_LARGE_PAGE_SIZE - 1)
#define ARMv8_LARGE_PAGE_OFFSET(a)             ((a) & ARMv8_LARGE_PAGE_MASK)

/** The system's huge page size is 1GB */
#define ARMv8_HUGE_PAGE_BITS                  30
#define ARMv8_HUGE_PAGE_SIZE                  (1<<ARMv8_HUGE_PAGE_BITS)
#define ARMv8_HUGE_PAGE_MASK                  (ARMv8_HUGE_PAGE_SIZE - 1)
#define ARMv8_HUGE_PAGE_OFFSET(a)             ((a) & ARMv8_HUGE_PAGE_MASK)

/**
 * Bits within the various page directories and tables.
 */

// TODO: check what ptable sizes are available
#define ARMv8_PTABLE_BITS         9       /**< Page directory/table size in bits */
/** Page directory/table size */
#define ARMv8_PTABLE_SIZE         (1UL<<ARMv8_PTABLE_BITS)
#define ARMv8_PTABLE_MASK         0x1ff   /**< Page dir/table address mask */
#define ARMv8_PTABLE_CLEAR        0       /**< Bitmap of a clear table entry */

// XXX: maybe sizeof(union ...)
#define ARMv8_PTABLE_ENTRY_SIZE   sizeof(uint64_t)

// XXX: These may depend on system config registers
/* Macros to compute the corresponding portions of the vaddr */
#define ARMv8_PML4_BASE(base)         (((uint64_t)(base) >> 39) & ARMv8_PTABLE_MASK)
#define ARMv8_PDPT_BASE(base)         (((uint64_t)(base) >> 30) & ARMv8_PTABLE_MASK)
#define ARMv8_PDIR_BASE(base)         (((uint64_t)(base) >> 21) & ARMv8_PTABLE_MASK)
#define ARMv8_PTABLE_BASE(base)       (((uint64_t)(base) >> 12) & ARMv8_PTABLE_MASK)

// non-prefixed versions
// XXX: should cleanup arm include mess

/** The system's base page size is 4kB */
#define BASE_PAGE_BITS                  ARMv8_BASE_PAGE_BITS
#define BASE_PAGE_SIZE                  ARMv8_BASE_PAGE_SIZE
#define BASE_PAGE_MASK                  ARMv8_BASE_PAGE_MASK
#define BASE_PAGE_OFFSET(a)             ARMv8_BASE_PAGE_OFFSET(a)

/** The system's large page size is 2MB */
#define LARGE_PAGE_BITS                  ARMv8_LARGE_PAGE_BITS
#define LARGE_PAGE_SIZE                  ARMv8_LARGE_PAGE_SIZE
#define LARGE_PAGE_MASK                  ARMv8_LARGE_PAGE_MASK
#define LARGE_PAGE_OFFSET(a)             ARMv8_LARGE_PAGE_OFFSET(a)

/** The system's huge page size is 1GB */
#define HUGE_PAGE_BITS                  ARMv8_HUGE_PAGE_BITS
#define HUGE_PAGE_SIZE                  ARMv8_HUGE_PAGE_SIZE
#define HUGE_PAGE_MASK                  ARMv8_HUGE_PAGE_MASK
#define HUGE_PAGE_OFFSET(a)             ARMv8_HUGE_PAGE_OFFSET(a)

/**
 * Bits within the various page directories and tables.
 */

// TODO: check what ptable sizes are available
#define PTABLE_BITS         ARMv8_PTABLE_BITS       /**< Page directory/table size in bits */
/** Page directory/table size */
#define PTABLE_SIZE         ARMv8_PTABLE_SIZE
#define PTABLE_MASK         ARMv8_PTABLE_MASK       /**< Page dir/table address mask */
#define PTABLE_CLEAR        ARMv8_PTABLE_CLEAR      /**< Bitmap of a clear table entry */

#define PTABLE_ENTRY_SIZE   ARMv8_PTABLE_ENTRY_SIZE

#endif // TARGET_ARMV8_BARRELFISH_KPI_PAGING_H
