/*
 * ARMv8 (VMSAv8-64) page table structures
 *
 * Copyright (c) 2015, ETH Zurich.
 * Copyright (c) 2015, 2016 Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_VMSAv8_64_BARRELFISH_KPI_PAGING_H
#define TARGET_VMSAv8_64_BARRELFISH_KPI_PAGING_H

#include <bitmacros.h>

#ifndef __ASSEMBLER__
typedef uint64_t paging_x86_64_flags_t;
#endif

/* In contrast to previous ARMs, ARMv8 has up to four levels of page tables,
 * with base page size (granule) configurable to 4kB, 16kB, or 64kB.  Page
 * tables at all levels are one base-sized page.
 *
 * The current ARMv8 port of Barrelfish uses a 4kB granule, so the 4
 * translation levels map 21b (2MB), 30b (1GB), 39b (512GB) and 48b (256TB),
 * respectively.  We configure a four-level page table giving a 256TB virtual
 * address space, with 4kB, 2MB, and 1GB pages.
 *
 * Naming convention: The "VMSAv8_64" prefix refers to the specific configuration
 * that Barrelfish uses on ARMv8 in 64-bit mode, and not architectural
 * constants - ARMv8 processors are generally much more configurable.
 */

/* The system's base page size is 4kB, mapped in the L3 table */
#define VMSAv8_64_BASE_PAGE_BITS       12
#define VMSAv8_64_BASE_PAGE_SIZE       BIT(VMSAv8_64_BASE_PAGE_BITS)
#define VMSAv8_64_BASE_PAGE_MASK       MASK(VMSAv8_64_BASE_PAGE_BITS)
#define VMSAv8_64_BASE_PAGE_OFFSET(a)  ((a) & VMSAv8_64_BASE_PAGE_MASK)

/* 2MB pages are mapped in the L2 table */
#define VMSAv8_64_L2_BLOCK_BITS      21
#define VMSAv8_64_L2_BLOCK_SIZE      BIT(VMSAv8_64_L2_BLOCK_BITS)
#define VMSAv8_64_L2_BLOCK_MASK      MASK(VMSAv8_64_L2_BLOCK_BITS)
#define VMSAv8_64_L2_BLOCK_OFFSET(a) ((a) & VMSAv8_64_L2_BLOCK_MASK)

/* 1GB pages are mapped in the L1 table */
#define VMSAv8_64_L1_BLOCK_BITS       30
#define VMSAv8_64_L1_BLOCK_SIZE       BIT(VMSAv8_64_L1_BLOCK_BITS)
#define VMSAv8_64_L1_BLOCK_MASK       MASK(VMSAv8_64_L1_BLOCK_BITS)
#define VMSAv8_64_L1_BLOCK_OFFSET(a)  ((a) & VMSAv8_64_L1_BLOCK_MASK)

// L0 entry info
#define VMSAv8_64_L0_BITS              39
#define VMSAv8_64_L0_SIZE              BIT(VMSAv8_64_L0_BITS)
#define VMSAv8_64_L0_MAST              MASK(VMSAv8_64_L0_BITS)
#define VMSAv8_64_L0_OFFSET(a)         ((a) & VMSAv8_64_L0_MASK)

/* All entries are 8 bytes */
#define VMSAv8_64_PTABLE_ENTRY_BITS 3
#define VMSAv8_64_PTABLE_ENTRY_SIZE BIT(VMSAv8_64_PTABLE_ENTRY_BITS)

/* All levels resolve 9 bits (in contrast to earlier ARMs). */
#define VMSAv8_64_PTABLE_BITS          9
#define VMSAv8_64_PTABLE_SIZE          BIT(VMSAv8_64_PTABLE_BITS + VMSAv8_64_PTABLE_ENTRY_BITS)
#define VMSAv8_64_PTABLE_MASK          MASK(VMSAv8_64_PTABLE_BITS + VMSAv8_64_PTABLE_ENTRY_BITS)
#define VMSAv8_64_PTABLE_CLEAR         0 /* An invalid table entry */
#define VMSAv8_64_PTABLE_NUM_ENTRIES   BIT(VMSAv8_64_PTABLE_BITS)

/* Macros to extract indices from the VAddr */
#define VMSAv8_64_L0_BASE(addr) FIELD(VMSAv8_64_L0_BITS, VMSAv8_64_PTABLE_BITS, (uintptr_t)addr)
#define VMSAv8_64_L1_BASE(addr) FIELD(VMSAv8_64_L1_BLOCK_BITS, VMSAv8_64_PTABLE_BITS, (uintptr_t)addr)
#define VMSAv8_64_L2_BASE(addr) FIELD(VMSAv8_64_L2_BLOCK_BITS, VMSAv8_64_PTABLE_BITS, (uintptr_t)addr)
#define VMSAv8_64_L3_BASE(addr) FIELD(VMSAv8_64_BASE_PAGE_BITS, VMSAv8_64_PTABLE_BITS, (uintptr_t)addr)


/* VMSAv8-64 page attributes */
// XXX: This needs to be revised
#define VMSAv8_64_L3_CACHEABLE  (3 << 8)
#define VMSAv8_64_L3_BUFFERABLE 0x00
#define VMSAv8_64_L3_USR_RO     (3 << 6)
#define VMSAv8_64_L3_USR_RW     (1 << 6)
#define VMSAv8_64_L3_USR_NONE   0x80

#define VMSAv8_64_L2_CACHEABLE  0x08
#define VMSAv8_64_L2_BUFFERABLE 0x04
#define VMSAv8_64_L2_USR_RO     0x20
#define VMSAv8_64_L2_USR_RW     0x30
#define VMSAv8_64_L2_USR_NONE   0x10

/* Page type independent page options */
#define KPI_PAGING_FLAGS_READ    0x01
#define KPI_PAGING_FLAGS_WRITE   0x02
#define KPI_PAGING_FLAGS_EXECUTE 0x04
#define KPI_PAGING_FLAGS_NOCACHE 0x08
#define KPI_PAGING_FLAGS_MASK    0x0f

#endif // TARGET_VMSAv8_64_BARRELFISH_KPI_PAGING_H
