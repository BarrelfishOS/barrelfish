/*
 * ARMv8 (VMSAv8-64) page table structures
 *
 * Copyright (c) 2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_ARMV8_BARRELFISH_KPI_PAGING_H
#define TARGET_ARMV8_BARRELFISH_KPI_PAGING_H

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
 * respectively.  We disable the top-level (L0) table, giving a 512GB virtual
 * address space, with 4kB, 2MB, and 1GB pages.
 *
 * Naming convention: The "ARMv8" prefix refers to the specific configuration
 * that Barrelfish uses on ARMv8 in 64-bit mode, and not architectural
 * constants - ARMv8 processors are generally much more configurable.
 */

/* The system's base page size is 4kB, mapped in the L3 table */
#define BASE_PAGE_BITS       12
#define BASE_PAGE_SIZE       BIT(BASE_PAGE_BITS)
#define BASE_PAGE_MASK       MASK(BASE_PAGE_BITS)
#define BASE_PAGE_OFFSET(a)  ((a) & BASE_PAGE_MASK)

/* 2MB pages are mapped in the L2 table */
#define LARGE_PAGE_BITS      21
#define LARGE_PAGE_SIZE      BIT(LARGE_PAGE_BITS)
#define LARGE_PAGE_MASK      MASK(LARGE_PAGE_BITS)
#define LARGE_PAGE_OFFSET(a) ((a) & LARGE_PAGE_MASK)

/* 1GB pages are mapped in the L1 table */
#define HUGE_PAGE_BITS       30
#define HUGE_PAGE_SIZE       BIT(HUGE_PAGE_BITS)
#define HUGE_PAGE_MASK       MASK(HUGE_PAGE_BITS)
#define HUGE_PAGE_OFFSET(a)  ((a) & HUGE_PAGE_MASK)

/* All entries are 8 bytes */
#define PTABLE_ENTRY_BITS 3
#define PTABLE_ENTRY_SIZE BIT(PTABLE_ENTRY_BITS)

/* All levels resolve 9 bits (in contrast to earlier ARMs). */
#define PTABLE_BITS          9
#define PTABLE_SIZE          BIT(PTABLE_BITS + PTABLE_ENTRY_BITS)
#define PTABLE_MASK          MASK(PTABLE_BITS + PTABLE_ENTRY_BITS)
#define PTABLE_CLEAR         0 /* An invalid table entry */
#define PTABLE_NUM_ENTRIES   BIT(PTABLE_BITS)

/* Macros to extract indices from the VAddr */
#define ARMv8_L1_OFFSET(addr) \
    FIELD(HUGE_PAGE_BITS, PTABLE_BITS, (uintptr_t)addr)
#define ARMv8_L2_OFFSET(addr) \
    FIELD(LARGE_PAGE_BITS, PTABLE_BITS, (uintptr_t)addr)
#define ARMv8_L3_OFFSET(addr) \
    FIELD(BASE_PAGE_BITS, PTABLE_BITS, (uintptr_t)addr)


/* A descriptor for the next-level table.
 * These are the same at all levels. */
struct table_descriptor {
    uint64_t        type            :2;     // == 3 -> Table
    uint64_t        ign0            :10;    // Ignored
    uint64_t        base_address    :28;    // Table address
    uint64_t        sbz0            :12;    // sbz
    uint64_t        ign1            :7;     // Ignored

    /* Hierarchical lookup attributes */
    uint64_t        pxn             :1;     // Privileged eXecute Never
    uint64_t        xn              :1;     // eXecute Never
    uint64_t        ap              :2;     // Access Permissions
    uint64_t        ns              :1;     // NonSecure
};

union armv8_l1_entry {
    uint64_t raw;

    /* An invalid L1 entry */
    struct {
        uint64_t        type           :2;     // == 0 or 2 -> Invalid
    } invalid;

    /* An L1 entry for an L2 table */
    struct table_descriptor page_table;

    /* An L1 entry for a 1GB block (page) */
    struct {
        uint64_t        type            :2;     // == 1 -> Block

        /* Lower block attributes */
        uint64_t        ai              :3;
        uint64_t        ns              :1;
        uint64_t        ap              :2;     // AP
        uint64_t        sh              :2;     // AP
        uint64_t        af              :1;     // AF
        uint64_t        ng              :1;     // NG

        uint64_t        sbz0            :18;
        uint64_t        base_address    :18;    // block base address
        uint64_t        sbz1            :4;

        /* Upper block attributes */
        uint64_t        ch              :1;     // CH
        uint64_t        pxn             :1;     // PXN
        uint64_t        xn              :1;     // XN
        uint64_t        res             :4;     // Reserved
        uint64_t        ign1            :5;     // Ignored
    } block;
};

union armv8_l2_entry {
    uint64_t raw;

    /* An invalid L2 entry */
    struct {
        uint64_t        type           :2;     // == 0 or 2 -> Invalid
    } invalid;

    /* An L2 entry for an L3 table */
    struct table_descriptor page_table;

    /* An L2 entry for a 2MB block (page) */
    struct {
        uint64_t        type            :2;     // == 1 -> Block

        /* Lower block attributes */
        uint64_t        ai              :3;
        uint64_t        ns              :1;
        uint64_t        ap              :2;     // AP
        uint64_t        sh              :2;     // AP
        uint64_t        af              :1;     // AF
        uint64_t        ng              :1;     // NG

        uint64_t        sbz0            :9;
        uint64_t        base_address    :27;    // block base address
        uint64_t        sbz1            :4;

        /* Upper block attributes */
        uint64_t        ch              :1;     // CH
        uint64_t        pxn             :1;     // PXN
        uint64_t        xn              :1;     // XN
        uint64_t        res             :4;     // Reserved
        uint64_t        ign1            :5;     // Ignored
    } block;
};

union armv8_l3_entry {
    uint64_t raw;

    /* An invalid L3 entry */
    struct {
        uint64_t        type           :2;     // == 0 or 2 -> Invalid
    } invalid;

    /* An L3 entry for a 4kB page */
    struct {
        uint64_t        type            :2;     // == 1 -> Page

        /* Lower attributes */
        uint64_t        ai              :3;
        uint64_t        ns              :1;
        uint64_t        ap              :2;     // AP
        uint64_t        sh              :2;     // SH
        uint64_t        af              :1;     // AF
        uint64_t        ng              :1;     // NG

        uint64_t        base_address    :36;    // page base address

        /* Upper attributes */
        uint64_t        ch              :1;     // CH
        uint64_t        pxn             :1;     // PXN
        uint64_t        xn              :1;     // XN
        uint64_t        res             :4;     // Reserved
        uint64_t        sbz1            :5;     // Ignored
    } page;
};

enum armv8_entry_type {
    ARMv8_Ln_INVALID = 0,
    ARMv8_Ln_BLOCK   = 1,
    ARMv8_Ln_TABLE   = 3,
    ARMv8_L3_PAGE    = 3
};

/* VMSA-64 page attributes */
#define AARCH64_L3_CACHEABLE  0x00
#define AARCH64_L3_BUFFERABLE 0x00
#define AARCH64_L3_USR_RO     0xc0
#define AARCH64_L3_USR_RW     0x40
#define AARCH64_L3_USR_NONE   0x80

#define AARCH64_L2_CACHEABLE  0x08
#define AARCH64_L2_BUFFERABLE 0x04
#define AARCH64_L2_USR_RO     0x20
#define AARCH64_L2_USR_RW     0x30
#define AARCH64_L2_USR_NONE   0x10

/* Page type independent page options */
#define KPI_PAGING_FLAGS_READ    0x01
#define KPI_PAGING_FLAGS_WRITE   0x02
#define KPI_PAGING_FLAGS_EXECUTE 0x04
#define KPI_PAGING_FLAGS_NOCACHE 0x08
#define KPI_PAGING_FLAGS_MASK    0x0f

#endif // TARGET_ARMV8_BARRELFISH_KPI_PAGING_H
