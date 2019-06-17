/*
 * Copyright (c) 2015, ETH Zuerich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __VM_H
#define __VM_H

#include <stdint.h>

#define ARMv8_BLOCK_BITS 9
#define ARMv8_BLOCK_SIZE (1 << ARMv8_BLOCK_BITS)
#define ARMv8_BLOCK_MASK (ARMv8_BLOCK_SIZE - 1)

/** The system's base page size is 4kB */
#define ARMv8_BASE_PAGE_BITS                  12
#define ARMv8_BASE_PAGE_SIZE                  (1ULL<<ARMv8_BASE_PAGE_BITS)
#define ARMv8_BASE_PAGE_MASK                  (ARMv8_BASE_PAGE_SIZE - 1)
#define ARMv8_BASE_PAGE_OFFSET(a)             ((a) & ARMv8_BASE_PAGE_MASK)

/** The system's large page size is 2MB */
#define ARMv8_LARGE_PAGE_BITS                  21
#define ARMv8_LARGE_PAGE_SIZE                  (1ULL<<ARMv8_LARGE_PAGE_BITS)
#define ARMv8_LARGE_PAGE_MASK                  (ARMv8_LARGE_PAGE_SIZE - 1)
#define ARMv8_LARGE_PAGE_OFFSET(a)             ((a) & ARMv8_LARGE_PAGE_MASK)

/** The system's huge page size is 1GB */
#define ARMv8_HUGE_PAGE_BITS                  30
#define ARMv8_HUGE_PAGE_SIZE                  (1ULL<<ARMv8_HUGE_PAGE_BITS)
#define ARMv8_HUGE_PAGE_MASK                  (ARMv8_HUGE_PAGE_SIZE - 1)
#define ARMv8_HUGE_PAGE_OFFSET(a)             ((a) & ARMv8_HUGE_PAGE_MASK)

/** The system's largest table granularity is 512GB */
#define ARMv8_TOP_TABLE_BITS                  39
#define ARMv8_TOP_TABLE_SIZE                  (1ULL<<ARMv8_TOP_TABLE_BITS)
#define ARMv8_TOP_TABLE_MASK                  (ARMv8_TOP_TABLE_SIZE - 1)
#define ARMv8_TOP_TABLE_OFFSET(a)             ((a) & ARMv8_TOP_TABLE_MASK)

/* These descriptors formats are valid for a 4kB translation granule. */
union aarch64_descriptor {
    uint64_t raw;
    struct {
        uint64_t valid       :1;
        uint64_t mb1         :1;    // 1 -> table, 0 -> block
        uint64_t ignored1    :10;   // lower block attrs, ignored for table
        uint64_t base        :36;   // base address of next level table
        uint64_t reserved1   :4;
        uint64_t ignored2    :7;
        uint64_t pxntable    :1;    // only stage 1, executable from EL1
        uint64_t uxntable    :1;    // only stage 1, executable from EL0
        uint64_t aptable     :2;    // only stage 1, access from EL0
        uint64_t nstable     :1;    // only stage 1, access from secure state
    } d;
    struct {
        uint64_t valid       :1;
        uint64_t mb0         :1;    // 1 -> table, 0 -> block
        uint64_t attrindex   :3;    // mem attr index field, D4-1798
        uint64_t ns          :1;    // non-secure bit
        uint64_t ap          :2;    // access permissions bits
        uint64_t sh          :2;    // shareability field
        uint64_t af          :1;    // accessed flag
        uint64_t ng          :1;    // not global bit
        uint64_t reserved1   :18;
        uint64_t base        :18;
        uint64_t reserved2   :4;
        uint64_t contiguous  :1;    // hint that entry is part of set
                                    // of contiguous entries, D4-1811
        uint64_t pxn         :1;    // privileged execute never bit
        uint64_t uxn         :1;    // (user) execute never bit
        uint64_t avail1      :4;    // available for SW use
        uint64_t ignored1    :5;
    } block_l1;
    struct {
        uint64_t valid       :1;
        uint64_t mb0         :1;    // 1 -> table, 0 -> block
        uint64_t attrindex   :3;    // mem attr index field, D4-1798
        uint64_t ns          :1;    // non-secure bit
        uint64_t ap          :2;    // access permissions bits
        uint64_t sh          :2;    // shareability field
        uint64_t af          :1;    // accessed flag
        uint64_t ng          :1;    // not global bit
        uint64_t reserved1   :9;
        uint64_t base        :27;
        uint64_t reserved2   :4;
        uint64_t contiguous  :1;    // hint that entry is part of set
                                    // of contiguous entries, D4-1811
        uint64_t pxn         :1;    // privileged execute never bit
        uint64_t uxn         :1;    // (user) execute never bit
        uint64_t avail1      :4;    // available for SW use
        uint64_t ignored1    :5;
    } block_l2;
    struct {
        uint64_t valid       :1;
        uint64_t mb1         :1;    // 0 -> makes entry invalid
        uint64_t attrindex   :3;    // mem attr index field, D4-1798
        uint64_t ns          :1;    // non-secure bit
        uint64_t ap          :2;    // access permissions bits
        uint64_t sh          :2;    // shareability field
        uint64_t af          :1;    // accessed flag
        uint64_t ng          :1;    // not global bit
        uint64_t base        :36;
        uint64_t reserved1   :4;
        uint64_t contiguous  :1;    // hint that entry is part of set
                                    // of contiguous entries, D4-1811
        uint64_t pxn         :1;    // privileged execute never bit
        uint64_t uxn         :1;    // (user) execute never bit
        uint64_t avail1      :4;    // available for SW use
        uint64_t ignored1    :5;
    } page;
};

#endif /* __VM_H */
