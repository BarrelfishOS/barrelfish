/**
 * \file
 * \brief Arch specific paging definitions
 */

/*
 * Copyright (c) 2010, 2015, ETH Zurich.
 * Copyright (c) 2015, 2016 Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_KPI_PAGING_H
#define ARCH_AARCH64_BARRELFISH_KPI_PAGING_H

#include <target/aarch64/barrelfish_kpi/paging_arm_v8.h>

/**
 * Information about page sizes
 */
#define BASE_PAGE_BITS      VMSAv8_64_BASE_PAGE_BITS
#define BASE_PAGE_SIZE      VMSAv8_64_BASE_PAGE_SIZE
#define BASE_PAGE_MASK      VMSAv8_64_BASE_PAGE_MASK
#define BASE_PAGE_OFFSET    VMSAv8_64_BASE_PAGE_OFFSET

#define LARGE_PAGE_BITS     VMSAv8_64_L2_BLOCK_BITS
#define LARGE_PAGE_SIZE     VMSAv8_64_L2_BLOCK_SIZE
#define LARGE_PAGE_MASK     VMSAv8_64_L2_BLOCK_MASK
#define LARGE_PAGE_OFFSET   VMSAv8_64_L2_BLOCK_OFFSET

#define HUGE_PAGE_BITS      VMSAv8_64_L1_BLOCK_BITS
#define HUGE_PAGE_SIZE      VMSAv8_64_L1_BLOCK_SIZE
#define HUGE_PAGE_MASK      VMSAv8_64_L1_BLOCK_MASK
#define HUGE_PAGE_OFFSET    VMSAv8_64_L1_BLOCK_OFFSET

/**
 * Bits within the various page directories and tables.
 * XXX: THIS NEEDS TO BE FIXED!!!
 */
#define PTABLE_EXECUTE_DISABLE  0 /* VMSAv8_64_PTABLE_EXECUTE_DISABLE */
#define PTABLE_GLOBAL_PAGE      0 /* VMSAv8_64_PTABLE_GLOBAL_PAGE  */
#define PTABLE_ATTR_INDEX       0 /* VMSAv8_64_PTABLE_ATTR_INDEX  */
#define PTABLE_DIRTY            0 /* VMSAv8_64_PTABLE_DIRTY  */
#define PTABLE_ACCESSED         0 /* VMSAv8_64_PTABLE_ACCESSED  */
#define PTABLE_CACHE_DISABLED   0 /* VMSAv8_64_PTABLE_CACHE_DISABLED  */
#define PTABLE_WRITE_THROUGH    0 /* VMSAv8_64_PTABLE_WRITE_THROUGH  */
#define PTABLE_USER_SUPERVISOR  0 /* VMSAv8_64_PTABLE_USER_SUPERVISOR  */
#define PTABLE_READ_WRITE       0 /* VMSAv8_64_PTABLE_READ_WRITE  */
#define PTABLE_PRESENT          0 /* VMSAv8_64_PTABLE_PRESENT */

#define PTABLE_SIZE             VMSAv8_64_PTABLE_SIZE
#define PTABLE_ENTRIES          VMSAv8_64_PTABLE_NUM_ENTRIES
#define PTABLE_MASK             VMSAv8_64_PTABLE_MASK
#define PTABLE_CLEAR            VMSAv8_64_PTABLE_CLEAR

#define PTABLE_ACCESS_DEFAULT   (KPI_PAGING_FLAGS_READ | KPI_PAGING_FLAGS_WRITE)
#define PTABLE_ACCESS_READONLY  KPI_PAGING_FLAGS_READ

#define PTABLE_ENTRY_SIZE       VMSAv8_64_PTABLE_ENTRY_SIZE
#endif // ARCH_ARM_BARRELFISH_KPI_PAGING_H
