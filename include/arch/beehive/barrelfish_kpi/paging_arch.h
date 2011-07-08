/**
 * \file
 * \brief Arch specific paging definitions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_BARRELFISH_KPI_PAGING_H
#define ARCH_BEEHIVE_BARRELFISH_KPI_PAGING_H

// Required for various APIs to compile even if it means nothing
#define PTABLE_ACCESS_DEFAULT 0

/* Default page size is 4K */
#define BASE_PAGE_BITS          12
#define BASE_PAGE_SIZE          (1u << BASE_PAGE_BITS)
#define BASE_PAGE_MASK          (BASE_PAGE_SIZE - 1)
#define BASE_PAGE_OFFSET(a)     ((a) & BASE_PAGE_MASK)

#endif // ARCH_BEEHIVE_BARRELFISH_KPI_PAGING_H
