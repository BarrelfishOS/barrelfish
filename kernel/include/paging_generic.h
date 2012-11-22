/**
 * \file
 * \brief Kernel memory management.
 */

/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PAGING_H
#define PAGING_H

#include <barrelfish/types.h>
#include <errors/errno.h>

struct mapping_info {
    lvaddr_t pte;           ///< where the capability is mapped
    uint16_t pt_slot;       ///< the first slot number for the mapping in the page table corresponding to pte
    lvaddr_t pt2;           ///< base address of 2nd leaf level ptable
    size_t mapped_pages;    ///< the amount of currently mapped pages
    // target meta data
    size_t page_count;      ///< the targeted amount of mapped pages
    uint64_t offset;        ///< the offset into the physical region identified by the capability where the mapping begins.
};

struct cte;
errval_t compile_vaddr(struct cte *ptable, size_t entry, genvaddr_t *retvaddr);

#endif // PAGING_H
