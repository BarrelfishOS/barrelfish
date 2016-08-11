/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "../../vspace_internal.h"

/**
 * \brief Initialize the AARCH64 layout.
 */
errval_t vspace_layout_init(struct vspace_layout *l)
{
    l->offset      = 0;
    l->granularity = BASE_PAGE_SIZE;
    l->size = (genvaddr_t) VMSAv8_64_PTABLE_SIZE * VMSAv8_64_PTABLE_SIZE *
            VMSAv8_64_PTABLE_SIZE * VMSAv8_64_PTABLE_SIZE * BASE_PAGE_SIZE;
    return SYS_ERR_OK;
}
