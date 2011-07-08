/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "../../vspace_internal.h"

/**
 * \brief Initialize the x86_32 layout
 */
errval_t vspace_layout_init(struct vspace_layout *l)
{
    l->offset = 0;
    l->granularity = BASE_PAGE_SIZE;
    // XXX: Copied size from kernel/offsets.h.
    //Layout is not used in the tree yet so does not matter yet.
    l->size = (genvaddr_t)1 << 31;
    return SYS_ERR_OK;
}
