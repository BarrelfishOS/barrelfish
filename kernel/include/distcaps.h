/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DISTCAPS_H
#define DISTCAPS_H

#include <kernel.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/distcaps.h>
#include <mdb/mdb.h>
#include <capabilities.h>

struct cte;

static inline distcap_state_t
distcap_get_state(struct cte *dcap)
{
    distcap_state_t result = 0;
    if (dcap->mdbnode.owner != my_core_id) {
        result |= DISTCAP_STATE_FOREIGN;
    }
    if (dcap->mdbnode.locked || dcap->mdbnode.in_delete) {
        result |= DISTCAP_STATE_BUSY;
    }
    return result;
}

static inline bool
distcap_is_foreign(struct cte *dcap)
{
    return dcap->mdbnode.owner != my_core_id;
}

static inline bool
distcap_is_in_delete(struct cte *dcap)
{
    return dcap->mdbnode.in_delete;
}

static inline void
distcap_set_deleted(struct cte *dcap)
{
    dcap->mdbnode.in_delete = true;
}

#endif
