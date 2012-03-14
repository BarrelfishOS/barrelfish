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
#include <barrelfish_kpi/distcaps.h>

struct distcap_info {
    coreid_t owner;
    bool locked, in_delete;
};

static inline distcap_state_t
distcap_get_state(struct distcap_info *dcap)
{
    distcap_state_t result = 0;
    if (dcap->owner != my_core_id) {
        result |= DISTCAP_STATE_FOREIGN;
    }
    if (dcap->locked || dcap->in_delete) {
        result |= DISTCAP_STATE_BUSY;
    }
    return result;
}

#endif
