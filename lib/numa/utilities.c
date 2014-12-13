/**
 * \file
 * \brief General Numa functions
 *
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <skb/skb.h>

#include <numa.h>

#include "numa_internal.h"

/**
 * \brief obtains the system topology from the SKB
 *
 * \param topology pointer to the topology information structure
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
errval_t numa_get_topology_from_skb(struct numa_topology *topology)
{
    errval_t err;

    /* don't query if no return pointer is specified */
    if (topology == NULL) {
        return SYS_ERR_OK;
    }


    NUMA_DEBUG_INIT("getting topology from SKB...\n");

    err = skb_client_connect();
    if (err_is_fail(err)) {
        return err_push(err, NUMA_ERR_SKB);
    }

    /* query SKB for topology information */

    /* allocate struct */




    assert(!"NYI");
    return SYS_ERR_OK;
}

