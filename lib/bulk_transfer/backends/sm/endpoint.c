/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_sm.h>

errval_t bulk_sm_ep_create(struct bulk_sm_endpoint_descriptor *ep_desc)
{
    assert(ep_desc);

    // We cannot export the service yet and bind the endpoint's iref.
    // The iref gets bound to a waitset which is only provided upon
    // channel_create/_bind.

    ep_desc->ep_generic.f = bulk_sm_get_implementation();
    ep_desc->state        = BULK_EPSTATE_CREATED;

    return SYS_ERR_OK;
}

/**
 * Creates a new bulk endpoint which uses the shared memory backend
 *
 * @param ep_desc       memory location to create the endpoint in
 * @param remote_iref   the iref of the exported service on the other side
 *
 * This function is intended to be used by the binding side
 */
errval_t bulk_sm_ep_create_remote(struct bulk_sm_endpoint_descriptor *ep_desc,
                                  iref_t remote_iref)
{
    ep_desc->ep_generic.f = bulk_sm_get_implementation();
    ep_desc->state        = BULK_EPSTATE_IREF_EXPORTED;
    // XXX How to ensure peer exported if on given iref? otherwise, should
    // refuse to create ep.

    ep_desc->iref = remote_iref;
    return SYS_ERR_OK;
}
