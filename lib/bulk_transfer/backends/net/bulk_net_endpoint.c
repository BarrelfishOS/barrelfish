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
#include <bulk_transfer/bulk_net.h>


/**
 * Creates a new bulk endpoint which uses the network backend
 *
 * @param ep_desc   memory location to create the endpoint in
 * @param setup     the setup parameters for the endpoint
 *
 * This function is intended to be used by the creator.
 */
errval_t bulk_net_ep_create(struct bulk_net_endpoint_descriptor *ep_desc,
                            struct bulk_net_ep_setup            *setup)
{
    assert(ep_desc);

    ep_desc->ip = setup->ip;
    ep_desc->port = setup->port;

    ep_desc->ep_generic.f = bulk_net_get_implementation();


    /*
     * XXX: Do we want to initialize the network queues and the
     *      tcp connection at this point ?
     *      Alternatively just prepare it and finalize it when the channel
     *      gets created,
     *      - RA
     */
    return SYS_ERR_OK;
}

/**
 * Destroys the given endpoint
 *
 * @param   ep_desc the endpoint to be destroyed
 */
errval_t bulk_net_ep_destroy(struct bulk_net_endpoint_descriptor *ep_desc)
{
    assert(ep_desc);

    /*
     * TODO: free up potential resources e.g. tcp, rx/tx queues etc
     *       only if resources are created upon endpoint create
     *       otherwise this is a no-op
     */


    return SYS_ERR_OK;
}

/**
 * Explicitly creates a specific remote endpoint
 *
 * @param ep_desc   memory location to create the endpoint
 * @param ip        the ip of the server machine
 * @param port      the port where the otherside listens to
 *
 * This is used to explicitly specify an endpoint to connect to. In the end,
 * a nameservice like lookup should return the correct endpoint descriptor.
 */
errval_t bulk_net_ep_create_remote(struct bulk_net_endpoint_descriptor *ep_desc,
                                   struct ip_addr ip, uint16_t port)
{
    assert(ep_desc);


    ep_desc->ip = ip;
    ep_desc->port = port;

    ep_desc->ep_generic.f = bulk_net_get_implementation();

    /*
     * this endpoint is used to specify the remote endpoint to bind to.
     * no need to create a listener for new connections
     *
     * potential receiving queues are created upon channel bind
     */

    return SYS_ERR_OK;
}
