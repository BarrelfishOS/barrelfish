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

#include "bulk_net_backend.h"

static char *default_cardname = "e10k";

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

    if (setup->port == 0 || setup->queue == 0) {
        return BULK_TRANSFER_INVALID_ARGUMENT;
    }

    ep_desc->ip.addr = htonl(setup->ip.addr);
    ep_desc->port = setup->port;
    ep_desc->queue = setup->queue;

    if (setup->cardname) {
        ep_desc->cardname = setup->cardname;
    } else {
        ep_desc->cardname = default_cardname;
    }

    if (setup->buffer_size == 0) {
        ep_desc->buffer_size = BULK_NET_DEFAULT_BUFFER_SIZE;
    } else {
        ep_desc->buffer_size = setup->buffer_size;
    }

    if (setup->buffer_count == 0) {
        ep_desc->buffer_count = BULK_NET_DEFAULT_BUFFER_COUNT;
    } else {
        ep_desc->buffer_count = setup->buffer_count;
    }

    if (setup->max_queues == 0) {
        ep_desc->max_queues = BULK_NET_DEFAULT_QUEUES;
    } else {
        ep_desc->max_queues = setup->max_queues;
    }


    if(setup->no_copy) {
        ep_desc->ep_generic.f = bulk_net_get_impl_no_copy();
    } else {
        ep_desc->ep_generic.f = bulk_net_get_impl();
    }

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
                                   struct bulk_net_ep_setup            *setup)
{
    assert(ep_desc);


    assert(ep_desc);

      if (setup->port == 0 || setup->queue == 0) {
          return BULK_TRANSFER_INVALID_ARGUMENT;
      }

      ep_desc->ip.addr = htonl(setup->ip.addr);
      ep_desc->port = setup->port;
      ep_desc->queue = setup->queue;

      if (setup->cardname) {
          ep_desc->cardname = setup->cardname;
      } else {
          ep_desc->cardname = default_cardname;
      }

      if (setup->buffer_size == 0) {
          ep_desc->buffer_size = BULK_NET_DEFAULT_BUFFER_SIZE;
      } else {
          ep_desc->buffer_size = setup->buffer_size;
      }

      if (setup->buffer_count == 0) {
          ep_desc->buffer_count = BULK_NET_DEFAULT_BUFFER_COUNT;
      } else {
          ep_desc->buffer_count = setup->buffer_count;
      }


      if(setup->no_copy) {
          ep_desc->ep_generic.f = bulk_net_get_impl_no_copy();
      } else {
          ep_desc->ep_generic.f = bulk_net_get_impl();
      }


    return SYS_ERR_OK;
}
