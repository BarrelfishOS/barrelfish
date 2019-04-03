/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_NET_H
#define BULK_NET_H

#include <bulk_transfer/bulk_transfer.h>
#include <barrelfish/event_queue.h>

#include <netinet/in.h>

#include <dev/e10k_dev.h>
#include <lwip/ip_addr.h>

struct bulk_net_msgdesc;
struct e10k_binding;
struct e10k_queue;

struct stack_allocator {
    size_t size;
    size_t top;
    void **stack;
};

struct bulk_e10k {
    bool                    ready;
    size_t                  buffer_size;
    size_t                  ring_size;
    uint8_t                 qi;
    uint8_t                 int_core;
    uint8_t                 int_vector;
    struct e10k_binding    *binding;
    e10k_t                  d;
    struct e10k_queue      *q;
    uint64_t                mac;
    struct capref           rxframe;
    struct capref           txframe;
    struct capref           txhwbframe;
    void                  (*received)(struct bulk_e10k *,
                                      struct bulk_net_msgdesc *);
    void                  (*transmitted)(struct bulk_e10k *,
                                         void *);

    struct event_queue      event_queue;
    struct stack_allocator  rx_event_alloc;
    struct stack_allocator  tx_event_alloc;

    struct waitset          *waitset;
    struct waitset_chanstate wscs;

    void                   *opaque;
};



/**
 * endpoint descriptor for the bulk network interface.
 *
 * XXX: the queue identifiers are for the control channel.
 *      we may have to distinguish the different pools with different queues
 *      because the buffer sizes may vary among pools.
 *      - RA
 */
struct bulk_net_endpoint_descriptor {
    /* generic part */
    struct bulk_endpoint_descriptor ep_generic; ///< generic endpoint part
    /* start of implementation specific part */
    struct ip_addr                  ip;          ///< ip address
    uint16_t                        port;        ///< port
    /* from transparent endpoint */
    char                           *cardname;
    uint8_t                         queue;
    uint8_t                         max_queues;
    size_t                          buffer_size;
    size_t                          buffer_count;
    /*
     * XXX: do we want to add the connection information here as well?
     *      e.g. tcp_pcb ?
     */
};

/**
 * setup parameters for creating a network endpoint
 */
struct bulk_net_ep_setup {
    struct ip_addr  ip;         ///< the ip address of the endpoint
    uint16_t        port;       ///< the port of the endpoint
    /* possibly queue id */
    uint8_t         queue;
    uint8_t         max_queues;
    size_t          buffer_size;
    size_t          buffer_count;
    char           *cardname;
    bool            no_copy;
};


/**
 * Creates a new bulk endpoint which uses the network backend
 *
 * @param ep_desc   memory location to create the endpoint in
 * @param setup     the setup parameters for the endpoint
 *
 * This function is intended to be used by the creator.
 */
errval_t bulk_net_ep_create(struct bulk_net_endpoint_descriptor *ep_desc,
                            struct bulk_net_ep_setup            *setup);

/**
 * Destroys the given endpoint
 *
 * @param   ep_desc the endpoint to be destroyed
 */
errval_t bulk_net_ep_destroy(struct bulk_net_endpoint_descriptor *ep_desc);

/**
 * Explicitly creates a specific remote endpoint
 *
 * @param ep_desc   memory location to create the endpoint
 * @param ip        the ip of the server machine
 * @param port      the port where the otherside listens to
 */
errval_t bulk_net_ep_create_remote(struct bulk_net_endpoint_descriptor *ep_desc,
                                   struct bulk_net_ep_setup            *setup);
#endif /* BULK_NET_H */
