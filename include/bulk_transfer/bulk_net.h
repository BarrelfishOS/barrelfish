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
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_NET_H
#define BULK_NET_H

#include <bulk_transfer/bulk_transfer.h>



/* TODO: correct inlude */
struct ip_addr  {
                int foo;
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
    uint64_t                        rx_queue_id; ///< rx queue (control channel)
    uint64_t                        tx_queue_id; ///< tx queue (control channel)
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
    uint64_t        rx_queue_id; ///< rx queue (control channel)
    uint64_t        tx_queue_id; ///< tx queue (control channel)
};


/**
 * enumeration of possible message types over the control channel
 */
enum bulk_net_msg_type {
    BULK_NET_MSG_BIND,          ///< binding to the channel
    BULK_NET_MSG_POOL_ASSIGN,   ///< pool has been assigned to the channel
    BULK_NET_MSG_POOL_REMOVE,   ///< pool has been removed from the channel
    BULK_NET_MSG_DATA           ///< data arrives over the channel
};

struct bulk_net_header {
    enum bulk_net_msg_type  type;
    size_t                  size;
};

struct bulk_net_msg {
    struct bulk_net_header header;
    /* meta data of the bulk data */
    union {
        struct {
            struct bulk_pool_id id;
            size_t buffer_size;
            size_t num_buffers;
        } pool_assign;

        struct {
            struct bulk_pool_id id;
        } pool_remove;

        struct {
            struct bulk_net_ep_setup ep_setup;
            struct bulk_channel_setup channel_setup;
        } bind;

        struct {


        } data;

    } msg;
};


/*
 * ---------------------------------------------------------------------------
 * Implementation Specific Interface Functions >>>
 *
 * TODO: make asynchronous
 */
errval_t bulk_net_channel_pass(struct bulk_channel *channel,
                               struct bulk_buffer  *buffer,
                               void                *meta,
                               struct bulk_continuation cont);

errval_t bulk_net_channel_copy(struct bulk_channel *channel,
                               struct bulk_buffer  *buffer,
                               void                *meta,
                               struct bulk_continuation cont);

errval_t bulk_net_channel_release(struct bulk_channel *channel,
                                  struct bulk_buffer  *buffer,
                                  struct bulk_continuation cont);

errval_t bulk_net_channel_move(struct bulk_channel *channel,
                               struct bulk_buffer   *buffer,
                               void                 *meta,
                               struct bulk_continuation cont);

errval_t bulk_net_channel_assign_pool(struct bulk_channel *channel,
                                      struct bulk_pool    *pool);

errval_t bulk_net_channel_bind(struct bulk_channel *channel);

errval_t bulk_net_channel_create(struct bulk_channel *channel);

struct bulk_implementation *bulk_net_get_implementation(void);

/*
 * <<< Implementation Specific Interface Functions
 * ---------------------------------------------------------------------------
 */

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
                                   struct ip_addr ip, uint16_t port);
#endif /* BULK_NET_H */
