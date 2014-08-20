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


errval_t bulk_net_channel_create(struct bulk_channel *channel)
{
    assert(!"NYI: bulk_net_channel_create");

    /*
     * TODO:
     * - initialize the channel struct
     * - initialize the network queues
     *
     * bool lwip_init(const char *card_name, uint64_t queueid)
     * -> does this has to be done once per domain or for every channel?
     *
     *     struct tcp_pcb *pcb = tcp_new();
     * //    err_t e = tcp_bind(pcb, IP_ADDR_ANY, (HTTP_PORT + disp_get_core_id()));
     *     err_t e = tcp_bind(pcb, IP_ADDR_ANY, HTTP_PORT);
         * assert(e == ERR_OK);
         * pcb = tcp_listen(pcb);
         * assert(pcb != NULL);
         * tcp_arg(pcb, pcb);
         * tcp_accept(pcb, http_server_accept);
     * - start listening on the queue
     */
    return SYS_ERR_OK;
}

errval_t bulk_net_channel_bind(struct bulk_channel *channel)
{
    assert(!"NYI: bulk_net_channel_bind");

    /*
     * TODO:
     * - initialize the channel struct
     * - initialize local queues
     * struct tcp_pcb *pcb = tcp_new();
     * tcp_connect();
     * tcp_write...
     * - connect to the network server
     */

    return SYS_ERR_OK;
}

errval_t bulk_net_channel_assign_pool(struct bulk_channel *channel,
                                      struct bulk_pool    *pool)
{
    assert(!"NYI: bulk_net_channel_assign_pool");

    /*
     * TODO:
     *  - send control message with pool information to the other side
     */

    return SYS_ERR_OK;
}

errval_t bulk_net_channel_move(struct bulk_channel      *channel,
                               struct bulk_buffer       *buffer,
                               void                     *meta,
                               struct bulk_continuation  cont)
{
    assert(!"NYI: bulk_net_channel_move");



    /**
     *
     * TODO:
     *  - prepend meta data / header
     *  - enqueue buffer on send queue
     *  - register sent callback
     *  - if (owner then set to read/write))
     *  - free up buffer and hand it back to the pool
     */

    return SYS_ERR_OK;
}

/**
 *
 */
errval_t bulk_net_channel_pass(struct bulk_channel *channel,
                               struct bulk_buffer  *buffer,
                               void                *meta,
                               struct bulk_continuation cont)
{
    /* this is a no-op over the network hop,
     * just do a buffer_release at this point */
    return bulk_net_channel_release(channel, buffer, cont);
}


errval_t bulk_net_channel_copy(struct bulk_channel *channel,
                               struct bulk_buffer  *buffer,
                               void                *meta,
                               struct bulk_continuation cont)
{
    assert(!"NYI: bulk_net_channel_copy");

    /*
     * TODO:
     *  - prepend meta data / header
     *  - enqueue buffer on send queue
     *  - register sent callback
     *  - if (owner then set to read/write))
     */

    return SYS_ERR_OK;
}

errval_t bulk_net_channel_release(struct bulk_channel *channel,
                          struct bulk_buffer  *buffer,
                          struct bulk_continuation cont)
{
    /* this is a no-op over the network hop */
    return SYS_ERR_OK;
}

struct bulk_implementation bulk_net_implementation = {
               .channel_create = bulk_net_channel_create,
               .channel_bind   = bulk_net_channel_bind,
               .assign_pool    = bulk_net_channel_assign_pool,
               .move           = bulk_net_channel_move,
               .pass           = bulk_net_channel_pass,
               .copy           = bulk_net_channel_copy,
               .release        = bulk_net_channel_release
};

struct bulk_implementation *bulk_net_get_implementation(void) {
    return &bulk_net_implementation;
}
