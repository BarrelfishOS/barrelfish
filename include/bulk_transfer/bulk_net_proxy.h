/**
 * \file
 * \brief Proxy for connecting bulk transfer channels over a network connection
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_NET_PROXY_H
#define BULK_NET_PROXY_H

#include <barrelfish/barrelfish.h>
#include <bulk_transfer/bulk_transfer.h>
#include <bulk_transfer/bulk_net.h>

#include <dev/e10k_dev.h>


/* Internal stuff, only here to avoid unnecessary mallocs */
struct transmit_buffer;
struct receive_buffer;


/** Proxy handle struct */
struct bulk_net_proxy {
    struct waitset *ws;
    struct bulk_channel channel;
    size_t buffer_size;

    errval_t err;
    bool bulk_bound;
    bool net_bound;
    void (*connected)(struct bulk_net_proxy *);

    struct bulk_e10k transfer;

    struct stack_allocator rb_stack;
    struct transmit_buffer *tb;
    struct stack_allocator tb_stack;

    struct bulk_continuation panic_cont;
    void *zero_meta;

    // Coordinates for network connection
    const char *card;
    uint8_t queue;
    uint16_t l_port;
    uint16_t r_port;
    uint32_t l_ip;
    uint32_t r_ip;
    uint64_t l_mac;
    uint64_t r_mac;

    void *user_state;
};

/**
 * Start listening proxy on the specified port. Note that the proxy will bind to
 * the bulk channel during the initialization phase, before actually receiving a
 * connection request. But transfers on the channel (including the passing of
 * buffers for the receive queue) must only be executed after the connected
 * callback is called.
 *
 * @param p           Proxy struct
 * @param desc        Bulk endpoint to bind to
 * @param ws          Waitset
 * @param buffer_size Size of buffers to be transmitted over this proxy
 * @param card        NIC name in barrelfish
 * @param queue       Queue ID to use
 * @param port        Port number to listen on (host byte order)
 * @param connected   Callback, will be invoked once an incoming connection has
 *                    been established.
 */
errval_t bulk_net_proxy_listen(struct bulk_net_proxy           *p,
                               struct bulk_endpoint_descriptor *desc,
                               struct waitset                  *ws,
                               size_t                           buffer_size,
                               const char                      *card,
                               uint8_t                          queue,
                               uint16_t                         port,
                               void (*connected)(struct bulk_net_proxy *));

/**
 * Connect to listening proxy. Note that the proxy will bind to the bulk channel
 * during the initialization phase, before actually receiving a connection
 * request. But transfers on the channel (including the passing of buffers for
 * the receive queue) must only be executed after the connected callback is
 * called.
 * @param p           Proxy struct
 * @param desc        Bulk endpoint to bind to
 * @param ws          Waitset
 * @param buffer_size Size of buffers to be transmitted over this proxy
 * @param card        NIC name in barrelfish
 * @param queue       Queue ID to use
 * @param ip          IP to connect to (host byte order)
 * @param port        Port number to connect to (host byte order)
 * @param connected   Callback, will be invoked once the connection has
 *                    been established.
*
 */
errval_t bulk_net_proxy_connect(struct bulk_net_proxy           *p,
                                struct bulk_endpoint_descriptor *desc,
                                struct waitset                  *ws,
                                size_t                           buffer_size,
                                const char                      *card,
                                uint8_t                          queue,
                                uint32_t                         ip,
                                uint16_t                         port,
                                void (*connected)(struct bulk_net_proxy *));

#endif /* BULK_NET_H */
