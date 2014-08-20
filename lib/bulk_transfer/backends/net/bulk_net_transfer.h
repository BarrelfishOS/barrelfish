/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BULK_NET_TRANSFER_H
#define BULK_NET_TRANSFER_H

#include <barrelfish/barrelfish.h>
#include <bulk_transfer/bulk_transfer.h>
#include <ipv4/lwip/inet.h>

#include "bulk_net_backend.h"
#include "bulk_net_e10k.h"



struct bulk_net_control
{
    struct bulk_e10k transfer;

    size_t buffer_size;
    size_t buffer_count;

    struct waitset *ws;

    struct stack_allocator rb_stack;
    struct transmit_buffer *tb;
    struct stack_allocator tb_stack;

    const char *card;
    uint8_t queue;
    uint8_t max_queues;
    uint8_t num_queues;
    uint16_t l_port;
    uint16_t r_port;
    uint32_t l_ip;
    uint32_t r_ip;
    uint64_t l_mac;
    uint64_t r_mac;
};

errval_t bulk_net_transfer_bind(struct bulk_net_control *tc,
                                void (*tx_cb)(struct bulk_e10k *bu,
                                              void *opaque),
                                void (*rx_cb)(struct bulk_e10k* bu,
                                              struct bulk_net_msgdesc *msg));

errval_t bulk_net_transfer_export(struct bulk_net_control *tc,
                                  void (*tx_cb)(struct bulk_e10k *bu,
                                                void *opaque),
                                  void (*rx_cb)(struct bulk_e10k* bu,
                                                struct bulk_net_msgdesc *msg));

errval_t bulk_net_transfer_init(struct bulk_net_control *tc,
                                void (*tx_cb)(struct bulk_e10k *bu,
                                              void *opaque),
                                void (*rx_cb)(struct bulk_e10k* bu,
                                              struct bulk_net_msgdesc *msg));

void bulk_net_transfer_strip_padding(struct bulk_net_msgdesc *msg);

void bulk_net_transfer_add_header(struct bulk_net_msgdesc *msg);

void bulk_net_transfer_update_tx_headers(struct bulk_net_control *p);

void bulk_net_transfer_free_rb(struct bulk_net_control *tc,
                               struct receive_buffer *rb);

void bulk_net_transfer_free_rx(struct bulk_net_control *tc,
                               struct bulk_net_msgdesc *msg);

#endif /* BULK_NET_TRANSFER_H */
