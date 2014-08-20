/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <sys/param.h>

#include <barrelfish/barrelfish.h>
#include <bulk_transfer/bulk_transfer.h>
#include <ipv4/lwip/inet.h>

#include "bulk_net_backend.h"
#include "bulk_net_transfer.h"

#if BULK_NET_ENABLE_DEBUG_TRANSF
#define BT_DEBUG_TRACE BULK_NET_TRACE
#define BT_DEBUG(fmt, msg...) BULK_NET_DEBUG(fmt, msg)
#else
#define BT_DEBUG(fmt, msg...) do{}while(0);
#define BT_DEBUG_TRACE do{}while(0);
#endif

#if BULK_NET_ENABLE_STATUS_TRANSF
#define BT_STATUS(fmt, msg...) BULK_NET_STATUS(fmt, msg)
#else
#define BT_STATUS(fmt, msg...) do{} while(0);
#endif

errval_t bulk_net_transfer_bind(struct bulk_net_control *tc,
                                void (*tx_cb)(struct bulk_e10k *bu,
                                              void *opaque),
                                void (*rx_cb)(struct bulk_e10k* bu,
                                              struct bulk_net_msgdesc *msg))
{
    errval_t err;

    err = bulk_net_transfer_init(tc, tx_cb, rx_cb);
    if (err_is_fail(err)) {
        return err;
    }

    err = bulk_e10k_arp_lookup(&tc->transfer, tc->r_ip, &tc->r_mac);
    if (err_is_fail(err)) {
        return err;
    }

    err = bulk_e10k_port_alloc(&tc->transfer, &tc->l_port);
    if (err_is_fail(err)) {
        return err;
    }

    bulk_net_transfer_update_tx_headers(tc);
    return err;
}

errval_t bulk_net_transfer_export(struct bulk_net_control *tc,
                                  void (*tx_cb)(struct bulk_e10k *bu,
                                                void *opaque),
                                  void (*rx_cb)(struct bulk_e10k* bu,
                                                struct bulk_net_msgdesc *msg))
{
    errval_t err;

    err = bulk_net_transfer_init(tc, tx_cb, rx_cb);
    if (err_is_fail(err)) {
        return err;
    }

    err = bulk_e10k_port_add(&tc->transfer, tc->l_port);
    if (err_is_fail(err)) {
        return err;
    }

    return err;
}

errval_t bulk_net_transfer_init(struct bulk_net_control *tc,
                                void (*tx_cb)(struct bulk_e10k *bu,
                                              void *opaque),
                                void (*rx_cb)(struct bulk_e10k* bu,
                                              struct bulk_net_msgdesc *msg))
{
    errval_t err;
    size_t i;
    size_t n = BULK_NET_TRANSFER_NUM_DESCS - 1;
    struct receive_buffer *rb;
    struct transmit_buffer *tb;
    void *h_vbase, *i_vbase;
    uintptr_t h_pbase, i_pbase;

    tc->transfer.opaque = tc;

    err = bulk_e10k_init(&tc->transfer, tc->ws, tc->card, tc->queue,
                         tc->buffer_size,
                         BULK_NET_TRANSFER_NUM_DESCS,
                         rx_cb, tx_cb);

    stack_alloc_init(&tc->rb_stack, n);
    stack_alloc_init(&tc->tb_stack, n);
    rb = calloc(n, sizeof(*rb));
    tc->tb = tb = calloc(n, sizeof(*tb));

    err = allocmap_frame(E10K_HDRSZ * n * 2, &h_vbase, &h_pbase, NULL);
    assert(err_is_ok(err));
    err = allocmap_frame(BULK_NET_INTERNAL_BUFER_SIZE * n, &i_vbase, &i_pbase, NULL);
    assert(err_is_ok(err));

    for (i = 0; i < n; i++) {
        rb[i].hdr_virt = h_vbase;
        rb[i].hdr_phys = h_pbase;
        h_pbase += E10K_HDRSZ;
        h_vbase = (void *) ((uintptr_t) h_vbase + E10K_HDRSZ);

        tb[i].hdr_virt = h_vbase;
        tb[i].hdr_phys = h_pbase;
        tb[i].int_virt = i_vbase;
        tb[i].int_phys = i_pbase;
        h_pbase += E10K_HDRSZ;
        h_vbase = (void *) ((uintptr_t) h_vbase + E10K_HDRSZ);
        i_pbase += BULK_NET_INTERNAL_BUFER_SIZE;
        i_vbase = (void *) ((uintptr_t) i_vbase + BULK_NET_INTERNAL_BUFER_SIZE);

        stack_alloc_free(&tc->rb_stack, rb + i);
        stack_alloc_free(&tc->tb_stack, tb + i);
    }

    for (uint32_t j=0; j < tc->buffer_count; ++j) {
        rb = stack_alloc_alloc(&tc->rb_stack);
        rb->buffer = NULL;
        err = allocmap_frame(tc->buffer_size, &rb->virt, &rb->phys, NULL);
        assert(err_is_ok(err));

        err = bulk_e10k_rx_add(&tc->transfer, rb->phys, rb->hdr_phys, rb);
        assert(err_is_ok(err));
    }
    tc->l_mac = tc->transfer.mac;
    bulk_e10k_ip_info(&tc->transfer, &tc->l_ip);
    return err;
}

void bulk_net_transfer_update_tx_headers(struct bulk_net_control *p)
{
    size_t i;
    struct packet_header *hdr;

    BT_DEBUG("Updating TX headers %"PRIx64"  %"PRIx64"   frst=%"PRIx64"\n",
                 p->r_mac, p->l_mac,
                 p->tb[BULK_NET_TRANSFER_NUM_DESCS - 2].hdr_phys);

    for (i = 0; i < BULK_NET_TRANSFER_NUM_DESCS - 1; i++) {
        hdr = p->tb[i].hdr_virt;
        memset(hdr, 0, sizeof(*hdr));
        memcpy(hdr->l2.dmac, &p->r_mac, 6);
        memcpy(hdr->l2.smac, &p->l_mac, 6);
        hdr->l2.type = htons(0x0800);

        hdr->l3.ver_ihl = 5 | (4 << 4);
        hdr->l3.ttl = 64;
        hdr->l3.proto = 0x11;
        hdr->l3.s_ip = htonl(p->l_ip);
        hdr->l3.d_ip = htonl(p->r_ip);

        hdr->l4.s_port = htons(p->l_port);
        hdr->l4.d_port = htons(p->r_port);
    }
}

void bulk_net_transfer_add_header(struct bulk_net_msgdesc *msg)
{
    struct transmit_buffer *tb = msg->parts[1].opaque;
    struct packet_header *h = tb->hdr_virt;
    size_t i;
    size_t len = 0;

    for (i = 1; i < BULK_NET_DESCLEN && msg->parts[i].size != 0; i++) {
        len += msg->parts[i].size;
    }

    msg->parts[0].phys = tb->hdr_phys;
    msg->parts[0].size = sizeof(*h);
    msg->parts[0].opaque = NULL;

    h->l4.len = htons(len + 8);
    h->l3.len = htons(len + 8 + 20);
}

void bulk_net_transfer_strip_padding(struct bulk_net_msgdesc *msg)
{
    struct receive_buffer *rb = msg->parts[0].opaque;
    struct packet_header *h = rb->hdr_virt;
    size_t len = ntohs(h->l4.len) - 8;
    size_t i;

    for (i = 1; i < BULK_NET_DESCLEN && msg->parts[i].size != 0; i++) {
        msg->parts[i].size = MIN(msg->parts[i].size, len);
        len -= msg->parts[i].size;
    }
}

void bulk_net_transfer_free_rb(struct bulk_net_control *tc,
                               struct receive_buffer *rb)
{
    if (rb->buffer == NULL) {
        // Temporary initialization buffer -> do not reenqueue after
        // initialization is done
        if (false) {
            // TODO: free, currently leaking here
            stack_alloc_free(&tc->rb_stack, rb);
            return;
        }
    }

    bulk_e10k_rx_add(&tc->transfer, rb->phys, rb->hdr_phys, rb);
}

void bulk_net_transfer_free_rx(struct bulk_net_control *tc,
                               struct bulk_net_msgdesc *msg)
{
    size_t i;

    for (i = 1; i < BULK_NET_DESCLEN && msg->parts[i].size != 0; i++) {
        bulk_net_transfer_free_rb(tc, msg->parts[i].opaque);
    }
}
