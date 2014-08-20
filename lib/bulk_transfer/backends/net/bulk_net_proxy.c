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

#define L234HEADER_SIZE (14 + 20 + 16)
#define NDESCS 1024

//#define DEBUG(x...) debug_printf(x)
#define DEBUG(x...) do {} while (0)


/******************************************************************************/
/* Protocol used on the wire */

/** Message types */
enum proto_msg {
    PROTO_INVALID = 0,
    PROTO_BIND_REQUEST,
    PROTO_BIND_RESPONSE,
    PROTO_DATA_TRANSFER,
};

/** Bind request */
struct proto_trail_bind_req {
    uint32_t buffer_size;
    uint32_t meta_size;

    uint8_t type;
} __attribute__((packed));

/** Bind response */
struct proto_trail_bind_resp {
    errval_t err;

    uint8_t type;
} __attribute__((packed));

/** Data transfer */
struct proto_trail_data_transfer {
    uint8_t type;
} __attribute__((packed));



/*errval_t (*bind_received)(struct bulk_channel *channel);
void (*teardown_received)(struct bulk_channel *channel);
errval_t (*pool_removed)(struct bulk_channel *channel,
                         struct bulk_pool *pool);
void (*copy_released)(struct bulk_channel *channel,
                      struct bulk_buffer  *buffer);*/

static errval_t cb_pool_assigned(struct bulk_channel *channel,
                                 struct bulk_pool *pool);
static void cb_move_received(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             void                *meta);
static void cb_buffer_received(struct bulk_channel *channel,
                               struct bulk_buffer  *buffer,
                               void                *meta);
static void cb_copy_received(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             void                *meta);

static struct bulk_channel_callbacks callbacks = {
    .pool_assigned = cb_pool_assigned,
    .move_received = cb_move_received,
    .buffer_received = cb_buffer_received,
    .copy_received = cb_copy_received,
};


/** Adapt MAC/IP/Port combinations in all transmit buffers (header part) */
static void update_tx_headers(struct bulk_net_proxy *p)
{
    size_t i;
    struct packet_header *hdr;
    DEBUG("Updating TX headers %"PRIx64"  %"PRIx64"   frst=%"PRIx64"\n",
            p->r_mac, p->l_mac, p->tb[NDESCS-2].hdr_phys);
    for (i = 0; i < NDESCS - 1; i++) {
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

static void add_header(struct bulk_net_msgdesc *msg)
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

static void strip_padding(struct bulk_net_msgdesc *msg)
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

static void dump_rx_msg(struct bulk_net_msgdesc *msg)
{
    size_t i, j;
    uint8_t *data;
    uintptr_t phys;
    struct receive_buffer *rb;

#if !DO_MSG_DUMP
    return;
#endif

    DEBUG("dump_rx_msg():\n");
    for (i = 0; i < BULK_NET_DESCLEN && msg->parts[i].size != 0; i++) {
        rb = msg->parts[i].opaque;
        DEBUG("  parts[%"PRId64"]: size=%"PRIx64" op=%p ",
                i, msg->parts[i].size, rb);
        if (i == 0) {
            data = rb->hdr_virt;
            phys = rb->hdr_phys;
        } else {
            data = rb->virt;
            phys = rb->phys;
        }
        printf(" phys=%"PRIx64" virt=%p  ", phys, data);
        for (j = 0; j < msg->parts[i].size; j++) {
            printf("%02"PRIx8" ", data[j]);
        }
        printf("\n");
    }
}

static void dump_tx_msg(struct bulk_net_msgdesc *msg)
{
#if !DO_MSG_DUMP
    return;
#endif
    size_t i, j;
    uint8_t *data;
    struct transmit_buffer *tb;

    DEBUG("dump_tx_msg():\n");
    for (i = 0; i < BULK_NET_DESCLEN && msg->parts[i].size != 0; i++) {
        DEBUG("  parts[%"PRId64"]: size=%"PRIx64"  ", i,
                msg->parts[i].size);
        tb = msg->parts[i].opaque;
        if (i == 0) {
            tb = msg->parts[1].opaque;
            data = tb->hdr_virt;
        } else if (tb->buffer == NULL) {
            data = tb->int_virt;
        } else {
            data = tb->buffer->address;
        }
        for (j = 0; j < msg->parts[i].size; j++) {
            printf("%02"PRIx8" ", data[j]);
        }
        printf("\n");
    }
}

/******************************************************************************/
/* Sending messages to other end */

/** Send out bind request */
static void send_bind_request(struct bulk_net_proxy *p,
                              size_t buffer_size,
                              size_t meta_size)
{
    errval_t err;
    struct proto_trail_bind_req *t;
    struct transmit_buffer *tb;
    struct bulk_net_msgdesc msg;

    tb = stack_alloc_alloc(&p->tb_stack);
    assert(tb != NULL);

    t = tb->int_virt;
    t->buffer_size = buffer_size;
    t->meta_size = meta_size;
    t->type = PROTO_BIND_REQUEST;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t);
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    add_header(&msg);
    err = bulk_e10k_send(&p->transfer, &msg);
    assert(err_is_ok(err));
}

/** Send out bind response */
static void send_bind_response(struct bulk_net_proxy *p,
                               errval_t err)
{
    struct proto_trail_bind_resp *t;
    struct transmit_buffer *tb;
    struct bulk_net_msgdesc msg;

    tb = stack_alloc_alloc(&p->tb_stack);
    assert(tb != NULL);

    t = tb->int_virt;
    t->err = err;
    t->type = PROTO_BIND_RESPONSE;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t);
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    add_header(&msg);
    err = bulk_e10k_send(&p->transfer, &msg);
    assert(err_is_ok(err));
}

/** Send data transfer */
static void send_data_transfer(struct bulk_net_proxy *p,
                               struct bulk_buffer *b,
                               void *meta,
                               bool is_copy)
{
    DEBUG("send_data_transfer()\n");
    errval_t err;
    struct proto_trail_data_transfer *t;
    struct transmit_buffer *tb_d, *tb;
    struct bulk_net_msgdesc msg;

    tb_d = stack_alloc_alloc(&p->tb_stack);
    assert(tb_d != NULL);
    tb_d->buffer = b;
    tb_d->is_copy = is_copy;

    // prepare trailer
    tb = stack_alloc_alloc(&p->tb_stack);
    assert(tb != NULL);

    memcpy(tb->int_virt, meta, p->channel.meta_size);
    t = (void *) ((uint8_t *) tb->int_virt + p->channel.meta_size);
    t->type = PROTO_DATA_TRANSFER;

    msg.parts[1].phys = b->phys;
    msg.parts[1].size = p->buffer_size;
    msg.parts[1].opaque = tb_d;
    msg.parts[2].phys = tb->int_phys;
    msg.parts[2].size = sizeof(*t) + p->channel.meta_size;
    msg.parts[2].opaque = tb;
    msg.parts[3].size = 0;

    add_header(&msg);
    dump_tx_msg(&msg);
    err = bulk_e10k_send(&p->transfer, &msg);
    assert(err_is_ok(err));
    DEBUG("sent_data_transfer()\n");
}


/******************************************************************************/
/* Receiving messages from other end */

static void free_rb(struct bulk_net_proxy *p,
                    struct receive_buffer *rb)
{
    if (rb->buffer == NULL) {
        // Temporary initialization buffer -> do not reenqueue after
        // initialization is done
        if (p->net_bound) {
            // TODO: free, currently leaking here
            stack_alloc_free(&p->rb_stack, rb);
            return;
        }
    }

    bulk_e10k_rx_add(&p->transfer, rb->phys, rb->hdr_phys, rb);
}

static void free_rx(struct bulk_net_proxy *p,
                    struct bulk_net_msgdesc *msg)
{
    size_t i;

    for (i = 1; i < BULK_NET_DESCLEN && msg->parts[i].size != 0; i++) {
        free_rb(p, msg->parts[i].opaque);
    }
}

/** Handle received bind request */
static void bind_req_received(struct bulk_net_proxy *p,
                              struct proto_trail_bind_req *t,
                              struct bulk_net_msgdesc     *msg)
{
    struct receive_buffer *rb = msg->parts[0].opaque;
    struct packet_header *hdr = rb->hdr_virt;

    if (p->net_bound) {
        DEBUG("Ignoring bind request to already bound proxy\n");
        goto free;
    }

    p->r_mac = 0;
    memcpy(&p->r_mac, hdr->l2.smac, 6);
    p->r_ip = ntohl(hdr->l3.s_ip);
    p->r_port = ntohs(hdr->l4.s_port);

    update_tx_headers(p);

    assert(t->buffer_size == p->buffer_size);
    send_bind_response(p, SYS_ERR_OK);
    p->net_bound = true;
    p->connected(p);

free:
    free_rx(p, msg);
}

/** Handle received bind response */
static void bind_resp_received(struct bulk_net_proxy *p,
                               struct proto_trail_bind_resp *t,
                               struct bulk_net_msgdesc *msg)
{
    if (p->net_bound) {
        DEBUG("Ignoring bind response to already bound proxy\n");
        goto free;
    }

    if (err_is_ok(t->err)) {
        p->net_bound = true;
        p->connected(p);
    } else {
        USER_PANIC("Remote bind attempt failed\n");
    }

free:
    free_rx(p, msg);
}

/** Handle received data transfer */
static void data_transfer_received(struct bulk_net_proxy *p,
                                   struct proto_trail_data_transfer *t,
                                   struct bulk_net_msgdesc *msg)
{
    errval_t err;
    struct receive_buffer *rb;
    struct bulk_buffer *buffer;

    assert(msg->parts[1].size == p->buffer_size);
    // TODO: assumes that meta_size has a reasonably small size
    assert(msg->parts[2].size == p->channel.meta_size + sizeof(*t));
    assert(msg->parts[3].size == 0);

    rb = msg->parts[1].opaque;
    buffer = rb->buffer;
    stack_alloc_free(&p->rb_stack, rb);

    rb = msg->parts[2].opaque;

    err = bulk_channel_move(&p->channel, buffer, rb->virt, p->panic_cont);
    assert(err_is_ok(err));

    free_rb(p, rb);
}

static void tcb_received(struct bulk_e10k* bu, struct bulk_net_msgdesc *msg)
{
    struct bulk_net_proxy *p = bu->opaque;
    size_t i;
    struct receive_buffer *rb;
    uint8_t *t;
    DEBUG("tcb_received()\n");

    assert(msg->parts[0].size == sizeof(struct packet_header));
    dump_rx_msg(msg);
    strip_padding(msg);
    dump_rx_msg(msg);

    for (i = 0; i < BULK_NET_DESCLEN && msg->parts[i].size != 0; i++);
    i--;

    rb = msg->parts[i].opaque;
    t = rb->virt;
    switch (t[msg->parts[i].size - 1]) {
        case PROTO_BIND_REQUEST:
            DEBUG("Received bind request\n");
            bind_req_received(p, (struct proto_trail_bind_req *) (t +
                    msg->parts[i].size - sizeof(struct proto_trail_bind_req)),
                    msg);
            break;

        case PROTO_BIND_RESPONSE:
            DEBUG("Received bind response\n");
            bind_resp_received(p, (struct proto_trail_bind_resp *) (t +
                    msg->parts[i].size - sizeof(struct proto_trail_bind_resp)),
                    msg);
            break;

        case PROTO_DATA_TRANSFER:
            DEBUG("Received data transfer\n");
            data_transfer_received(p, (struct proto_trail_data_transfer *) (t +
                    msg->parts[i].size -
                    sizeof(struct proto_trail_data_transfer)), msg);
            break;

        default:
            USER_PANIC("Unexpected message type received\n");
    }
}


/******************************************************************************/
/* Management of network channel */

static void tcb_transmitted(struct bulk_e10k *bu, void *opaque)
{
    struct bulk_net_proxy *p = bu->opaque;
    struct transmit_buffer *tb = opaque;
    errval_t err;
    DEBUG("tcb_transmitted()\n");

    if (opaque == NULL) {
        // We can ignore the header buffers
        return;
    }

    // If there is a bulk buffer attached, need to pass it back
    if (tb->buffer != NULL) {
        if (tb->is_copy) {
            err = bulk_channel_release(&p->channel, tb->buffer, p->panic_cont);
        } else {
            err = bulk_channel_pass(&p->channel, tb->buffer, p->zero_meta,
                                    p->panic_cont);
        }
        assert(err_is_ok(err));
        tb->buffer = NULL;
    }
    stack_alloc_free(&p->tb_stack, tb);
}

static errval_t t_init(struct bulk_net_proxy *p)
{
    errval_t err;
    size_t i;
    size_t n = NDESCS - 1;
    struct receive_buffer *rb;
    struct transmit_buffer *tb;
    void *h_vbase, *i_vbase;
    uintptr_t h_pbase, i_pbase;

    p->net_bound = false;
    p->transfer.opaque = p;

    err = bulk_e10k_init(&p->transfer, p->ws, p->card, p->queue, p->buffer_size,
                         NDESCS, tcb_received, tcb_transmitted);

    stack_alloc_init(&p->rb_stack, n);
    stack_alloc_init(&p->tb_stack, n);
    rb = calloc(n, sizeof(*rb));
    p->tb = tb = calloc(n, sizeof(*tb));

    err = allocmap_frame(E10K_HDRSZ * n * 2, &h_vbase, &h_pbase, NULL);
    assert(err_is_ok(err));
    err = allocmap_frame(INT_BUFSZ * n, &i_vbase, &i_pbase, NULL);
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
        i_pbase += INT_BUFSZ;
        i_vbase = (void *) ((uintptr_t) i_vbase + INT_BUFSZ);

        stack_alloc_free(&p->rb_stack, rb + i);
        stack_alloc_free(&p->tb_stack, tb + i);
    }

    rb = stack_alloc_alloc(&p->rb_stack);
    rb->buffer = NULL;
    err = allocmap_frame(p->buffer_size, &rb->virt, &rb->phys, NULL);
    assert(err_is_ok(err));

    err = bulk_e10k_rx_add(&p->transfer, rb->phys, rb->hdr_phys, rb);
    assert(err_is_ok(err));

    p->l_mac = p->transfer.mac;
    bulk_e10k_ip_info(&p->transfer, &p->l_ip);
    return err;
}

static errval_t t_export(struct bulk_net_proxy *p)
{
    errval_t err;

    err = t_init(p);
    if (err_is_fail(err)) {
        return err;
    }

    err = bulk_e10k_port_add(&p->transfer, p->l_port);
    if (err_is_fail(err)) {
        return err;
    }

    return err;
}

static errval_t t_bind(struct bulk_net_proxy *p)
{
    errval_t err;

    err = t_init(p);
    if (err_is_fail(err)) {
        return err;
    }

    err = bulk_e10k_arp_lookup(&p->transfer, p->r_ip, &p->r_mac);
    if (err_is_fail(err)) {
        return err;
    }

    err = bulk_e10k_port_alloc(&p->transfer, &p->l_port);
    if (err_is_fail(err)) {
        return err;
    }

    update_tx_headers(p);
    return err;
}

/******************************************************************************/
/* Bulk transfer callbacks */

static errval_t cb_pool_assigned(struct bulk_channel *channel,
                                 struct bulk_pool *pool)
{
    struct bulk_net_proxy *p = channel->user_state;
    assert(pool->buffer_size == p->buffer_size);
    return SYS_ERR_OK;
}

static void cb_move_received(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             void                *meta)
{
    DEBUG("cb_move_received()\n");
    struct bulk_net_proxy *p = channel->user_state;
    assert(p->bulk_bound && p->net_bound);
    send_data_transfer(p, buffer, meta, false);
}

static void cb_buffer_received(struct bulk_channel *channel,
                               struct bulk_buffer  *buffer,
                               void                *meta)
{
    DEBUG("cb_buffer_received(b=%p,b->p=%"PRIx64")\n", buffer,
            buffer->phys);
    errval_t err;
    struct bulk_net_proxy *p = channel->user_state;
    struct receive_buffer *rb;
    assert(p->bulk_bound && p->net_bound);

    rb = stack_alloc_alloc(&p->rb_stack);
    assert(rb != NULL);

    rb->virt = buffer->address;
    rb->phys = buffer->phys;
    rb->buffer = buffer;

    err = bulk_e10k_rx_add(&p->transfer, rb->phys, rb->hdr_phys, rb);
    assert(err_is_ok(err));
    DEBUG("added buffer to rx queue\n");
}

static void cb_copy_received(struct bulk_channel *channel,
                             struct bulk_buffer  *buffer,
                             void                *meta)
{
    DEBUG("cb_copy_received()\n");
    struct bulk_net_proxy *p = channel->user_state;
    assert(p->bulk_bound && p->net_bound);
    send_data_transfer(p, buffer, meta, true);
}


/******************************************************************************/
/* Initialization */

static void cb_bind(void *arg, errval_t err, struct bulk_channel *c)
{
    struct bulk_net_proxy *p = arg;
    p->err = err;
    p->bulk_bound = true;
}

static errval_t channel_bind(struct bulk_net_proxy           *p,
                             struct bulk_endpoint_descriptor *epd)
{
    errval_t err;
    struct bulk_channel_bind_params bind_params = {
        .role = BULK_ROLE_SLAVE,
        .trust = BULK_TRUST_FULL,
        .waitset = p->ws,
    };
    struct bulk_continuation cont = {
        .handler = cb_bind,
        .arg = p,
    };
    DEBUG("before bulk_channel_bind, %p\n", epd->f->channel_bind);


    p->bulk_bound = false;
    err = bulk_channel_bind(&p->channel, epd, &callbacks, &bind_params, cont);
    if (err_is_fail(err)) {
        return err;
    }

    p->channel.user_state = p;

    while (!p->bulk_bound) {
        event_dispatch(p->ws);
    }

    p->zero_meta = calloc(1, p->channel.meta_size);

    return p->err;
}

errval_t bulk_net_proxy_listen(struct bulk_net_proxy           *p,
                               struct bulk_endpoint_descriptor *desc,
                               struct waitset                  *ws,
                               size_t                           buffer_size,
                               const char                      *card,
                               uint8_t                          queue,
                               uint16_t                         port,
                               void (*connected)(struct bulk_net_proxy *))
{
    errval_t err;
    p->card = card;
    p->queue = queue;
    p->ws = ws;
    p->buffer_size = buffer_size;
    p->l_port = port;
    p->connected = connected;

    err = channel_bind(p, desc);
    if (err_is_fail(err)) {
        return err;
    }

    return t_export(p);
}

errval_t bulk_net_proxy_connect(struct bulk_net_proxy           *p,
                                struct bulk_endpoint_descriptor *desc,
                                struct waitset                  *ws,
                                size_t                           buffer_size,
                                const char                      *card,
                                uint8_t                          queue,
                                uint32_t                         ip,
                                uint16_t                         port,
                                void (*connected)(struct bulk_net_proxy *))
{
    errval_t err;
    DEBUG("inside proxy connect, %p\n", ws);
    p->ws = ws;
    p->card = card;
    p->queue = queue;
    p->r_port = port;
    p->buffer_size = buffer_size;
    p->r_ip = ip;
    p->connected = connected;

    DEBUG("before channel bind. %p, %p, %p\n", p, desc, desc->f->channel_bind);
    err = channel_bind(p, desc);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG("before tbind\n");
    err = t_bind(p);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG("Sending bind request...\n");
    send_bind_request(p, p->buffer_size, p->channel.meta_size);
    DEBUG("Sent bind request\n");
    return SYS_ERR_OK;
}

