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
#include <bulk_transfer/bulk_net.h>
#include <bulk_transfer/bulk_allocator.h>
#include <ipv4/lwip/inet.h>

#include "../../bulk_pool.h"
#include "../../bulk_buffer.h"
#include "../../helpers.h"

#include "bulk_net_backend.h"
#include "bulk_net_transfer.h"

#if BULK_NET_ENABLE_DEBUG_BACKEND
#define BNT_DEBUG_TRACE BULK_NET_TRACE
#define BNT_DEBUG(fmt, msg...) BULK_NET_DEBUG(fmt, msg)
#else
#define BNT_DEBUG(fmt, msg...) do{}while(0);
#define BNT_DEBUG_TRACE do{}while(0);
#endif

#if BULK_NET_ENABLE_STATUS_BACKEND
#define BNT_STATUS(fmt, msg...) debug_printf("%s(): "fmt"\n", __func__,  msg);
#else
#define BNT_STATUS(fmt, msg...) do{} while(0);
#endif

#define BULK_NET_BUFFER_SIZE 0x1000

struct pending_pool_request
{
    struct bulk_pool *pool;
    struct bulk_continuation cont;
    struct pending_pool_request *next;
};

struct transmit_buffer;
struct receive_buffer;
struct bulk_net_transp
{
    struct bulk_net_control net_ctrl;

    struct bulk_channel *channel;
    struct bulk_continuation bind_cont;
    struct pending_pool_request *pending_pool_requests;

    errval_t err;
    bool bound;
    struct bulk_continuation panic_cont;
    void *zero_meta;

    void *user_state;
};

enum proto_msg
{
    PROTO_INVALID,
    PROTO_BIND_REQUEST,
    PROTO_BIND_RESPONSE,
    PROTO_POOL_REQUEST,
    PROTO_POOL_RESPONSE,
    PROTO_BUFFER_MOVE,
    PROTO_BUFFER_COPY,
    PROTO_BUFFER_PASS,
    PROTO_BUFFER_RELEASE,
    PROTO_STATUS,

    /* NOT IMPLEMENTED */
    PROTO_POOL_REMOVE,
    PROTO_TEARDOWN
};

struct proto_trail_bind_req
{
    uint32_t buffer_size;
    uint8_t trust_level;
    uint8_t role;
    /* XXX: there are no constraints on this channel */

    uint8_t type;
}__attribute__((packed));

struct proto_trail_bind_resp
{
    uint32_t buffer_size;   ///< XXX: given by the creator side
    uint32_t meta_size;     ///< XXX: given by the creator side
    uint8_t direction;
    uint8_t trust_level;
    uint8_t role;
    errval_t err;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_pool_req
{
    uint32_t buffer_count;
    uint32_t buffer_size;
    uint32_t pool_machine_id;
    domainid_t pool_domain_id;
    uint32_t pool_local_id;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_pool_resp
{
    errval_t err;
    uint32_t pool_machine_id;
    domainid_t pool_domain_id;
    uint32_t pool_local_id;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_move
{
    uint32_t pool_machine_id;
    domainid_t pool_domain_id;
    uint32_t pool_local_id;
    uint32_t buffer_id;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_copy
{
    uint32_t pool_machine_id;
    domainid_t pool_domain_id;
    uint32_t pool_local_id;
    uint32_t buffer_id;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_pass
{
    uint32_t pool_machine_id;
    domainid_t pool_domain_id;
    uint32_t pool_local_id;
    uint32_t buffer_id;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_release
{
    uint32_t pool_machine_id;
    domainid_t pool_domain_id;
    uint32_t pool_local_id;
    uint32_t buffer_id;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_status
{
    errval_t err;

    uint8_t type;
}__attribute__((packed));

/* ----------------------------- pools ------------------------------------- */
static inline struct bulk_buffer *get_buffer(struct bulk_channel *chan,
                                             struct bulk_pool_id *pool_id,
                                             uint32_t buffer_id)
{
    struct bulk_pool *pool = bulk_pool_get(pool_id, chan);
    assert(pool);
    assert(buffer_id < pool->num_buffers);
    return pool->buffers[buffer_id];
}

/* --------------------------- binding ------------------------------------- */

static void send_bind_response(struct bulk_net_transp *p,
                               uint32_t buffer_size,
                               uint32_t meta_size,
                               uint8_t direction,
                               uint8_t role,
                               uint8_t trust,
                               errval_t err)
{
    BNT_DEBUG_TRACE

    struct proto_trail_bind_resp *t;
    struct transmit_buffer *tb;
    struct bulk_net_msgdesc msg;

    tb = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb != NULL);

    t = tb->int_virt;
    t->err = err;
    t->buffer_size = buffer_size;
    t->meta_size = meta_size;
    t->direction = direction;
    t->role = role;
    t->trust_level = trust;
    t->type = PROTO_BIND_RESPONSE;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t);
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_bind_response(struct bulk_net_transp *p,
                                 struct proto_trail_bind_resp *t,
                                 struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    if (p->bound) {
        debug_printf("Ignoring bind response to already bound\n");
        goto free;
    }

    struct bulk_channel *chan = p->channel;

    assert(chan->state == BULK_STATE_BINDING);

    chan->meta_size = t->meta_size;
    chan->trust = t->trust_level;
    chan->role = t->role;
    chan->direction = t->direction;

    if (err_is_fail(t->err)) {
        BNT_DEBUG("ERROR: binding failed. %s\n", err_getstring(t->err));
        chan->state = BULK_STATE_CLOSED;
    } else {
        BNT_DEBUG("SUCCESS: channel %p bound", chan);
        chan->state = BULK_STATE_CONNECTED;
        p->bound = true;
    }

    p->zero_meta = calloc(1, chan->meta_size);

    if (p->bind_cont.handler) {
        p->bind_cont.handler(p->bind_cont.arg, t->err, p->channel);
    }

    free: bulk_net_transfer_free_rx(&p->net_ctrl, msg);
}

static void send_bind_request(struct bulk_net_transp *p,
                              uint32_t buffer_size,
                              uint8_t trust_level,
                              uint8_t role)
{
    BNT_DEBUG_TRACE

    errval_t err;

    struct proto_trail_bind_req *t;
    struct transmit_buffer *tb;
    struct bulk_net_msgdesc msg;

    tb = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb != NULL);

    t = tb->int_virt;
    t->buffer_size = buffer_size;
    t->role = role;
    t->trust_level = trust_level;
    t->type = PROTO_BIND_REQUEST;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t);
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_bind_request(struct bulk_net_transp *p,
                                struct proto_trail_bind_req *t,
                                struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    errval_t err;

    struct receive_buffer *rb = msg->parts[0].opaque;
    struct packet_header *hdr = rb->hdr_virt;

    if (p->bound) {
        debug_printf("Already bound!\n");
        goto free;
    }

    p->net_ctrl.r_mac = 0;
    memcpy(&p->net_ctrl.r_mac, hdr->l2.smac, 6);
    p->net_ctrl.r_ip = ntohl(hdr->l3.s_ip);
    p->net_ctrl.r_port = ntohs(hdr->l4.s_port);

    bulk_net_transfer_update_tx_headers(&p->net_ctrl);

    if (t->buffer_size != p->net_ctrl.buffer_size) {
        BNT_DEBUG("ERROR:  buffer sizes do not match: %i", t->buffer_size);
        err = BULK_TRANSFER_ALLOC_BUFFER_SIZE;
        goto send_and_free;
    }

    err = p->channel->callbacks->bind_received(p->channel);
    if (err_is_fail(err)) {
        BNT_DEBUG("ERROR: bind request rejected. %s", err_getstring(err));
        goto send_and_free;
    } else {
        p->bound = true;
    }

    if (p->channel->role == BULK_ROLE_GENERIC) {
        if (t->role == BULK_ROLE_GENERIC) {
            p->channel->role = BULK_ROLE_MASTER;
        } else {
            p->channel->role = bulk_role_other(t->role);
        }
    }

    if (p->channel->trust != t->trust_level) {
        /* TODO: chose appropriate trust level */
        p->channel->trust = BULK_TRUST_NONE;
    }

    p->channel->state = BULK_STATE_CONNECTED;

    send_and_free: send_bind_response(
                    p, p->net_ctrl.buffer_size, p->channel->meta_size,
                    bulk_direction_other(p->channel->direction),
                    bulk_role_other(p->channel->role), p->channel->trust, err);

    free: bulk_net_transfer_free_rx(&p->net_ctrl, msg);
}

/* -------------------------- pool assignment -------------------------------*/

static void send_pool_assign_response(struct bulk_net_transp *p,
                                      errval_t err,
                                      struct bulk_pool *pool)
{
    BNT_DEBUG_TRACE

    struct proto_trail_pool_resp *t;
    struct transmit_buffer *tb;
    struct bulk_net_msgdesc msg;

    tb = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb != NULL);

    t = tb->int_virt;
    t->err = err;
    t->pool_domain_id = pool->id.dom;
    t->pool_machine_id = pool->id.machine;
    t->pool_local_id = pool->id.local;
    t->type = PROTO_POOL_RESPONSE;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t);
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_pool_assign_response(struct bulk_net_transp *p,
                                        struct proto_trail_pool_resp *t,
                                        struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    errval_t err;

    struct pending_pool_request *ppr = p->pending_pool_requests;
    struct pending_pool_request *prev = NULL;

    struct bulk_pool_id id = {
        .dom = t->pool_domain_id,
        .machine = t->pool_machine_id,
        .local = t->pool_local_id };

    while (ppr) {
        if (bulk_pool_cmp_id(&id, &ppr->pool->id) == 0) {
            if (prev == NULL) {
                p->pending_pool_requests = ppr->next;
            } else {
                prev->next = ppr->next;
            }
            break;
        }
        prev = ppr;
        ppr = ppr->next;
    }
    if (ppr == NULL) {
        BNT_DEBUG("NOTICE: no pending binding request (ignored). [%i, %i]",
                  (uint32_t )id.dom, id.local);
        goto free;
    }

    struct bulk_pool *pool = ppr->pool;
    if (err_is_fail(t->err)) {
        DEBUG_ERR(t->err, "no assignment of the pool");
        goto free;
    }

    err = bulk_pool_assign(pool, p->channel);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "assignment to the channel failed");
        goto free;
    }

    free: if (ppr->cont.handler) {
        ppr->cont.handler(ppr->cont.arg, t->err, p->channel);
    }

    free(ppr);
    bulk_net_transfer_free_rx(&p->net_ctrl, msg);
}

static void send_pool_assign_request(struct bulk_net_transp *p,
                                     struct bulk_pool *pool)
{
    BNT_DEBUG_TRACE

    errval_t err;

    struct proto_trail_pool_req *t;
    struct transmit_buffer *tb;
    struct bulk_net_msgdesc msg;

    tb = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb != NULL);

    t = tb->int_virt;
    t->buffer_size = pool->buffer_size;
    t->buffer_count = pool->num_buffers;
    t->pool_domain_id = pool->id.dom;
    t->pool_machine_id = pool->id.machine;
    t->pool_local_id = pool->id.local;
    t->type = PROTO_POOL_REQUEST;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t);
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));

}

static void handle_pool_assign_request(struct bulk_net_transp *p,
                                       struct proto_trail_pool_req *t,
                                       struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    errval_t err;

    struct bulk_pool_id id = {
        .dom = t->pool_domain_id,
        .machine = t->pool_machine_id,
        .local = t->pool_local_id };

    struct bulk_pool *pool = bulk_pool_domain_list_get(&id);

    uint8_t first_assignment = 0;

    if (pool == NULL) {
        struct bulk_allocator pool_alloc;

        /* TODO: pool constraints */

        err = bulk_alloc_init(&pool_alloc, t->buffer_count, t->buffer_size,
        NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failed to allocate memory for the pool\n");
            goto send_and_free;
        }
        /* overwrite the ID */
        pool_alloc.pool->id = id;

        pool = pool_alloc.pool;

        first_assignment = 1;

        /* TODO: Free the allocator resources */

        BNT_DEBUG("Pool created. [%i, %i]", (uint32_t )id.dom, id.local)
    } else {
        BNT_DEBUG("Pool reuse. [%i, %i]", (uint32_t )id.dom, id.local)
    }

    err = p->channel->callbacks->pool_assigned(p->channel, pool);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "User revokes pool assignment\n");
        if (first_assignment) {
            bulk_pool_domain_list_remove(pool);
        }
        bulk_pool_dealloc(pool);
        goto send_and_free;
    }

    err = bulk_pool_assign(pool, p->channel);
    assert(!err_is_fail(err)); // should not fail

    send_and_free: send_pool_assign_response(p, err, pool);

    bulk_net_transfer_free_rx(&p->net_ctrl, msg);
}

/* ---------------------------- move operation ----------------------------- */

static void send_buffer_move(struct bulk_net_transp *p,
                             struct bulk_buffer *b,
                             void *meta,
                             struct bulk_continuation cont)
{
    BNT_DEBUG_TRACE

    errval_t err;
    struct proto_trail_move *t;
    struct transmit_buffer *tb_d, *tb;
    struct bulk_net_msgdesc msg;

    tb_d = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb_d != NULL);
    tb_d->buffer = b;
    tb_d->is_copy = false;
    tb_d->cont = cont;

    // prepare trailer
    tb = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb != NULL);

    if (meta != NULL) {
        memcpy(tb->int_virt, meta, p->channel->meta_size);
    } else {
        memset(tb->int_virt, 0, p->channel->meta_size);
    }
    t = (void *) ((uint8_t *) tb->int_virt + p->channel->meta_size);
    t->type = PROTO_BUFFER_MOVE;
    t->pool_domain_id = b->pool->id.dom;
    t->pool_local_id = b->pool->id.local;
    t->pool_machine_id = b->pool->id.machine;
    t->buffer_id = ((lvaddr_t) b->address - b->pool->base_address)
                    / b->pool->buffer_size;

    msg.parts[1].phys = b->phys;
    msg.parts[1].size = p->net_ctrl.buffer_size;
    msg.parts[1].opaque = tb_d;
    msg.parts[2].phys = tb->int_phys;
    msg.parts[2].size = sizeof(*t) + p->channel->meta_size;
    msg.parts[2].opaque = tb;
    msg.parts[3].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_buffer_move(struct bulk_net_transp *p,
                               struct proto_trail_move *t,
                               struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    errval_t err;
    struct receive_buffer *rb;

    struct bulk_pool_id id = {
        .machine = t->pool_machine_id,
        .local = t->pool_local_id,
        .dom = t->pool_domain_id, };

    struct bulk_buffer *buf = get_buffer(p->channel, &id, t->buffer_id);

    assert(buf);

    err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_WRITE);
    assert(!err_is_fail(err));

    rb = msg->parts[1].opaque;
    memcpy(buf->address, rb->virt, buf->pool->buffer_size);
    bulk_net_transfer_free_rb(&p->net_ctrl, rb);

    rb = msg->parts[2].opaque;

    p->channel->callbacks->move_received(p->channel, buf, rb->virt);

    bulk_net_transfer_free_rb(&p->net_ctrl, rb);
}

/* ----------------------------- copy operation ---------------------------- */

static void send_buffer_copy(struct bulk_net_transp *p,
                             struct bulk_buffer *b,
                             void *meta,
                             struct bulk_continuation cont)
{
    BNT_DEBUG_TRACE

    errval_t err;
    struct proto_trail_move *t;
    struct transmit_buffer *tb_d, *tb;
    struct bulk_net_msgdesc msg;

    tb_d = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb_d != NULL);
    tb_d->buffer = b;
    tb_d->is_copy = true;
    tb_d->cont = cont;

    // prepare trailer
    tb = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb != NULL);

    if (meta != NULL) {
        memcpy(tb->int_virt, meta, p->channel->meta_size);
    } else {
        memset(tb->int_virt, 0, p->channel->meta_size);
    }
    t = (void *) ((uint8_t *) tb->int_virt + p->channel->meta_size);
    t->type = PROTO_BUFFER_COPY;
    t->pool_domain_id = b->pool->id.dom;
    t->pool_local_id = b->pool->id.local;
    t->pool_machine_id = b->pool->id.machine;
    t->buffer_id = ((lvaddr_t) b->address - b->pool->base_address)
                    / b->pool->buffer_size;

    msg.parts[1].phys = b->phys;
    msg.parts[1].size = p->net_ctrl.buffer_size;
    msg.parts[1].opaque = tb_d;
    msg.parts[2].phys = tb->int_phys;
    msg.parts[2].size = sizeof(*t) + p->channel->meta_size;
    msg.parts[2].opaque = tb;
    msg.parts[3].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_buffer_copy(struct bulk_net_transp *p,
                               struct proto_trail_copy *t,
                               struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    errval_t err;
    struct receive_buffer *rb;

    struct bulk_pool_id id = {
        .machine = t->pool_machine_id,
        .local = t->pool_local_id,
        .dom = t->pool_domain_id, };

    struct bulk_buffer *buf = get_buffer(p->channel, &id, t->buffer_id);

    BNT_DEBUG("buf=%p, id=%i", buf, t->buffer_id);

    assert(buf);
    err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_WRITE);
    assert(!err_is_fail(err));

    rb = msg->parts[1].opaque;
    memcpy(buf->address, rb->virt, buf->pool->buffer_size);
    bulk_net_transfer_free_rb(&p->net_ctrl, rb);

    rb = msg->parts[2].opaque;

    enum bulk_buffer_state st = BULK_BUFFER_READ_ONLY;
    if (bulk_buffer_is_owner(buf)) {
        st = BULK_BUFFER_RO_OWNED;
    }
    err = bulk_buffer_change_state(buf, st);
    assert(!err_is_fail(err));

    p->channel->callbacks->copy_received(p->channel, buf, rb->virt);

    bulk_net_transfer_free_rb(&p->net_ctrl, rb);
}

/* ------------------------------ pass operation --------------------------- */

static void send_buffer_pass(struct bulk_net_transp *p,
                             struct bulk_buffer *b,
                             void *meta,
                             struct bulk_continuation cont)
{
    BNT_DEBUG_TRACE

    errval_t err;
    struct proto_trail_move *t;
    struct transmit_buffer *tb;
    struct bulk_net_msgdesc msg;

    // prepare trailer
    tb = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb != NULL);
    tb->cont = cont;
    tb->buffer = b;
    if (meta != NULL) {
        memcpy(tb->int_virt, meta, p->channel->meta_size);
    } else {
        memset(tb->int_virt, 0, p->channel->meta_size);
    }
    t = (void *) ((uint8_t *) tb->int_virt + p->channel->meta_size);
    t->type = PROTO_BUFFER_PASS;
    t->pool_domain_id = b->pool->id.dom;
    t->pool_local_id = b->pool->id.local;
    t->pool_machine_id = b->pool->id.machine;
    t->buffer_id = ((lvaddr_t) b->address - b->pool->base_address)
                    / b->pool->buffer_size;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t) + p->channel->meta_size;
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_buffer_pass(struct bulk_net_transp *p,
                               struct proto_trail_pass *t,
                               struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    errval_t err;
    struct receive_buffer *rb;

    struct bulk_pool_id id = {
        .machine = t->pool_machine_id,
        .local = t->pool_local_id,
        .dom = t->pool_domain_id, };

    struct bulk_buffer *buf = get_buffer(p->channel, &id, t->buffer_id);

    assert(buf);

    err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_WRITE);
    assert(!err_is_fail(err));

    rb = msg->parts[1].opaque;

    p->channel->callbacks->buffer_received(p->channel, buf, rb->virt);

    bulk_net_transfer_free_rb(&p->net_ctrl, rb);
}

/* ----------------------------- release operation ------------------------- */

static void send_buffer_release(struct bulk_net_transp *p,
                                struct bulk_buffer *b,
                                void *meta,
                                struct bulk_continuation cont)
{
    BNT_DEBUG_TRACE

    errval_t err;
    struct proto_trail_move *t;
    struct transmit_buffer *tb;
    struct bulk_net_msgdesc msg;

    // prepare trailer
    tb = stack_alloc_alloc(&p->net_ctrl.tb_stack);
    assert(tb != NULL);
    tb->cont = cont;
    tb->buffer = b;
    if (meta != NULL) {
        memcpy(tb->int_virt, meta, p->channel->meta_size);
    } else {
        memset(tb->int_virt, 0, p->channel->meta_size);
    }
    t = (void *) ((uint8_t *) tb->int_virt + p->channel->meta_size);
    t->type = PROTO_BUFFER_RELEASE;
    t->pool_domain_id = b->pool->id.dom;
    t->pool_local_id = b->pool->id.local;
    t->pool_machine_id = b->pool->id.machine;
    t->buffer_id = ((lvaddr_t) b->address - b->pool->base_address)
                    / b->pool->buffer_size;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t) + p->channel->meta_size;
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_buffer_release(struct bulk_net_transp *p,
                                  struct proto_trail_release *t,
                                  struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    errval_t err;
    struct receive_buffer *rb;

    struct bulk_pool_id id = {
        .machine = t->pool_machine_id,
        .local = t->pool_local_id,
        .dom = t->pool_domain_id, };

    struct bulk_buffer *buf = get_buffer(p->channel, &id, t->buffer_id);

    assert(buf);

    rb = msg->parts[1].opaque;

    buf->local_ref_count--;

    if (buf->state == BULK_BUFFER_RO_OWNED && bulk_buffer_can_release(buf)) {
        err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_WRITE);
        assert(!err_is_fail(err));
    }

    p->channel->callbacks->copy_released(p->channel, buf);

    bulk_net_transfer_free_rb(&p->net_ctrl, rb);
}

/* ---------------------------- status message ----------------------------- */
static void send_status_msg(void)
{
    BNT_DEBUG_TRACE

}

static void handle_status_msg(struct bulk_net_transp *p,
                              struct proto_trail_status *t,
                              struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

}

/* ------------------------ network managements ---------------------------- */

static void tcb_transmitted(struct bulk_e10k *bu, void *opaque)
{
    struct bulk_net_transp *p = bu->opaque;
    struct transmit_buffer *tb = opaque;

    if (opaque == NULL) {
        // We can ignore the header buffers
        return;
    }

    if (tb->buffer != NULL) {
        if (tb->cont.handler) {
            tb->cont.handler(tb->cont.arg, SYS_ERR_OK, p->channel);
        }
        tb->buffer = NULL;
        tb->cont = BULK_CONT_NOP;
    }
    assert(tb != NULL);
    stack_alloc_free(&p->net_ctrl.tb_stack, tb);
}

static void tcb_received(struct bulk_e10k* bu, struct bulk_net_msgdesc *msg)
{
    struct bulk_net_transp *p = bu->opaque;
    size_t i;
    struct receive_buffer *rb;
    uint8_t *t;

    assert(msg->parts[0].size == sizeof(struct packet_header));
    bulk_net_transfer_strip_padding(msg);

    for (i = 0; i < BULK_NET_DESCLEN && msg->parts[i].size != 0; i++)
        ;
    i--;

    rb = msg->parts[i].opaque;
    t = rb->virt;

    switch (t[msg->parts[i].size - 1]) {
        case PROTO_BIND_REQUEST:
            handle_bind_request(
                            p,
                            (struct proto_trail_bind_req *) (t
                                            + msg->parts[i].size
                                            - sizeof(struct proto_trail_bind_req)),
                            msg);
            break;
        case PROTO_BIND_RESPONSE:
            handle_bind_response(
                            p,
                            (struct proto_trail_bind_resp *) (t
                                            + msg->parts[i].size
                                            - sizeof(struct proto_trail_bind_resp)),
                            msg);
            break;
        case PROTO_POOL_REQUEST:
            handle_pool_assign_request(
                            p,
                            (struct proto_trail_pool_req *) (t
                                            + msg->parts[i].size
                                            - sizeof(struct proto_trail_pool_req)),
                            msg);
            break;
        case PROTO_POOL_RESPONSE:
            handle_pool_assign_response(
                            p,
                            (struct proto_trail_pool_resp *) (t
                                            + msg->parts[i].size
                                            - sizeof(struct proto_trail_pool_resp)),
                            msg);
            break;
        case PROTO_BUFFER_MOVE:
            handle_buffer_move(
                            p,
                            (struct proto_trail_move *) (t + msg->parts[i].size
                                            - sizeof(struct proto_trail_move)),
                            msg);
            break;
        case PROTO_BUFFER_COPY:
            handle_buffer_copy(
                            p,
                            (struct proto_trail_copy *) (t + msg->parts[i].size
                                            - sizeof(struct proto_trail_copy)),
                            msg);
            break;
        case PROTO_BUFFER_PASS:
            handle_buffer_pass(
                            p,
                            (struct proto_trail_pass *) (t + msg->parts[i].size
                                            - sizeof(struct proto_trail_pass)),
                            msg);
            break;
        case PROTO_BUFFER_RELEASE:
            handle_buffer_release(
                            p,
                            (struct proto_trail_release *) (t
                                            + msg->parts[i].size
                                            - sizeof(struct proto_trail_release)),
                            msg);
            break;
        case PROTO_STATUS:
            handle_status_msg(
                            p,
                            (struct proto_trail_status *) (t
                                            + msg->parts[i].size
                                            - sizeof(struct proto_trail_status)),
                            msg);
            break;
        default:
            USER_PANIC("Unsupported Request")
            break;

    }
}

/* --------------------- implementation callbacks -------------------------- */

static errval_t impl_channel_create(struct bulk_channel *channel)
{
    errval_t err;

    BNT_STATUS("Creating new bulk channel [%p] using net.transparent backend",
               channel);

    struct bulk_net_transp *p = malloc(sizeof(struct bulk_net_transp));
    if (p == NULL) {
        return BULK_TRANSFER_MEM;
    }

    struct bulk_net_endpoint_descriptor *ep =
        (struct bulk_net_endpoint_descriptor *) channel->ep;

    p->net_ctrl.card = ep->cardname;
    p->net_ctrl.l_port = ep->port;
    p->net_ctrl.queue = ep->queue;
    p->net_ctrl.ws = channel->waitset;
    p->net_ctrl.buffer_size = ep->buffer_size;
    p->net_ctrl.buffer_count = ep->buffer_count;
    p->net_ctrl.max_queues = ep->max_queues;
    p->net_ctrl.num_queues = 1;

    channel->state = BULK_STATE_BINDING;

    err = bulk_net_transfer_export(&p->net_ctrl, tcb_transmitted, tcb_received);
    if (err_is_fail(err)) {
        free(p);
        return err;
    }

    p->channel = channel;
    channel->impl_data = p;

    p->zero_meta = calloc(1, p->channel->meta_size);

    return err;
}

static errval_t impl_channel_bind(struct bulk_channel *channel,
                                  struct bulk_continuation cont)
{
    errval_t err;

    BNT_STATUS("Binding new bulk channel [%p] using net.transparent backend",
               channel);

    struct bulk_net_transp *bnt = malloc(sizeof(struct bulk_net_transp));
    if (!bnt) {
        return BULK_TRANSFER_MEM;
    }

    struct bulk_net_endpoint_descriptor *ep =
        (struct bulk_net_endpoint_descriptor *) channel->ep;

    bnt->net_ctrl.card = ep->cardname;
    bnt->net_ctrl.r_port = ep->port;
    bnt->net_ctrl.r_ip = ep->ip.addr;
    bnt->net_ctrl.queue = ep->queue;
    bnt->net_ctrl.ws = channel->waitset;
    bnt->net_ctrl.buffer_size = ep->buffer_size;
    bnt->net_ctrl.buffer_count = ep->buffer_count;
    bnt->net_ctrl.max_queues = ep->max_queues;
    bnt->net_ctrl.num_queues = 1;

    err = bulk_net_transfer_bind(&bnt->net_ctrl, tcb_transmitted, tcb_received);
    if (err_is_fail(err)) {
        free(bnt);
        return err;
    }

    channel->impl_data = bnt;
    bnt->channel = channel;
    channel->state = BULK_STATE_BINDING;
    bnt->bind_cont = cont;

    send_bind_request(bnt, bnt->net_ctrl.buffer_size, channel->trust,
                      channel->role);
    send_status_msg();

    return SYS_ERR_OK;
}

static errval_t impl_channel_assign_pool(struct bulk_channel *channel,
                                         struct bulk_pool *pool,
                                         struct bulk_continuation cont)
{

    struct bulk_net_transp *bnt = (struct bulk_net_transp *) channel->impl_data;

    if (bnt->net_ctrl.buffer_size != pool->buffer_size) {
        /* TODO: change to buffer size */
        debug_printf("ERROR: only pools with matching buffer size can be assigned\n");
        return BULK_TRANSFER_ALLOC_BUFFER_SIZE;
    }

    struct pending_pool_request *req = malloc(
                    sizeof(struct pending_pool_request));
    if (!req) {
        return BULK_TRANSFER_MEM;
    }

    req->cont = cont;
    req->pool = pool;
    if (bnt->pending_pool_requests) {
        req->next = bnt->pending_pool_requests->next;
    } else {
        req->next = NULL;
    }
    bnt->pending_pool_requests = req;

    send_pool_assign_request(bnt, pool);

    return SYS_ERR_OK;
}

static errval_t impl_channel_move(struct bulk_channel *channel,
                                  struct bulk_buffer *buffer,
                                  void *meta,
                                  struct bulk_continuation cont)
{
    struct bulk_net_transp *bnt = (struct bulk_net_transp *) channel->impl_data;
    send_buffer_move(bnt, buffer, meta, cont);
    return SYS_ERR_OK;
}

/**
 *
 */
static errval_t impl_channel_pass(struct bulk_channel *channel,
                                  struct bulk_buffer *buffer,
                                  void *meta,
                                  struct bulk_continuation cont)
{
    struct bulk_net_transp *bnt = (struct bulk_net_transp *) channel->impl_data;
    send_buffer_pass(bnt, buffer, meta, cont);
    return SYS_ERR_OK;
}

static errval_t impl_channel_copy(struct bulk_channel *channel,
                                  struct bulk_buffer *buffer,
                                  void *meta,
                                  struct bulk_continuation cont)
{
    struct bulk_net_transp *bnt = (struct bulk_net_transp *) channel->impl_data;
    send_buffer_copy(bnt, buffer, meta, cont);
    return SYS_ERR_OK;
}

static errval_t impl_channel_release(struct bulk_channel *channel,
                                     struct bulk_buffer *buffer,
                                     struct bulk_continuation cont)
{
    struct bulk_net_transp *bnt = (struct bulk_net_transp *) channel->impl_data;
    send_buffer_release(bnt, buffer, bnt->zero_meta, cont);
    return SYS_ERR_OK;
}

static struct bulk_implementation bulk_net_implementation = {
    .channel_create = impl_channel_create,
    .channel_bind = impl_channel_bind,
    .assign_pool = impl_channel_assign_pool,
    .move = impl_channel_move,
    .pass = impl_channel_pass,
    .copy = impl_channel_copy,
    .release = impl_channel_release };

struct bulk_implementation *bulk_net_get_impl(void)
{
    return &bulk_net_implementation;
}

