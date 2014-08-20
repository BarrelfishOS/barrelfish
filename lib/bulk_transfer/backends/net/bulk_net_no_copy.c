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
#define BNT_STATUS(fmt, msg...) BULK_NET_STATUS(fmt, msg)
#else
#define BNT_STATUS(fmt, msg...) do{} while(0);
#endif

#define BULK_NET_CTRL_CHANNEL_BUF_SIZE 256

struct bulk_net_nocopy
{
    struct bulk_net_control net_ctrl;

    struct bulk_net_nocopy *bulk_control;
    struct bulk_channel *channel;
    struct bulk_pool *pool;
    struct bulk_continuation bind_cont;
    struct pending_pool_request *pending_pool_requests;
    struct receive_buffer *meta_rb;
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
    uint16_t port;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_pool_resp
{
    errval_t err;
    uint32_t pool_machine_id;
    domainid_t pool_domain_id;
    uint32_t pool_local_id;
    uint16_t port;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_move
{
    uint32_t buffer_id;

    uint8_t type;
}__attribute__((packed));

struct proto_trail_copy
{
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

static void tcb_received(struct bulk_e10k* bu, struct bulk_net_msgdesc *msg);
static void tcb_transmitted(struct bulk_e10k *bu, void *opaque);

struct pending_pool_request
{
    struct bulk_pool *pool;
    struct bulk_net_nocopy *bnt;
    struct bulk_continuation cont;
    struct pending_pool_request *next;
};

struct bulk_net_pool_data
{
    struct bulk_net_nocopy *p;
    uint32_t *buf_id_local_to_remote;
    uint32_t *buf_id_remote_to_local;
};

/* ----------------------------- pools ------------------------------------- */
static inline struct bulk_net_pool_data *get_pool_data(struct bulk_pool *pool)
{
    return ((struct bulk_pool_internal*) pool)->impl_data;
}

static inline struct bulk_net_nocopy *get_net_nocopy(struct bulk_pool *pool)
{
    struct bulk_net_pool_data *pd = get_pool_data(pool);
    if (pd) {
        return pd->p;
    }
    return NULL;
}

static inline struct bulk_buffer *get_buffer(struct bulk_channel *chan,
                                             struct bulk_pool_id *pool_id,
                                             uint32_t buffer_id)
{
    struct bulk_pool *pool = bulk_pool_get(pool_id, chan);
    assert(pool);
    assert(buffer_id < pool->num_buffers);
    return pool->buffers[buffer_id];
}

/* ---------------------------- buffer id translation ---------------------- */

static inline uint32_t get_local_bufid(struct bulk_buffer *buf)
{
    return ((lvaddr_t) buf->address - buf->pool->base_address)
                    / buf->pool->buffer_size;
}

/// XXX: assuming pool goes just over one net
static inline uint32_t get_remote_bufid(struct bulk_pool *pool,
                                        uint32_t local_buf_id)
{
    struct bulk_net_pool_data *pd = get_pool_data(pool);
    assert(pd);
    return pd->buf_id_local_to_remote[local_buf_id];
}

static inline void set_remote_bufid(struct bulk_pool *pool,
                                    uint32_t local_buf_id,
                                    uint32_t remote_buf_id)
{
    struct bulk_net_pool_data *pd = get_pool_data(pool);
    assert(pd);
    pd->buf_id_local_to_remote[local_buf_id] = remote_buf_id;
    pd->buf_id_remote_to_local[remote_buf_id] = local_buf_id;
}

static errval_t bulk_net_init_meta_rb(struct receive_buffer *rbs,
                                      uint32_t num,
                                      uint32_t size)
{
    errval_t err;
    struct receive_buffer tmp_rb, *rb;

    if (BULK_NET_NOCOPY_META_BUFFER_SIZE) {
        size = BULK_NET_NOCOPY_META_BUFFER_SIZE;
    }

    err = allocmap_frame(num * size, &tmp_rb.virt, &tmp_rb.phys, NULL);
    assert(err_is_ok(err));

    for (uint32_t j = 0; j < num; ++j) {
        rb = rbs + j;
        rb->buffer = NULL;
        rb->is_meta = true;
        rb->virt = tmp_rb.virt + (j * size);
        rb->phys = tmp_rb.phys + (j * size);
    }
    return SYS_ERR_OK;
}

/* --------------------------- binding ------------------------------------- */

static void send_bind_response(struct bulk_net_nocopy *p,
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

static void handle_bind_response(struct bulk_net_nocopy *p,
                                 struct proto_trail_bind_resp *t,
                                 struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    struct bulk_channel *chan = p->channel;

    if (p->bound) {
        BNT_DEBUG("channel [%p] already bound. request ignored.", chan);
        goto free_rx;
    }

    assert(chan->state == BULK_STATE_BINDING);

    chan->meta_size = t->meta_size;
    chan->trust = t->trust_level;
    chan->role = t->role;
    chan->direction = t->direction;

    if (err_is_fail(t->err)) {
        BNT_STATUS("ERROR: binding failed on channel [%p].", chan);
        chan->state = BULK_STATE_CLOSED;
    } else {
        BNT_STATUS("SUCCESS: channel [%p] bound.", chan);
        chan->state = BULK_STATE_CONNECTED;
        p->bound = true;
    }

    if (p->bind_cont.handler) {
        p->bind_cont.handler(p->bind_cont.arg, t->err, p->channel);
    }

    free_rx: bulk_net_transfer_free_rx(&p->net_ctrl, msg);
}

static void send_bind_request(struct bulk_net_nocopy *p,
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

static void handle_bind_request(struct bulk_net_nocopy *p,
                                struct proto_trail_bind_req *t,
                                struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    errval_t err;

    assert(p->bulk_control == p);

    struct receive_buffer *rb = msg->parts[0].opaque;
    struct packet_header *hdr = rb->hdr_virt;

    if (p->bound) {
        BNT_DEBUG("channel [%p] already bound. request ignored.", p->channel);

        goto free_rx;
    }

    /* update mac address */
    p->net_ctrl.r_mac = 0;
    memcpy(&p->net_ctrl.r_mac, hdr->l2.smac, 6);

    /* set the remote ip and ports */
    p->net_ctrl.r_ip = ntohl(hdr->l3.s_ip);
    p->net_ctrl.r_port = ntohs(hdr->l4.s_port);

    /* update the TX headers */
    bulk_net_transfer_update_tx_headers(&p->net_ctrl);

    if (t->buffer_size != p->net_ctrl.buffer_size) {
        BNT_DEBUG("ERROR: wrong buffer size: [%x] [%x]", t->buffer_size,
                  (uint32_t )p->net_ctrl.buffer_size);
        err = BULK_TRANSFER_ALLOC_BUFFER_SIZE;
        goto send_and_free;
    }

    /* update the roles */
    if (p->channel->role == BULK_ROLE_GENERIC) {
        if (t->role == BULK_ROLE_GENERIC) {
            p->channel->role = BULK_ROLE_MASTER;
        } else {
            p->channel->role = bulk_role_other(t->role);
        }
    }

    /* update the trust level */
    if (p->channel->trust != t->trust_level) {
        /* TODO: chose appropriate trust level */
        if (p->channel->trust == BULK_TRUST_FULL) {
            p->channel->trust = t->trust_level;
        } else if (p->channel->trust == BULK_TRUST_HALF) {
            if (t->trust_level == BULK_TRUST_NONE) {
                p->channel->trust = BULK_TRUST_NONE;
            }
        }
    }

    /* do the callback tot he application */
    err = p->channel->callbacks->bind_received(p->channel);

    /* update the connectoin state */
    p->channel->state = BULK_STATE_CONNECTED;
    p->bound = true;

    send_and_free: send_bind_response(
                    p, p->net_ctrl.buffer_size, p->channel->meta_size,
                    bulk_direction_other(p->channel->direction),
                    bulk_role_other(p->channel->role), p->channel->trust, err);

    free_rx: bulk_net_transfer_free_rx(&p->net_ctrl, msg);
}

/* -------------------------- pool assignment -------------------------------*/

static void send_pool_assign_response(struct bulk_net_nocopy *p,
                                      errval_t err,
                                      struct bulk_pool *pool,
                                      uint16_t l_port)
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

    t->port = l_port;

    t->type = PROTO_POOL_RESPONSE;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t);
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_pool_assign_response(struct bulk_net_nocopy *p,
                                        struct proto_trail_pool_resp *t,
                                        struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    errval_t err;

    assert(p->bulk_control == p);

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
        BNT_DEBUG("ERROR: no pending binding request (ignored). [%i, %i]",
                  (uint32_t )id.dom, id.local);
        goto free_and_cont;
    }

    struct bulk_pool *pool = ppr->pool;
    if (err_is_fail(t->err)) {
        BNT_STATUS("FAILED: Pool [%x, %x, %x] assign to channel [%p] vetoed",
                   pool->id.machine, pool->id.dom, pool->id.local, p->channel);
        goto free_and_cont;
    }

    err = bulk_pool_assign(pool, p->channel);
    if (err_is_fail(err)) {
        BNT_STATUS("FAILED: Pool [%x, %x, %x] assignment to channel [%p] \n%s",
                   pool->id.machine, pool->id.dom, pool->id.local, p->channel,
                   err_getstring(err));
        goto free_and_cont;
    }

    /* update status values */
    struct bulk_net_nocopy *bnt = ppr->bnt;
    bnt->bound = true;

    /* update port information */
    bnt->net_ctrl.r_port = t->port;
    bulk_net_transfer_update_tx_headers(&bnt->net_ctrl);

    BNT_STATUS("SUCCESS: Pool [%x, %x, %x] assigned to channel [%p]",
               pool->id.machine, pool->id.dom, pool->id.local, p->channel);

    free_and_cont:

    if (ppr->cont.handler) {
        ppr->cont.handler(ppr->cont.arg, t->err, p->channel);
    }

    if (err_is_fail(t->err)) {
        assert(!"NYI: Cleaning up of network structs...");
    }

    free(ppr);
    bulk_net_transfer_free_rx(&p->net_ctrl, msg);
}

static void send_pool_assign_request(struct bulk_net_nocopy *p,
                                     struct bulk_pool *pool,
                                     uint16_t l_port)
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
    t->port = l_port;
    t->type = PROTO_POOL_REQUEST;

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t);
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));

}

static void handle_pool_assign_request(struct bulk_net_nocopy *p,
                                       struct proto_trail_pool_req *t,
                                       struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    assert(p->bulk_control == p);

    errval_t err;
    uint16_t port = 0;
    uint8_t first_assignment = 0;

    struct bulk_net_pool_data *pd = NULL;
    struct bulk_net_nocopy *bnt = NULL;

    /* calculate the new queue */
    uint8_t queueid = p->net_ctrl.queue + p->net_ctrl.num_queues;
    p->net_ctrl.num_queues++;

    /* check if the pool is already present in the domain */
    struct bulk_pool_id id = {
        .dom = t->pool_domain_id,
        .machine = t->pool_machine_id,
        .local = t->pool_local_id };

    struct bulk_pool *pool = bulk_pool_domain_list_get(&id);

    if (p->net_ctrl.num_queues == p->net_ctrl.max_queues) {
        err = BULK_TRANSFER_NET_MAX_QUEUES;
        goto send_and_free;
    }

    /* there is no such pool */
    if (pool == NULL) {
        struct bulk_allocator pool_alloc;

        struct bulk_pool_constraints constr = {
            .range_min = p->channel->constraints.mem_range_min,
            .range_max = p->channel->constraints.mem_range_max,
            .alignment = p->channel->constraints.men_align,
            .trust = p->channel->trust };

        err = bulk_alloc_init(&pool_alloc, t->buffer_count, t->buffer_size,
                              &constr);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failed to allocate memory for the pool\n");
            goto send_and_free;
        }

        /* Free allocator memory*/
        free(pool_alloc.mngs);

        /* overwrite the ID */
        pool = pool_alloc.pool;
        pool->id = id;

        first_assignment = 1;

        BNT_DEBUG("New pool allocated: [%x, %x, %x]", pool->id.machine,
                  pool->id.dom, pool->id.local)

    } else {
        BNT_DEBUG("Pool already present in domain: [%x, %x, %x]",
                  pool->id.machine, pool->id.dom, pool->id.local);
        if (get_net_nocopy(pool)) {
            err = BULK_TRANSFER_NET_POOL_USED;
            goto send_and_free;
        }

        if (bulk_pool_is_assigned(pool, p->channel)) {
            err = BULK_TRANSFER_POOL_ALREADY_ASSIGNED;
            goto send_and_free;
        }
    }

    /* we have a pool and this pool does not go over the network channel */

    struct bulk_pool_internal *pool_int = (struct bulk_pool_internal *) pool;

    assert(!pool_int->impl_data);

    size_t pd_size = sizeof(struct bulk_net_pool_data)
                    + 2 * t->buffer_count * sizeof(uint32_t);

    pd = malloc(pd_size);

    if (!pd) {
        err = BULK_TRANSFER_MEM;
        goto send_and_free;
    }

    pd->buf_id_local_to_remote = (uint32_t *) (pd + 1);
    pd->buf_id_remote_to_local = (pd->buf_id_local_to_remote + t->buffer_count);

    for (uint32_t i = 0; i < t->buffer_count; ++i) {
        pd->buf_id_remote_to_local[i] = 0;
        pd->buf_id_local_to_remote[i] = 0;
    }

    pool_int->impl_data = pd;

    bnt =
        calloc(1,
               sizeof(struct bulk_net_nocopy)
                               + t->buffer_count
                                               * sizeof(struct receive_buffer));
    if (!bnt) {
        err = BULK_TRANSFER_MEM;
        goto send_and_free;
    }

    bnt->meta_rb = (struct receive_buffer *) (bnt + 1);
    err = bulk_net_init_meta_rb(bnt->meta_rb, t->buffer_count,
                                pool->buffer_size);
    assert(!err_is_fail(err));

    memcpy(&bnt->net_ctrl, &p->net_ctrl, sizeof(bnt->net_ctrl));
    bnt->net_ctrl.queue = queueid;
    bnt->net_ctrl.buffer_count = 0;

    pd->p = bnt;
    bnt->net_ctrl.r_port = t->port;
    /* this is the control channel, has just two buffers */

    bnt->bulk_control = p;
    bnt->channel = p->channel;
    bnt->pool = pool;

    err = bulk_net_transfer_bind(&bnt->net_ctrl, tcb_transmitted, tcb_received);
    if (err_is_fail(err)) {
        goto send_and_free;
    }

    err = p->channel->callbacks->pool_assigned(p->channel, pool);
    if (err_is_fail(err)) {
        BNT_STATUS("VETO: Pool [%x, %x, %x] not assigned to channel [%p]",
                   pool->id.machine, pool->id.dom, pool->id.local, p->channel);
        goto send_and_free;
    }

    err = bulk_pool_assign(pool, p->channel);
    assert(!err_is_fail(err)); // should not fail

    BNT_STATUS("SUCCESS: Pool [%x, %x, %x] assigned to channel [%p]",
               pool->id.machine, pool->id.dom, pool->id.local, p->channel);

    /* update status */
    bnt->bound = true;

    /* we must make sure that the buffers are ready for receiving */
    if (p->channel->direction == BULK_DIRECTION_RX) {
        BNT_STATUS("Adding %i receive buffers.", (uint32_t )pool->num_buffers);
        for (uint32_t i = 0; i < pool->num_buffers; ++i) {
            struct receive_buffer *rb;
            struct bulk_buffer *buffer = pool->buffers[i];
            rb = stack_alloc_alloc(&bnt->net_ctrl.rb_stack);
            assert(rb != NULL);

            rb->virt = buffer->address;
            rb->phys = buffer->phys;
            rb->buffer = buffer;

            err = bulk_e10k_rx_add(&bnt->net_ctrl.transfer, rb->phys,
                                   rb->hdr_phys, rb);
            assert(err_is_ok(err));

            rb = bnt->meta_rb + i;
            err = bulk_e10k_rx_add(&bnt->net_ctrl.transfer, rb->phys,
                                   rb->hdr_phys, rb);
            assert(err_is_ok(err));
        }

    }

    port = bnt->net_ctrl.l_port;

    if (!pool) {
        struct bulk_pool tmp_pool;
        tmp_pool.id = id;
        pool = &tmp_pool;
    }

    send_and_free: send_pool_assign_response(p, err, pool, port);

    if (err_is_fail(err)) {
        if (pd) {
            free(pd);
        }
        if (bnt) {
            /* TODO: Free up net resources */
            free(bnt);
        }
        if (first_assignment) {
            bulk_pool_dealloc(pool);
        }
    }

    bulk_net_transfer_free_rx(&p->net_ctrl, msg);
}

/* ---------------------------- move operation ----------------------------- */

static void send_buffer_move(struct bulk_net_nocopy *p,
                             struct bulk_buffer *b,
                             void *meta,
                             struct bulk_continuation cont)
{
    BNT_DEBUG_TRACE

    assert(p->bulk_control != p);
    assert(p->channel->direction == BULK_DIRECTION_TX);

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

    uint32_t local_id = ((lvaddr_t) b->address - b->pool->base_address)
                    / b->pool->buffer_size;
    t->buffer_id = get_remote_bufid(b->pool, local_id);

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

static void handle_buffer_move(struct bulk_net_nocopy *p,
                               struct proto_trail_move *t,
                               struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    assert(p->bulk_control != p);

    errval_t err;
    struct receive_buffer *rb;
    struct bulk_buffer *buffer;

    rb = msg->parts[1].opaque;
    buffer = rb->buffer;
    stack_alloc_free(&p->net_ctrl.rb_stack, rb);

    uint32_t local_id =
        ((lvaddr_t) buffer->address - buffer->pool->base_address)
                        / buffer->pool->buffer_size;

    set_remote_bufid(buffer->pool, local_id, t->buffer_id);

    err = bulk_buffer_change_state(buffer, BULK_BUFFER_READ_WRITE);
    assert(!err_is_fail(err));

    rb = msg->parts[2].opaque;

    p->channel->callbacks->move_received(p->channel, buffer, rb->virt);

    assert(rb->is_meta == true);

    // bulk_net_transfer_free_rb(&p->net_ctrl, rb);
}

/* ----------------------------- copy operation ---------------------------- */

static void send_buffer_copy(struct bulk_net_nocopy *p,
                             struct bulk_buffer *b,
                             void *meta,
                             struct bulk_continuation cont)
{
    BNT_DEBUG_TRACE

    assert(p->bulk_control != p);
    assert(p->channel->direction == BULK_DIRECTION_TX);

    errval_t err;
    struct proto_trail_copy *t;
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

    uint32_t local_id = ((lvaddr_t) b->address - b->pool->base_address)
                    / b->pool->buffer_size;
    t->buffer_id = get_remote_bufid(b->pool, local_id);

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

static void handle_buffer_copy(struct bulk_net_nocopy *p,
                               struct proto_trail_copy *t,
                               struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    assert(p->bulk_control != p);

    errval_t err;
    struct receive_buffer *rb;

    rb = msg->parts[2].opaque;

    struct bulk_buffer *buf = rb->buffer;

    assert(buf);

    uint32_t local_id = ((lvaddr_t) buf->address - buf->pool->base_address)
                    / buf->pool->buffer_size;

    set_remote_bufid(buf->pool, local_id, t->buffer_id);

    enum bulk_buffer_state st = BULK_BUFFER_READ_ONLY;
    if (bulk_buffer_is_owner(buf)) {
        st = BULK_BUFFER_RO_OWNED;
    }
    err = bulk_buffer_change_state(buf, st);
    assert(!err_is_fail(err));

    p->channel->callbacks->copy_received(p->channel, buf, rb->virt);

    assert(rb->is_meta == true);

    // bulk_net_transfer_free_rb(&p->net_ctrl, rb);
}

/* ------------------------------ pass operation --------------------------- */

static void send_buffer_pass(struct bulk_net_nocopy *p,
                             struct bulk_buffer *b,
                             void *meta,
                             struct bulk_continuation cont)
{
    BNT_DEBUG_TRACE

    assert(p->bulk_control == p);

    errval_t err;
    struct proto_trail_pass *t;
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

    uint32_t local_id = ((lvaddr_t) b->address - b->pool->base_address)
                    / b->pool->buffer_size;

    t->buffer_id = get_remote_bufid(b->pool, local_id);

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t) + p->channel->meta_size;
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_buffer_pass(struct bulk_net_nocopy *p,
                               struct proto_trail_pass *t,
                               struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    assert(p->bulk_control == p);

    errval_t err;
    struct receive_buffer *rb;

    struct bulk_pool_id id = {
        .machine = t->pool_machine_id,
        .local = t->pool_local_id,
        .dom = t->pool_domain_id, };

    struct bulk_buffer *buf = get_buffer(p->channel, &id, t->buffer_id);

    assert(buf);

    uint32_t local_id = ((lvaddr_t) buf->address - buf->pool->base_address)
                    / buf->pool->buffer_size;

    set_remote_bufid(buf->pool, local_id, t->buffer_id);

    err = bulk_buffer_change_state(buf, BULK_BUFFER_READ_WRITE);
    assert(!err_is_fail(err));

    rb = msg->parts[1].opaque;

    p->channel->callbacks->buffer_received(p->channel, buf, rb->virt);

    bulk_net_transfer_free_rb(&p->net_ctrl, rb);
}

/* ----------------------------- release operation ------------------------- */

static void send_buffer_release(struct bulk_net_nocopy *p,
                                struct bulk_buffer *b,
                                void *meta,
                                struct bulk_continuation cont)
{
    BNT_DEBUG_TRACE

    errval_t err;
    struct proto_trail_release *t;
    struct transmit_buffer *tb;
    struct bulk_net_msgdesc msg;

    assert(p->bulk_control == p);

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

    uint32_t local_id = ((lvaddr_t) b->address - b->pool->base_address)
                    / b->pool->buffer_size;
    t->buffer_id = get_remote_bufid(b->pool, local_id);

    msg.parts[1].phys = tb->int_phys;
    msg.parts[1].size = sizeof(*t) + p->channel->meta_size;
    msg.parts[1].opaque = tb;
    msg.parts[2].size = 0;

    bulk_net_transfer_add_header(&msg);
    err = bulk_e10k_send(&p->net_ctrl.transfer, &msg);
    assert(err_is_ok(err));
}

static void handle_buffer_release(struct bulk_net_nocopy *p,
                                  struct proto_trail_release *t,
                                  struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    assert(p->bulk_control == p);

    errval_t err;
    struct receive_buffer *rb;

    struct bulk_pool_id id = {
        .machine = t->pool_machine_id,
        .local = t->pool_local_id,
        .dom = t->pool_domain_id, };

    struct bulk_buffer *buf = get_buffer(p->channel, &id, t->buffer_id);

    assert(buf);

    uint32_t local_id = ((lvaddr_t) buf->address - buf->pool->base_address)
                    / buf->pool->buffer_size;

    set_remote_bufid(buf->pool, local_id, t->buffer_id);

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

}

static void handle_status_msg(struct bulk_net_nocopy *p,
                              struct proto_trail_status *t,
                              struct bulk_net_msgdesc *msg)
{

}

/* ------------------------ network managements ---------------------------- */
static void tcb_transmitted(struct bulk_e10k *bu, void *opaque)
{
    BNT_DEBUG_TRACE

    struct bulk_net_nocopy *p = bu->opaque;
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

    stack_alloc_free(&p->net_ctrl.tb_stack, tb);
}

static void tcb_received(struct bulk_e10k* bu, struct bulk_net_msgdesc *msg)
{
    BNT_DEBUG_TRACE

    struct bulk_net_nocopy *p = bu->opaque;
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
    BNT_DEBUG_TRACE

    errval_t err;

    BNT_STATUS("Creating new bulk channel [%p] using net.no-copy backend",
               channel);

    struct bulk_net_nocopy *p = calloc(1, sizeof(struct bulk_net_nocopy));
    if (p == NULL) {
        return BULK_TRANSFER_MEM;
    }

    struct bulk_net_endpoint_descriptor *ep =
        (struct bulk_net_endpoint_descriptor *) channel->ep;

    p->net_ctrl.card = ep->cardname;
    p->net_ctrl.l_port = ep->port;
    p->net_ctrl.queue = ep->queue;
    p->net_ctrl.ws = channel->waitset;
    p->net_ctrl.buffer_size = BULK_NET_CTRL_CHANNEL_BUF_SIZE;
    /* this is the control channel, has just two buffers */
    p->net_ctrl.buffer_count = ep->buffer_count;
    p->net_ctrl.max_queues = ep->max_queues;
    p->net_ctrl.num_queues = 1;

    p->bulk_control = p;

    err = bulk_net_transfer_export(&p->net_ctrl, tcb_transmitted, tcb_received);
    if (err_is_fail(err)) {
        free(p);
        return err;
    }

    p->net_ctrl.buffer_size = ep->buffer_size;

    channel->state = BULK_STATE_BINDING;
    p->channel = channel;
    channel->impl_data = p;

    return err;
}

static errval_t impl_channel_bind(struct bulk_channel *channel,
                                  struct bulk_continuation cont)
{
    BNT_DEBUG_TRACE

    errval_t err;

    BNT_STATUS("Binding new bulk channel [%p] using net.no-copy backend",
               channel);

    struct bulk_net_nocopy *bnt = calloc(1, sizeof(struct bulk_net_nocopy));
    if (!bnt) {
        return BULK_TRANSFER_MEM;
    }

    struct bulk_net_endpoint_descriptor *ep =
        (struct bulk_net_endpoint_descriptor *) channel->ep;

    bnt->net_ctrl.card = ep->cardname;
    bnt->net_ctrl.r_port = ep->port;
    bnt->net_ctrl.r_ip = ep->ip.addr;   ///XXX: IP already in network byte order
    bnt->net_ctrl.queue = ep->queue;
    bnt->net_ctrl.ws = channel->waitset;
    bnt->net_ctrl.buffer_size = BULK_NET_CTRL_CHANNEL_BUF_SIZE;
    /* this is the control channel, has just two buffers */
    bnt->net_ctrl.buffer_count = ep->buffer_count;
    bnt->net_ctrl.max_queues = ep->max_queues;
    bnt->net_ctrl.num_queues = 1;
    bnt->bulk_control = bnt;

    err = bulk_net_transfer_bind(&bnt->net_ctrl, tcb_transmitted, tcb_received);
    if (err_is_fail(err)) {
        free(bnt);
        return err;
    }

    bnt->net_ctrl.buffer_size = ep->buffer_size;

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
    BNT_DEBUG_TRACE

    errval_t err;
    struct bulk_net_nocopy *bnt, *p;
    struct bulk_pool_internal *pool_int = (struct bulk_pool_internal *) pool;

    p = get_net_nocopy(pool);
    if (p) {
        return BULK_TRANSFER_NET_POOL_USED;
    }

    bnt = (struct bulk_net_nocopy *) channel->impl_data;

    /* need to take the control channel for this */
    assert(bnt->bulk_control == bnt);

    if (bnt->net_ctrl.buffer_size != pool->buffer_size) {
        return BULK_TRANSFER_ALLOC_BUFFER_SIZE;
    }

    if (bnt->net_ctrl.num_queues == bnt->net_ctrl.max_queues) {
        return BULK_TRANSFER_NET_MAX_QUEUES;
    }

    uint8_t queueid = bnt->net_ctrl.queue + bnt->net_ctrl.num_queues;
    bnt->net_ctrl.num_queues++;

    /* allocate a new queue for this pool */
    p = calloc(1,
               sizeof(struct bulk_net_nocopy)
                               + pool->num_buffers
                                               * sizeof(struct receive_buffer));

    p->meta_rb = (struct receive_buffer *) (p + 1);
    err = bulk_net_init_meta_rb(p->meta_rb, pool->num_buffers,
                                pool->buffer_size);
    assert(!err_is_fail(err));

    memcpy(&p->net_ctrl, &bnt->net_ctrl, sizeof(p->net_ctrl));
    p->net_ctrl.queue = queueid;
    p->net_ctrl.buffer_count = 0;

    err = bulk_net_transfer_bind(&p->net_ctrl, tcb_transmitted, tcb_received);
    if (err_is_fail(err)) {
        free(p);
        return err;
    }

    p->channel = channel;
    p->pool = pool;
    p->bulk_control = bnt;
    p->bind_cont = cont;

    assert(!pool_int->impl_data);

    size_t pd_size = sizeof(struct bulk_net_pool_data)
                    + 2 * pool->num_buffers * sizeof(uint32_t);

    struct bulk_net_pool_data *pd = malloc(pd_size);

    if (!pd) {
        free(p);
        /* TODO: Free network resources */
        return BULK_TRANSFER_MEM;
    }

    pd->buf_id_local_to_remote = (uint32_t *) (pd + 1);
    pd->buf_id_remote_to_local =
        (pd->buf_id_local_to_remote + pool->num_buffers);
    for (uint32_t i = 0; i < pool->num_buffers; ++i) {
        pd->buf_id_local_to_remote[i] = i;
        pd->buf_id_remote_to_local[i] = i;
    }
    pd->p = p;
    pool_int->impl_data = pd;

    struct pending_pool_request *req = malloc(
                    sizeof(struct pending_pool_request));
    if (!req) {
        free(p);
        free(pd);
        /* TODO: free network resources */
        return BULK_TRANSFER_MEM;
    }

    req->cont = cont;
    req->pool = pool;
    req->bnt = p;
    if (bnt->pending_pool_requests) {
        req->next = bnt->pending_pool_requests->next;
    } else {
        req->next = NULL;
    }
    bnt->pending_pool_requests = req;

    send_pool_assign_request(bnt, pool, p->net_ctrl.l_port);

    return SYS_ERR_OK;
}

static errval_t impl_channel_move(struct bulk_channel *channel,
                                  struct bulk_buffer *buffer,
                                  void *meta,
                                  struct bulk_continuation cont)
{
    struct bulk_pool_internal *pool = (struct bulk_pool_internal *) buffer->pool;
    struct bulk_net_pool_data *pd = pool->impl_data;
    struct bulk_net_nocopy *bnt = pd->p;

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
    errval_t err;

    struct bulk_pool_internal *pool = (struct bulk_pool_internal *) buffer->pool;
    struct bulk_net_pool_data *pd = pool->impl_data;
    struct bulk_net_nocopy *bnt = pd->p;
    struct bulk_net_nocopy *p = channel->impl_data;

    if (channel->direction == BULK_DIRECTION_TX) {
        return BULK_TRANSFER_CHAN_DIRECTION;
    }

    assert(bnt != NULL);

    struct receive_buffer *rb;
    rb = stack_alloc_alloc(&bnt->net_ctrl.rb_stack);
    assert(rb != NULL);

    rb->virt = buffer->address;
    rb->phys = buffer->phys;
    rb->buffer = buffer;

    err = bulk_e10k_rx_add(&bnt->net_ctrl.transfer, rb->phys, rb->hdr_phys, rb);
    if (err_is_fail(err)) {
        return err;
    }
    uint32_t local_id = get_local_bufid(buffer);
    rb = bnt->meta_rb + local_id;
    err = bulk_e10k_rx_add(&bnt->net_ctrl.transfer, rb->phys, rb->hdr_phys, rb);
    if (err_is_fail(err)) {
        return err;
    }

    /* send the buffer pass over the control channel */
    send_buffer_pass(p, buffer, meta, cont);
    return SYS_ERR_OK;
}

static errval_t impl_channel_copy(struct bulk_channel *channel,
                                  struct bulk_buffer *buffer,
                                  void *meta,
                                  struct bulk_continuation cont)
{
    struct bulk_pool_internal *pool = (struct bulk_pool_internal *) buffer->pool;
    struct bulk_net_pool_data *pd = pool->impl_data;
    struct bulk_net_nocopy *bnt = pd->p;

    send_buffer_copy(bnt, buffer, meta, cont);
    return SYS_ERR_OK;
}

static errval_t impl_channel_release(struct bulk_channel *channel,
                                     struct bulk_buffer *buffer,
                                     struct bulk_continuation cont)
{
    errval_t err;

    struct bulk_pool_internal *pool = (struct bulk_pool_internal *) buffer->pool;
    struct bulk_net_pool_data *pd = pool->impl_data;
    struct bulk_net_nocopy *bnt = pd->p;
    struct bulk_net_nocopy *p = channel->impl_data;

    if (channel->direction == BULK_DIRECTION_TX) {
        return BULK_TRANSFER_CHAN_DIRECTION;
    }

    struct receive_buffer *rb;
    rb = stack_alloc_alloc(&bnt->net_ctrl.rb_stack);
    assert(rb != NULL);

    rb->virt = buffer->address;
    rb->phys = buffer->phys;
    rb->buffer = buffer;

    err = bulk_e10k_rx_add(&bnt->net_ctrl.transfer, rb->phys, rb->hdr_phys, rb);
    if (err_is_fail(err)) {
        return err;
    }
    uint32_t local_id = get_local_bufid(buffer);
    rb = bnt->meta_rb + local_id;
    err = bulk_e10k_rx_add(&bnt->net_ctrl.transfer, rb->phys, rb->hdr_phys, rb);
    if (err_is_fail(err)) {
        return err;
    }
    /* send the buffer pass over the control channel */
    send_buffer_release(p, buffer, bnt->zero_meta, cont);
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

struct bulk_implementation *bulk_net_get_impl_no_copy(void)
{
    return &bulk_net_implementation;
}

