/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <net_interfaces/net_interfaces.h>

#include <barrelfish/net_constants.h>
#include <if/net_queue_manager_defs.h>

#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have
#define BUFFER_SIZE 2048
#define BUFFER_COUNT ((128*1024*1024) / BUFFER_SIZE)

#define QUEUE_SIZE 2048

static errval_t idc_raw_add_buffer(struct net_queue_manager_binding *binding,
                               uint64_t *queue, uint64_t offset, uint64_t len,
                               uint64_t more_chunks, uint64_t flags,
                               bool blocking);

static uint64_t queue = 0;
static uint64_t card_mac = -1ULL;

static struct net_queue_manager_binding *binding_rx = NULL;
static uint64_t bufid_rx = -1ULL;

static struct net_queue_manager_binding *binding_tx = NULL;
static uint64_t bufid_tx = -1ULL;

static struct capref buffer_frame;
void *buffer_base = NULL;
size_t buffer_size = 2048;
size_t buffer_count = BUFFER_COUNT;

static struct capref rx_queue_frame;
uint64_t *rx_queue_base = NULL;
static struct capref tx_queue_frame;
uint64_t *tx_queue_base = NULL;

static void init_queue(uint64_t *q)
{
    q[0] = 1;
    q[1] = 1;
    q[2] = 0;
    q[3] = 0;
}

static int put_to_queue(uint64_t *q, uint64_t v1, uint64_t v2, uint64_t v3, uint64_t v4)
{
    uint64_t start = q[0];
    uint64_t end = q[1];

    if (end == (QUEUE_SIZE - 1)) {
        assert(start > 1);
    } else {
        assert((end + 1) != start);
    }

    q[4 * end] = v1;
    q[4 * end + 1] = v2;
    q[4 * end + 2] = v3;
    q[4 * end + 3] = v4;
    bool s = start == end;
    if (end == (QUEUE_SIZE - 1))
        q[1] = 1;
    else
        q[1]++;
    end = q[1];
    bool f = (end == (QUEUE_SIZE - 1)) ? (start == 1): (start == end + 1);
    return s + 2 * f;
}

static bool check_queue(uint64_t *q)
{
    uint64_t start = q[0];
    uint64_t end = q[1];

    return start != end;
}

static bool get_from_queue(uint64_t *q, uint64_t *v1, uint64_t *v2, uint64_t *v3, uint64_t *v4)
{
    uint64_t start = q[0];
    uint64_t end = q[1];

    assert(start != end);
    *v1 = q[4 * start];
    *v2 = q[4 * start + 1];
    *v3 = q[4 * start + 2];
    *v4 = q[4 * start + 3];
    if (start == (QUEUE_SIZE - 1))
        q[0] = 1;
    else
        q[0]++;
    return !q[2];
}

static errval_t register_buffer(struct net_queue_manager_binding *b, struct capref buf, struct capref sp, uint64_t queueid, uint64_t slots, uint8_t role, struct capref queue_cap, uint64_t *idx)
{
    errval_t _err = SYS_ERR_OK;
    b->error = SYS_ERR_OK;
    thread_set_outgoing_token(thread_set_token(b->message_chanstate + net_queue_manager_register_buffer_response__msgnum));
    _err = b->tx_vtbl.register_buffer_call(b, BLOCKING_CONT, buf, sp, queueid, slots, role, queue_cap);
    if (err_is_fail(_err))
        goto out;
    _err = wait_for_channel(get_default_waitset(), b->message_chanstate + net_queue_manager_register_buffer_response__msgnum, &b->error);
    if (err_is_fail(_err))
        goto out;
    *idx = b->rx_union.register_buffer_response.idx;
    _err = b->receive_next(b);
out:
    thread_clear_token(b->get_receiving_chanstate(b));
    return(_err);
}

static errval_t get_mac_address(struct net_queue_manager_binding *b, uint64_t queueid, uint64_t *hwaddr)
{
    errval_t _err = SYS_ERR_OK;
    b->error = SYS_ERR_OK;
    thread_set_outgoing_token(thread_set_token(b->message_chanstate + net_queue_manager_get_mac_address_response__msgnum));
    _err = b->tx_vtbl.get_mac_address_call(b, BLOCKING_CONT, queueid);
    if (err_is_fail(_err))
        goto out;
    _err = wait_for_channel(get_default_waitset(), b->message_chanstate + net_queue_manager_get_mac_address_response__msgnum, &b->error);
    if (err_is_fail(_err))
        goto out;
    *hwaddr = b->rx_union.get_mac_address_response.hwaddr;
    _err = b->receive_next(b);
out:
    thread_clear_token(b->get_receiving_chanstate(b));
    return(_err);
}

/******************************************************************************/
/* Buffer management */

errval_t buffer_tx_add(size_t idx, size_t offset, size_t len,
                       size_t more_chunks, uint64_t flags)
{

    errval_t err = SYS_ERR_OK;
    err = idc_raw_add_buffer(binding_tx, tx_queue_base, idx * BUFFER_SIZE + offset, len,
            (uint64_t)more_chunks, flags, 0);
    return err;
}

errval_t buffer_rx_add(size_t idx)
{

    errval_t err = SYS_ERR_OK;
    err = idc_raw_add_buffer(binding_rx, rx_queue_base, idx * BUFFER_SIZE, BUFFER_SIZE, 0, 0, 0);
    return err;
}


static void alloc_mem(struct capref *frame, void** virt, size_t size)
{
    errval_t r;
    vregion_flags_t flags;

    r = frame_alloc(frame, size, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Allocating memory region frame failed!");
    }

    flags = VREGION_FLAGS_READ_WRITE;
    r = vspace_map_one_frame_attr(virt, size, *frame, flags, NULL, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Mapping memory region frame failed!");
    }
    memset(*virt, 0, size);
}

static void buffers_init(size_t count)
{
    alloc_mem(&buffer_frame, &buffer_base, BUFFER_SIZE * count);

    errval_t err;
    void *va;

    alloc_mem(&rx_queue_frame, &va, QUEUE_SIZE * 4 * sizeof(uint64_t) * 2);
    rx_queue_base = va;
    alloc_mem(&tx_queue_frame, &va, QUEUE_SIZE * 4 * sizeof(uint64_t) * 2);
    tx_queue_base = va;
    memset(rx_queue_base, 0, QUEUE_SIZE * 4 * sizeof(uint64_t) * 2);
    memset(tx_queue_base, 0, QUEUE_SIZE * 4 * sizeof(uint64_t) * 2);
    init_queue(rx_queue_base);
    init_queue(rx_queue_base + QUEUE_SIZE * 4);
    init_queue(tx_queue_base);
    init_queue(tx_queue_base + QUEUE_SIZE * 4);

    err = register_buffer(binding_rx, buffer_frame, NULL_CAP, queue, count,
        RX_BUFFER_ID, rx_queue_frame, &bufid_rx);
    assert(err_is_ok(err));
    err = register_buffer(binding_tx, buffer_frame, NULL_CAP, queue, count,
        TX_BUFFER_ID, tx_queue_frame, &bufid_tx);
    assert(err_is_ok(err));
}


/******************************************************************************/
/* Flounder interface */

static errval_t idc_raw_add_buffer(struct net_queue_manager_binding *binding, uint64_t *q,
                               uint64_t offset, uint64_t len,
                               uint64_t more_chunks, uint64_t flags, bool blocking)
{
    int r;
    r = put_to_queue(q, offset, len, more_chunks, flags);
    if (r) {
        if (r == 2) {//} || binding == binding_tx) {
            binding->control(binding, IDC_CONTROL_SET_SYNC);
        }
        errval_t err = binding->tx_vtbl.raw_add_buffer(binding, BLOCKING_CONT, offset, len, more_chunks, flags);
        assert(err_is_ok(err));
        if (r == 2) {//} || binding == binding_tx)
            binding->control(binding, IDC_CONTROL_CLEAR_SYNC);
        }
    }
    return SYS_ERR_OK;
}


// Returns the bufferid for specified type (RX, TX)
uint64_t get_rx_bufferid(void)
{
    return bufid_rx;
}

uint64_t get_tx_bufferid(void)
{
    return bufid_tx;
}

static void raw_xmit_done(struct net_queue_manager_binding *st,
                          uint64_t offset, uint64_t len, uint64_t more,
                          uint64_t flags)
{
    if (st == binding_rx) {
        for (;;) {
            bool c = check_queue(rx_queue_base + QUEUE_SIZE * 4);
            if (c) {
                get_from_queue(rx_queue_base + QUEUE_SIZE * 4, &offset, &len, &more, &flags);
                size_t idx = offset / BUFFER_SIZE;
                benchmark_rx_done(idx, len, more, flags);
            } else
                break;
        }
    } else {
        for (;;) {
            bool c = check_queue(tx_queue_base + QUEUE_SIZE * 4);
            if (c) {
                get_from_queue(tx_queue_base + QUEUE_SIZE * 4, &offset, &len, &more, &flags);
                size_t idx = offset / BUFFER_SIZE;
                benchmark_tx_done(idx);
            } else
                break;
        }
    }
}

static struct net_queue_manager_rx_vtbl rx_vtbl = {
    .raw_xmit_done = raw_xmit_done,
};


static void bind_cb_rx(void *st, errval_t err, struct net_queue_manager_binding *b)
{
    assert(err_is_ok(err));

    b->rx_vtbl = rx_vtbl;
    b->control(b, IDC_CONTROL_CLEAR_SYNC);
    binding_rx = b;
}

static void bind_cb_tx(void *st, errval_t err, struct net_queue_manager_binding *b)
{
    assert(err_is_ok(err));

    b->rx_vtbl = rx_vtbl;
    b->control(b, IDC_CONTROL_CLEAR_SYNC);
    binding_tx = b;
}


static void connect_to_driver(const char *cname, uint64_t qid, bool isRX, struct waitset *ws)
{
    errval_t err;
    iref_t iref;
    char qm_name[MAX_SERVICE_NAME_LEN] = { 0 };

    snprintf(qm_name, sizeof(qm_name), "%s_%"PRIu64"", cname, qid);
    err = nameservice_blocking_lookup(qm_name, &iref);
    assert(err_is_ok(err));

    err = net_queue_manager_bind(iref, isRX ? bind_cb_rx: bind_cb_tx, NULL, ws,
            IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));
}

void net_if_init(const char* cardname, uint64_t qid)
{
    static bool initialized = false;
    struct waitset *ws = get_default_waitset();

    // Only initialize once
    if (initialized) {
        return;
    }

    queue = qid;

    // Connect RX path
    connect_to_driver(cardname, queue, true, ws);
    // Connect TX path
    connect_to_driver(cardname, queue, false, ws);

    while (binding_rx == NULL  || binding_tx == NULL) {
        event_dispatch(ws);
    }

    buffers_init(BUFFER_COUNT);

    // Get MAC address
    errval_t err = get_mac_address(binding_rx, queue, &card_mac);
    assert(err_is_ok(err));

    initialized = true;
}

void net_if_terminate(void)
{
    vspace_unmap(buffer_base);
    cap_delete(buffer_frame);
}

void benchmark_get_mac_address(uint8_t *mac)
{
    memcpy(mac, &card_mac, 6);
}
