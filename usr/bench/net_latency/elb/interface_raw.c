/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "elb.h"

#include <barrelfish/net_constants.h>
#include <if/net_queue_manager_defs.h>
#include <barrelfish/bulk_transfer_arch.h>
#include <procon/procon.h>

#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have
#define BUFFER_SIZE 2048


static void idc_register_buffer(struct net_queue_manager_binding *binding,
                                struct capref buf, struct capref sp,
                                uint64_t qid, uint64_t slots, uint8_t role);

static void idc_raw_add_buffer(struct net_queue_manager_binding *binding,
                               uint64_t offset, uint64_t len);

static uint64_t queue = 0;

static struct net_queue_manager_binding *binding_rx = NULL;
static uint64_t bufid_rx = -1ULL;

static struct net_queue_manager_binding *binding_tx = NULL;
static uint64_t bufid_tx = -1ULL;

static struct capref buffer_frame;
void *buffer_base = NULL;
size_t buffer_size = 2048;

/******************************************************************************/
/* Buffer management */

void buffer_tx_add(size_t idx, size_t len)
{
    idc_raw_add_buffer(binding_tx, idx * BUFFER_SIZE, len);
}

void buffer_rx_add(size_t idx)
{
    idc_raw_add_buffer(binding_rx, idx * BUFFER_SIZE, BUFFER_SIZE);
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
    struct waitset *ws = get_default_waitset();

    alloc_mem(&buffer_frame, &buffer_base, BUFFER_SIZE * count);

    idc_register_buffer(binding_rx, buffer_frame, NULL_CAP, queue, count,
                        RX_BUFFER_ID);
    while (bufid_rx == -1ULL) { event_dispatch(ws); }

    idc_register_buffer(binding_tx, buffer_frame, NULL_CAP, queue, count,
                        TX_BUFFER_ID);
    while (bufid_tx == -1ULL) { event_dispatch(ws); }
}


/******************************************************************************/
/* Flounder interface */

static void idc_raw_add_buffer(struct net_queue_manager_binding *binding,
                               uint64_t offset, uint64_t len)
{
    errval_t err;
    err = net_queue_manager_raw_add_buffer__tx(binding, NOP_CONT, offset, len);
    assert(err_is_ok(err));
}

static void idc_register_buffer(struct net_queue_manager_binding *binding,
                                struct capref buf, struct capref sp,
                                uint64_t qid, uint64_t slots, uint8_t role)
{
    errval_t err;
    err = net_queue_manager_register_buffer__tx(binding, NOP_CONT, buf, sp,
                                                queue, slots, role);
    assert(err_is_ok(err));
}

static void new_buffer_id(struct net_queue_manager_binding *st, errval_t err,
                          uint64_t queueid, uint64_t buffer_id)
{
    printf("new_buffer_id(%"PRIu64")\n", buffer_id);

    assert(err_is_ok(err));

    if (st == binding_rx) {
        bufid_rx = buffer_id;
    } else {
        bufid_tx = buffer_id;
    }
}

static void raw_xmit_done(struct net_queue_manager_binding *st,
                          uint64_t offset, uint64_t len)
{
    size_t idx = offset / BUFFER_SIZE;

    if (st == binding_rx) {
        benchmark_rx_done(idx, len);
    } else {
        benchmark_tx_done(idx);
    }
}

static struct net_queue_manager_rx_vtbl rx_vtbl = {
    .new_buffer_id = new_buffer_id,
    .raw_xmit_done = raw_xmit_done,
};

static void bind_cb(void *st, errval_t err, struct net_queue_manager_binding *b)
{
    assert(err_is_ok(err));

    b->rx_vtbl = rx_vtbl;

    if (binding_rx == NULL) {
        binding_rx = b;
    } else {
        binding_tx = b;
    }
}

static void connect_to_driver(const char *cname, uint64_t qid)
{
    errval_t err;
    iref_t iref;
    char qm_name[MAX_SERVICE_NAME_LEN] = { 0 };

    snprintf(qm_name, sizeof(qm_name), "%s_%"PRIu64, cname, qid);
    err = nameservice_blocking_lookup(qm_name, &iref);
    assert(err_is_ok(err));

    err = net_queue_manager_bind(iref, bind_cb, NULL, get_default_waitset(),
                                 IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));

}

void terminate_benchmark(void)
{
    vspace_unmap(buffer_base);
    cap_delete(buffer_frame);
    exit(-1);
}

static void process_cmdline(int argc, char* argv[])
{
    int i;
    for (i = 1; i < argc; i++) {
        benchmark_argument(argv[i]);
    }
}

static void eventloop(void)
{
    struct waitset *ws = get_default_waitset();

    while (1) {
        event_dispatch_non_block(ws);
        benchmark_do_pending_work();
    }
}

int main(int argc, char* argv[])
{
    struct waitset *ws = get_default_waitset();

    printf("elb_app: Started, v3\n");
    process_cmdline(argc, argv);

    char *cardname = get_cardname();
    if (cardname == NULL) {
        cardname = "e10k";
    }

    queue = get_cmdline_queueid();

    printf("Using [%s] as cardname and %"PRIu64"\n", cardname,
            queue);

    // Connect RX path
    connect_to_driver(cardname, queue);
    while (binding_rx == NULL) { event_dispatch(ws); }

    // Connect TX path
    connect_to_driver(cardname, queue);
    while (binding_tx == NULL) { event_dispatch(ws); }

    buffers_init(32);
    benchmark_init(32);

    eventloop();
}

