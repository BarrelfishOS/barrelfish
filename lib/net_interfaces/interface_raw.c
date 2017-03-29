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
#include <devif/queue_interface.h>
#include <devif/backends/descq.h>
#include <devif/backends/net/sfn5122f_devif.h>
#include <devif/backends/net/e10k_devif.h>

#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have
#define BUFFER_SIZE 2048
#define BUFFER_COUNT ((128*1024*1024) / BUFFER_SIZE)

static uint64_t queue_id = 0;
static uint64_t card_mac = -1ULL;

static struct descq *devq_rx = NULL;
static uint64_t bufid_rx;
static regionid_t regid_rx;

static struct descq *devq_tx = NULL;
static uint64_t bufid_tx;
static regionid_t regid_tx;

static struct devq *devq_direct = NULL;
static regionid_t regid_direct;
static bool direct = false;
static bool reg = false;


static struct capref buffer_frame;
void *buffer_base = NULL;
size_t buffer_size = 2048;
size_t buffer_count = BUFFER_COUNT;


/******************************************************************************/
/* Buffer management */

errval_t buffer_tx_add(size_t idx, size_t offset, size_t length,
                       size_t more_chunks, uint64_t flags)
{
    errval_t err;
    
    offset += idx * BUFFER_SIZE;
    if (!direct) {
        err = devq_enqueue((struct devq *)devq_tx, regid_tx, offset, length, 0, 0, flags);
    } else {
        flags = 0;
        err = devq_enqueue((struct devq *)devq_direct, regid_direct, offset, 
                           length, 0, length, (flags | NETIF_TXFLAG | NETIF_TXFLAG_LAST));
    }
    return err;
}

errval_t buffer_rx_add(size_t idx)
{
    errval_t err;
    size_t offset;
    
    offset = idx * BUFFER_SIZE;
    if (!direct) {
        err = devq_enqueue((struct devq *)devq_rx, regid_rx, offset, BUFFER_SIZE, 0, 0, 0);
    } else {
        uint64_t flags = 0;
        err = devq_enqueue((struct devq *)devq_direct, regid_direct, offset, BUFFER_SIZE, 
                           0, BUFFER_SIZE, flags | NETIF_RXFLAG);
    }
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
    errval_t err;
    alloc_mem(&buffer_frame, &buffer_base, BUFFER_SIZE * count);

    if (!direct) { 
        err = devq_register((struct devq *)devq_rx, buffer_frame, &regid_rx);
        assert(err_is_ok(err));
        err = devq_register((struct devq *)devq_tx, buffer_frame, &regid_tx);
        assert(err_is_ok(err));
    } else {
        err = devq_register((struct devq *)devq_direct, buffer_frame, &regid_direct);
        assert(err_is_ok(err));
        reg = true;
    }
}


/******************************************************************************/
/* Flounder interface */

// Returns the bufferid for specified type (RX, TX)
uint64_t get_rx_bufferid(void)
{
    return bufid_rx;
}

uint64_t get_tx_bufferid(void)
{
    return bufid_tx;
}

static errval_t notify_rx(struct descq *queue)
{
    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;

    for (;;) {
        errval_t err;
        err = devq_dequeue((struct devq *)queue, &rid, &offset, &length,
                           &valid_data, &valid_length, &flags);
        if (err_is_fail(err))
            break;
        size_t idx = offset / BUFFER_SIZE;
        benchmark_rx_done(idx, length, 0/*more*/, flags);
    }
    return SYS_ERR_OK;
}

static errval_t notify_tx(struct descq *queue)
{
    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;

    for (;;) {
        errval_t err;
        err = devq_dequeue((struct devq *)queue, &rid, &offset, &length,
                           &valid_data, &valid_length, &flags);
        if (err_is_fail(err))
            break;
        size_t idx = offset / BUFFER_SIZE;
        benchmark_tx_done(idx);
    }
    return SYS_ERR_OK;
}

static void connect_to_driver(const char *cname, uint64_t qid, bool isRX, struct waitset *ws)
{
    errval_t err;
    char qm_name[MAX_SERVICE_NAME_LEN] = { 0 };

    snprintf(qm_name, sizeof(qm_name), "%s_%"PRIu64"", cname, qid);
    debug_printf("%s: nqm bind [%s]\n", __func__, qm_name);

    struct descq_func_pointer f;
    f.notify = isRX ? notify_rx: notify_tx;
    
    err = descq_create(isRX ? &devq_rx: &devq_tx, DESCQ_DEFAULT_SIZE, qm_name,
                       false, true, !isRX, isRX ? &bufid_rx: &bufid_tx, &f);
    assert(err_is_ok(err));
}

static void int_handler(void* args)
{
    regionid_t rid;
    genoffset_t offset;
    genoffset_t length;
    genoffset_t valid_data;
    genoffset_t valid_length;
    uint64_t flags;

    for (;;) {
        errval_t err;
        err = devq_dequeue(devq_direct, &rid, &offset, &length,
                           &valid_data, &valid_length, &flags);
        if (err_is_fail(err))
            break;

        size_t idx = offset / BUFFER_SIZE;
        if (flags & NETIF_TXFLAG) {
            benchmark_tx_done(idx);
        } else if (flags & NETIF_RXFLAG) {
            assert(valid_length > 0);
            benchmark_rx_done(idx, valid_length, 0/*more*/, flags);
        }
    }

}

void net_if_init(const char* cardname, uint64_t qid)
{
    errval_t err;
    static bool initialized = false;
    struct waitset *ws = get_default_waitset();

    // Only initialize once
    if (initialized) {
        return;
    }

    queue_id = qid;

    // Connect RX path
    if ((strcmp(cardname, "e1000") == 0) || (qid == 0)) {
        connect_to_driver(cardname, queue_id, true, ws);
        // Connect TX path
        connect_to_driver(cardname, queue_id, false, ws);
    } else if ((strcmp(cardname, "e10k") == 0) && (qid != 0)) {
        USER_PANIC("e10k queue NIY \n");
        /*
        direct = true;
        struct e10k_queue* e10k;
        err = e10k_queue_create(&e10k, int_handler, false, false);
        assert(err_is_ok(err));

        devq_direct = (struct devq*) e10k; 
        card_mac = 0x1; // TODO 
        */
    } else if ((strcmp(cardname, "sfn5122f") == 0) && qid != 0) {
        direct = true;
        struct sfn5122f_queue* sfn5122f;
        err = sfn5122f_queue_create(&sfn5122f, int_handler, false, true);
        assert(err_is_ok(err));

        devq_direct = (struct devq*) sfn5122f; 
        card_mac = 0x000f530748d4; // TODO 
    } else {
        USER_PANIC("Unknown card name \n");
    }

    buffers_init(BUFFER_COUNT);

    // Get MAC address
    if (!direct) {
        err = devq_control((struct devq *)devq_rx, 0, 0, &card_mac);
        assert(err_is_ok(err));
    }

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
