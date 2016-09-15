/* Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/nameservice_client.h>
#include <devif/queue_interface.h>

#include <if/sfn5122f_defs.h>
#include <if/sfn5122f_devif_defs.h>
#include <if/sfn5122f_devif_rpcclient_defs.h>
#include <dev/sfn5122f_dev.h>
#include <dev/sfn5122f_q_dev.h>

#include "sfn5122f.h"
#include "sfn5122f_queue.h"
#include "sfn5122f_debug.h"
#include "helper.h"


static struct sfn5122f_queue* q_state;

/**  Misc             */
static errval_t update_rxtail(struct sfn5122f_queue* q, void *opaque, size_t tail)
{
    assert(q->device != NULL);
    uint64_t reg = 0;

    reg = sfn5122f_rx_desc_upd_reg_hi_rx_desc_wptr_insert(reg, tail);
    /* don't want to push an additional rx descriptor with the write pointer */
    reg = sfn5122f_rx_desc_upd_reg_hi_rx_desc_push_cmd_insert(reg, 0);
    /* the lower register will be ignored   */
    sfn5122f_rx_desc_upd_reg_lo_wr(q->device, q->id, 0);
    sfn5122f_rx_desc_upd_reg_hi_wr(q->device, q->id, reg);

    return SYS_ERR_OK;
}

static errval_t update_txtail(struct sfn5122f_queue* q, void *opaque, size_t tail)
{
    assert(q->device != NULL);
    uint64_t reg = 0;
    reg = sfn5122f_tx_desc_upd_reg_hi_tx_desc_wptr_insert(reg, tail);
    /* don't want to push an additional tx descriptor with the write pointer */
    reg = sfn5122f_tx_desc_upd_reg_hi_tx_desc_push_cmd_insert(reg, 0);
    reg = sfn5122f_tx_desc_upd_reg_hi_tx_desc_insert(reg, 0);

    /*  the lower register will be ignored  */
    sfn5122f_tx_desc_upd_reg_lo_wr(q->device, q->id, 0);
    sfn5122f_tx_desc_upd_reg_hi_wr(q->device, q->id, reg);
    return SYS_ERR_OK;
}

static void bind_cb(void *st, errval_t err, struct sfn5122f_devif_binding *b)
{
    struct devq* q = (struct devq*) st;
    assert(err_is_ok(err));
    
    struct sfn5122f_queue* queue = devq_get_state(q);
    b->st = queue;
    // Initi RPC client
    
    queue->b = b;
    queue->rpc = malloc(sizeof(struct sfn5122f_devif_rpc_client));
    assert(queue->rpc != NULL);

    err = sfn5122f_devif_rpc_client_init(queue->rpc, b);
    if (err_is_fail(err)) {
       free(queue->rpc);
    }   
 
    queue->bound = true;   
}

/**
 * Devif functions
 *
 */
static errval_t sfn5122f_create(struct devq* q, uint64_t flags)
{
    errval_t err;
    struct capref tx_frame, rx_frame, ev_frame;
    size_t tx_size, rx_size, ev_size;
    void *tx_virt, *rx_virt, *ev_virt;
    struct sfn5122f_queue* queue;
    struct frame_identity id;

    struct sfn5122f_queue_ops ops = {
        .update_txtail = update_txtail,
        .update_rxtail = update_rxtail
     };
   
    /* Allocate memory for descriptor rings  
       No difference for userspace networking*/
    tx_size = sfn5122f_q_tx_ker_desc_size * TX_ENTRIES;
    tx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, tx_size, &tx_frame);
    if (tx_virt == NULL) {
        return SFN_ERR_ALLOC_QUEUE;
    }

    rx_size = sfn5122f_q_rx_user_desc_size * RX_ENTRIES;
    rx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, rx_size, &rx_frame);
    if (rx_virt == NULL) {
        return SFN_ERR_ALLOC_QUEUE;
    }

    ev_size = sfn5122f_q_event_entry_size * EV_ENTRIES;
    ev_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, ev_size, &ev_frame);
    if (ev_virt == NULL) {
        return SFN_ERR_ALLOC_QUEUE;
    }

    // Init queue
    queue = sfn5122f_queue_init(tx_virt, TX_ENTRIES, rx_virt, RX_ENTRIES,
                                ev_virt, EV_ENTRIES, &ops,  NULL, false);

    queue->bound = false;
    devq_set_state(q, queue);


    iref_t iref;
    const char *name = "sfn5122f_sfn5122fmng_devif";

    // Connect to solarflare card driver
    err = nameservice_blocking_lookup(name, &iref);
    if (err_is_fail(err)) {
        return err;
    }

    err = sfn5122f_devif_bind(iref, bind_cb, q, get_default_waitset(),
                              1);

    if (err_is_fail(err)) {
        return err;
    }

    // wait until bound
    while(!queue->bound) {
        event_dispatch(get_default_waitset());
    }

    errval_t err2;
    struct capref regs;
    // Inform card driver about new queue and get the registers/queue id
    err = queue->rpc->vtbl.create_queue(queue->rpc, rx_frame, tx_frame, ev_frame,
                                        &queue->id, &regs, &err2);
    if (err_is_fail(err) || err_is_fail(err2)) {
        err = err_is_fail(err) ? err: err2;
        return err;
    }

    void* va;

    err = invoke_frame_identify(regs, &id);
    if (err_is_fail(err)) {
        return err;
    }

    err = vspace_map_one_frame_attr(&va, id.bytes, regs, 
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }
  
    queue->device = malloc(sizeof(sfn5122f_t));
    sfn5122f_initialize(queue->device, va);

    q_state = queue;
    return SYS_ERR_OK;
}


// Don't have to do antything for register
static errval_t sfn5122f_register(struct devq* q, struct capref cap,
                                  regionid_t rid) 
{
    return SYS_ERR_OK;
}

static errval_t sfn5122f_deregister(struct devq* q, regionid_t rid) 
{
    return SYS_ERR_OK;
}


static errval_t sfn5122f_control(struct devq* q, uint64_t cmd, uint64_t value)
{
    return SYS_ERR_OK;
}

static errval_t enqueue_rx_buf(struct sfn5122f_queue* q, regionid_t rid,
                               bufferid_t bid, lpaddr_t base, size_t len, 
                               uint64_t flags)
{

    DEBUG_QUEUE("Enqueueing RX buf \n");
    // check if there is space
    if (sfn5122f_queue_free_rxslots(q) == 0) {
        printf("SFN5122F_%d: Not enough space in RX ring, not adding buffer\n", 
                q->id);
        return SFN_ERR_ENQUEUE;
    }
    
    DEBUG_QUEUE("RX_BUF addr=%lu flags=%lu \n", 
                base, flags);
    sfn5122f_queue_add_rxbuf_devif(q, rid, bid, base, len, flags);
    sfn5122f_queue_bump_rxtail(q);
    return SYS_ERR_OK;
}


static errval_t enqueue_tx_buf(struct sfn5122f_queue* q, regionid_t rid,
                               bufferid_t bid, lpaddr_t base, size_t len, 
                               uint64_t flags)
{

    DEBUG_QUEUE("Enqueueing TX buf \n");
    // check if there is space
    if (sfn5122f_queue_free_txslots(q) == 0) {
        printf("SFN5122F_%d: Not enough space in RX ring, not adding buffer\n", 
                q->id);
        return SFN_ERR_ENQUEUE;
    }
    
    DEBUG_QUEUE("TX_BUF addr=%lu flags=%lu \n", 
                base, flags);
    sfn5122f_queue_add_txbuf_devif(q, rid, bid, base, len, flags);
    sfn5122f_queue_bump_txtail(q);
    return SYS_ERR_OK;
}

static errval_t sfn5122f_notify(struct devq* q, uint8_t num_slots)
{
    errval_t err;
    regionid_t rid;
    bufferid_t bid;
    lpaddr_t addr;
    size_t len;
    uint64_t flags;

    struct sfn5122f_queue* queue = devq_get_state(q);

    for (int i = 0; i < num_slots; i++) {
        err = devq_dequeue(q, &rid, &addr, &len, &bid, &flags);
        if (err_is_fail(err)) {
            return err;
        }

        if (flags & DEVQ_BUF_FLAG_RX) {
            err = enqueue_rx_buf(queue, rid, bid, addr, len, flags);
            if (err_is_fail(err)) {
                return err;
            }      
        } else if (flags & DEVQ_BUF_FLAG_TX) {
            err = enqueue_tx_buf(queue, rid, bid, addr, len, flags);
            if (err_is_fail(err)) {
                return err;
            }      
        }   
    }

    return SYS_ERR_OK;
}

static errval_t sfn5122f_destroy(struct devq* q)
{
    return SYS_ERR_OK;
}

/**
 * Main loop
 *
 */

int main(int argc, char **argv)
{
    struct waitset *ws;
    errval_t err;

    struct devq_func_pointer f = {
        .create = sfn5122f_create,
        .reg = sfn5122f_register,
        .notify = sfn5122f_notify,
        .dereg = sfn5122f_deregister,
        .ctrl = sfn5122f_control,
        .destroy = sfn5122f_destroy,
    };

    struct endpoint_state my_state = {
        .endpoint_type = ENDPOINT_TYPE_DEVICE,
        .features = 0,
        .f = f,
    };
    
    sprintf(my_state.device_name, "sfn5122f_devif_qdriver_%d", disp_get_core_id());
 
    err = devq_driver_export(&my_state);
    if (err_is_fail(err)){  
        abort();
    }
    
    ws = get_default_waitset();
    while (true) {
        event_dispatch_non_block(ws);
    }
}
