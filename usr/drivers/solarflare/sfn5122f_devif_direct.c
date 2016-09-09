/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <devif/queue_interface.h>
#include <devif/sfn5122f_devif_direct.h>
#include <if/sfn5122f_devif_defs.h>
#include <if/sfn5122f_devif_rpcclient_defs.h>
#include "sfn5122f.h"
#include "sfn5122f_queue.h"
#include "sfn5122f_debug.h"
#include "helper.h"

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
    DEBUG_QUEUE("Bound to interface \n");
}


errval_t sfn5122f_create_direct(struct devq* q, uint64_t flags)
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
                                ev_virt, EV_ENTRIES, &ops,  NULL, true);

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
                              IDC_BIND_FLAGS_DEFAULT);
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
  
    sfn5122f_initialize(queue->device, va);

    return SYS_ERR_OK;
}

errval_t sfn5122f_register_direct(struct devq* q, struct capref cap,
                                  regionid_t rid) 
{
    uint64_t buftbl_idx = 0;
    errval_t err, err2;
    struct frame_identity id;

    struct sfn5122f_queue* queue = devq_get_state(q);
    err = queue->rpc->vtbl.register_region(queue->rpc, queue->id, cap, 
                                           &buftbl_idx, &err2);
    if (err_is_fail(err) || err_is_fail(err2)) {
        err = err_is_fail(err) ? err: err2;
        return err;
    }

    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) {
        return err;
    }


    // Setup datastructure for translating region ID to buffer table entry id
    // Currently only a linked list
    struct region_entry* entry = malloc(sizeof(struct region_entry));
    entry->rid = rid;
    entry->phys = id.base;
    entry->size = id.bytes;
    entry->cap = cap;
    entry->buftbl_idx = buftbl_idx;
    entry->next = NULL;
    
    struct region_entry* cur = queue->regions;

    if (cur == NULL) {
        queue->regions = entry;
    }

    while (cur->next != NULL) {
        cur = cur->next;
    }  
    
    cur->next = entry;
    
    return SYS_ERR_OK;
}

errval_t sfn5122f_deregister_direct(struct devq* q, regionid_t rid) 
{
    errval_t err, err2;
    struct sfn5122f_queue* queue = devq_get_state(q);

    // find region and translate to buftlb entry 
    struct region_entry* cur = queue->regions;

    if (cur == NULL) {
        return SFN_ERR_DEREGISTER_REGION;
    }

    while (cur->next != NULL && cur->rid != rid) {
        cur = cur->next;
    }  
   
    // do rpc do inform carddriver to remove buftbl entries
    err = queue->rpc->vtbl.deregister_region(queue->rpc, cur->buftbl_idx, cur->size,
                                             &err2);
    if (err_is_fail(err) || err_is_fail(err2)) {
        err = err_is_fail(err) ? err: err2;
        return err;
    }

    return SYS_ERR_OK;
}


errval_t sfn5122f_control_direct(struct devq* q, uint64_t cmd, uint64_t value)
{
    return SYS_ERR_OK;
}


errval_t sfn5122f_notify_direct(struct devq* q, uint8_t num_slots)
{
    return SYS_ERR_OK;
}

errval_t sfn5122f_destroy_direct(struct devq* q)
{
    return SYS_ERR_OK;
}

static errval_t enqueue_rx_buf(struct sfn5122f_queue* q, regionid_t rid,
                               bufferid_t bid, lpaddr_t base, size_t len, 
                               uint64_t flags)
{
    // check if there is space
    if (sfn5122f_queue_free_rxslots(q) == 0) {
        printf("SFN5122F_%d: Not enough space in RX ring, not adding buffer\n", 
                q->id);
        return SFN_ERR_ENQUEUE;
    }

    // find region
    struct region_entry* entry = q->regions;
    
    while((entry->next != NULL) && (entry->rid != rid)) {
        entry = entry->next;
    }
    
    if (entry == NULL) {
        return SFN_ERR_ENQUEUE;
    }
    
    // compute buffer table entry of the rx buffer and the within it offset
    uint64_t buftbl_idx = entry->buftbl_idx + (bid/BUF_SIZE);
    uint16_t offset = bid & 0x0000DFFF;    

    sfn5122f_queue_add_user_rxbuf_devif(q, buftbl_idx, offset,
                                        rid, bid, base, len, flags);
    sfn5122f_queue_bump_rxtail(q);
    return SYS_ERR_OK;
}


static errval_t enqueue_tx_buf(struct sfn5122f_queue* q, regionid_t rid,
                               bufferid_t bid, lpaddr_t base, size_t len, 
                               uint64_t flags)
{
    // check if there is space
    if (sfn5122f_queue_free_txslots(q) == 0) {
        printf("SFN5122F_%d: Not enough space in TX ring, not adding buffer\n", 
                q->id);
        return SFN_ERR_ENQUEUE;
    }

    // find region
    struct region_entry* entry = q->regions;
    
    while((entry->next != NULL) && (entry->rid != rid)) {
        entry = entry->next;
    }
    
    if (entry == NULL) {
        return SFN_ERR_ENQUEUE;
    }
    
    // compute buffer table entry of the rx buffer and the within it offset
    uint64_t buftbl_idx = entry->buftbl_idx + (bid/BUF_SIZE);
    uint16_t offset = bid & 0x0000DFFF;    

    sfn5122f_queue_add_user_txbuf_devif(q, buftbl_idx, offset,
                                        rid, bid, base, len, flags);
    sfn5122f_queue_bump_txtail(q);
    return SYS_ERR_OK;
}


errval_t sfn5122f_enqueue_direct(struct devq* q, regionid_t rid, bufferid_t bid, 
                                 lpaddr_t base, size_t len, uint64_t flags)
{
    errval_t err;

    struct sfn5122f_queue* queue = devq_get_state(q);
    if (flags & DEVQ_BUF_FLAG_RX) {
        err = enqueue_rx_buf(queue, rid, bid, base, len, flags);
        if (err_is_fail(err)) {
            return err;
        }      
    } else if (flags & DEVQ_BUF_FLAG_TX) {
        err = enqueue_tx_buf(queue, rid, bid, base, len, flags);
        if (err_is_fail(err)) {
            return err;
        }      
    }

    return SYS_ERR_OK;
}

errval_t sfn5122f_dequeue_direct(struct devq* q, regionid_t* rid, bufferid_t* bid, 
                                 lpaddr_t* base, size_t* len, uint64_t* flags)
{
    uint8_t ev_code;
    bool exit = false;
    errval_t err;    
    
    struct sfn5122f_queue* queue = devq_get_state(q);

    while(!exit) {
        ev_code = sfn5122f_get_event_code(queue);
        switch(ev_code){
        case EV_CODE_RX:
            // TODO multiple packets
            err = sfn5122f_queue_handle_rx_ev_devif(queue, rid, bid, base,
                                                    len, flags);
            exit = true;  
            if (err_is_ok(err)) {
                DEBUG_QUEUE(" RX_EV Q_ID: %d len %ld \n", queue->id, *len);
            }
            sfn5122f_queue_bump_evhead(queue);
            break;
        case EV_CODE_TX:
            err = sfn5122f_queue_handle_tx_ev_devif(queue, rid, bid, base, 
                                                    len, flags);
            if (err_is_ok(err)) {
                DEBUG_QUEUE("TX EVENT OK %d \n", queue->id);               
            } else {
                DEBUG_QUEUE("TX EVENT ERR %d \n", queue->id);
            }

            exit = true;
            sfn5122f_queue_bump_evhead(queue);
            break;
        case EV_CODE_DRV:
            //DEBUG_QUEUE("DRIVER EVENT %d\n", qi);
            sfn5122f_handle_drv_ev(queue, queue->id);
            sfn5122f_queue_bump_evhead(queue);
            break;
        case EV_CODE_DRV_GEN:
            DEBUG_QUEUE("DRIVER GENERATED EVENT \n");
            sfn5122f_queue_bump_evhead(queue);
            break;
        case EV_CODE_USER:
            DEBUG_QUEUE("USER EVENT \n");
            sfn5122f_queue_bump_evhead(queue);
            break;
        case EV_CODE_MCDI:
            //DEBUG_QUEUE("MCDI EVENT \n");
            sfn5122f_queue_handle_mcdi_event(queue);
            sfn5122f_queue_bump_evhead(queue);
            break;
        case EV_CODE_GLOBAL:
            DEBUG_QUEUE("GLOBAL EVENT \n");
            sfn5122f_queue_bump_evhead(queue);
            break;
        }
        sfn5122f_evq_rptr_reg_wr(queue->device, queue->id, 
                                 queue->ev_head);
    }

    return err;
}

