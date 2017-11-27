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
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/deferred.h>
#include <barrelfish/nameservice_client.h>
#include <devif/queue_interface.h>
#include <pci/pci.h>
#include <if/sfn5122f_devif_defs.h>
#include <if/sfn5122f_devif_defs.h>
#include <devif/backends/net/sfn5122f_devif.h>
#include <devif/queue_interface_backend.h>
#include "hw_queue.h"
#include "helper.h"


//#define DEBUG_SFN
#ifdef DEBUG_SFN
    #define DEBUG_QUEUE(x...) printf("sfn5122f_q : " x)
#else
    #define DEBUG_QUEUE(x...) do {} while (0)
#endif

//#define DELAY 1

// TX Queue
#define TX_ENTRIES 4096
#define RX_ENTRIES 4096
#define EV_ENTRIES 32768

STATIC_ASSERT((TX_ENTRIES & (TX_ENTRIES - 1)) == 0, "must be a power of two");
STATIC_ASSERT((RX_ENTRIES & (RX_ENTRIES - 1)) == 0, "must be a power of two");
STATIC_ASSERT((EV_ENTRIES & (EV_ENTRIES - 1)) == 0, "must be a power of two");


// Event Queue
#define EV_CODE_RX 0
#define EV_CODE_TX 2
#define EV_CODE_DRV 5
#define EV_CODE_DRV_GEN 7
#define EV_CODE_USER 8
#define EV_CODE_MCDI 12
#define EV_CODE_GLOBAL 6
#define EV_CODE_NONE 15

#define BUF_SIZE 4096

/* for each TX/RX entry one entry plus an additonal 2 for mcdi completion
and link state events */

struct sfn5122f_queue* queues[1024];


/**  Misc             */
static errval_t update_rxtail(struct sfn5122f_queue* q, size_t tail)
{
    assert(q->device != NULL);
    uint64_t reg = 0;

    q->rx_batch_size++;

    if (q->rx_batch_size > 31) {
        /* Write to this register is very very expensive (2500 cycles +) 
           So we batch the updates together*/
        reg = sfn5122f_rx_desc_upd_reg_hi_rx_desc_wptr_insert(reg, tail);
        /* don't want to push an additional rx descriptor with the write pointer */
        reg = sfn5122f_rx_desc_upd_reg_hi_rx_desc_push_cmd_insert(reg, 0);
        /* the lower register will be ignored   */
        sfn5122f_rx_desc_upd_reg_lo_wr(q->device, q->id, 0);
        sfn5122f_rx_desc_upd_reg_hi_wr(q->device, q->id, reg);
        q->rx_batch_size = 0;
    }

    return SYS_ERR_OK;
}

static errval_t update_txtail(struct sfn5122f_queue* q, size_t tail)
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

static void interrupt_cb(struct sfn5122f_devif_binding *b, uint16_t qid)
{
    struct sfn5122f_queue* q = queues[qid];

    if (q != b->st) {
        debug_printf("STATE MISMATCH!\n %p %p\n", q, b->st);
        q = b->st;
    }

    q->cb(q);
}

static struct sfn5122f_devif_rx_vtbl rx_vtbl = {
    .interrupt = interrupt_cb,
};

static void bind_cb(void *st, errval_t err, struct sfn5122f_devif_binding *b)
{
    
    DEBUG_QUEUE("binding CB  \n");
    struct sfn5122f_queue* queue = (struct sfn5122f_queue*) st;
    b->st = queue;
    b->rx_vtbl = rx_vtbl;
    // Initi RPC client
    
    queue->b = b;
    sfn5122f_devif_rpc_client_init(queue->b);
    queue->bound = true;
}


static errval_t sfn5122f_register(struct devq* q, struct capref cap,
                                  regionid_t rid)
{
    uint64_t buftbl_idx = 0;
    errval_t err, err2;
    struct frame_identity id;

    struct sfn5122f_queue* queue = (struct sfn5122f_queue*) q;

    if (queue->userspace) {
        err = queue->b->rpc_tx_vtbl.register_region(queue->b, queue->id, cap,
                                                    &buftbl_idx, &err2);
        if (err_is_fail(err) || err_is_fail(err2)) {
            err = err_is_fail(err) ? err: err2;
            return err;
        }
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
        return SYS_ERR_OK;
    }

    while (cur->next != NULL) {
        cur = cur->next;
    }
    
    cur->next = entry;
    
    DEBUG_QUEUE("Region %d registered \n", rid);
    return SYS_ERR_OK;
}

static errval_t sfn5122f_deregister(struct devq* q, regionid_t rid)
{
    errval_t err, err2;
    
    struct sfn5122f_queue* queue = (struct sfn5122f_queue*) q;

    // find region and translate to buftlb entry
    struct region_entry* cur = queue->regions;

    if (cur == NULL) {
        return DEVQ_ERR_INVALID_REGION_ARGS;
    }

    while (cur->next != NULL && cur->rid != rid) {
        cur = cur->next;
    }
   
    // do rpc do inform carddriver to remove buftbl entries
    if (queue->userspace) {
        err = queue->b->rpc_tx_vtbl.deregister_region(queue->b, cur->buftbl_idx,
                                                      cur->size, &err2);
        if (err_is_fail(err) || err_is_fail(err2)) {
            err = err_is_fail(err) ? err: err2;
            return err;
        }
    }

    return SYS_ERR_OK;
}


static errval_t sfn5122f_control(struct devq* q, uint64_t cmd, uint64_t value, 
                                 uint64_t *result)
{
    struct sfn5122f_queue* queue = (struct sfn5122f_queue*) q;
    *result = queue->mac;
    DEBUG_QUEUE("Control cmd=%lu value=%lu \n", cmd, value);
    return SYS_ERR_OK;
}


static errval_t sfn5122f_notify(struct devq* q)
{
    DEBUG_QUEUE("Notify \n");
    return SYS_ERR_OK;
}

static errval_t enqueue_rx_buf(struct sfn5122f_queue* q, regionid_t rid,
                               genoffset_t offset, genoffset_t length,
                               genoffset_t valid_data, genoffset_t valid_length,
                               uint64_t flags)
{
    DEBUG_QUEUE("Enqueueing RX buf \n");
    // check if there is space

    if (sfn5122f_queue_free_rxslots(q) == 0) {
        DEBUG_QUEUE("SFN5122F_%d: Not enough space in RX ring, not adding buffer\n",
                q->id);
        return DEVQ_ERR_QUEUE_FULL;
    }

    // find region

    struct region_entry* entry = q->regions;
    
    // If regions already empty -> return error
    if (entry == NULL) {
        return DEVQ_ERR_INVALID_REGION_ARGS;
    }

    while((entry->next != NULL) && (entry->rid != rid)) {
        entry = entry->next;
    }
    
    if (entry == NULL) {
        return DEVQ_ERR_INVALID_REGION_ARGS;
    }

    if (q->userspace) {
        // compute buffer table entry of the rx buffer and the within it offset
        uint64_t buftbl_idx = entry->buftbl_idx + (offset/BUF_SIZE);
        uint16_t b_off = offset & 0x00000FFF;

        DEBUG_QUEUE("RX_BUF tbl_idx=%lu offset=%d flags=%lu \n",
                    buftbl_idx, b_off, flags);
        // still in the same buffer table entry
        assert(buftbl_idx == (entry->buftbl_idx + ((offset+length-1)/BUF_SIZE)));
        sfn5122f_queue_add_user_rxbuf_devif(q, buftbl_idx, b_off,
                                            rid, offset, length, valid_data,
                                            valid_length, flags);
    } else {
        sfn5122f_queue_add_rxbuf_devif(q, entry->phys + offset, rid, offset, length,
                                       valid_data, valid_length, flags);

    }

    update_rxtail(q, q->rx_tail);
    return SYS_ERR_OK;
}

static errval_t enqueue_tx_buf(struct sfn5122f_queue* q, regionid_t rid,
                               genoffset_t offset, genoffset_t length,
                               genoffset_t valid_data, genoffset_t valid_length,
                               uint64_t flags)
{
    DEBUG_QUEUE("Enqueueing TX buf\n");
    // check if there is space
    if (sfn5122f_queue_free_txslots(q) == 0) {
        printf("SFN5122F_%d: Not enough space in TX ring, not adding buffer\n",
                q->id);
        return DEVQ_ERR_QUEUE_FULL;
    }

    // find region
    struct region_entry* entry = q->regions;
    
    if (entry == NULL) {
        return DEVQ_ERR_INVALID_REGION_ARGS;
    }

    while((entry->next != NULL) && (entry->rid != rid)) {
        entry = entry->next;
    }
    
    if (entry == NULL) {
        return DEVQ_ERR_INVALID_REGION_ARGS;
    }
    

    if (q->userspace) {
        // compute buffer table entry of the rx buffer and the within it offset
        uint64_t buftbl_idx = entry->buftbl_idx + (offset/BUF_SIZE);
        uint16_t b_off = offset & 0x00000FFF;


        DEBUG_QUEUE("TX_BUF tbl_idx=%lu offset=%d flags=%lx \n", buftbl_idx, b_off,
                flags);
        // still in the same buffer table entry
        assert(buftbl_idx == (entry->buftbl_idx + ((offset+length-1)/BUF_SIZE)));
        sfn5122f_queue_add_user_txbuf_devif(q, buftbl_idx, b_off,
                                            rid, offset, length, valid_data,
                                            valid_length, flags);

    } else {

        DEBUG_QUEUE("TX_BUF phys=%zu \n", entry->phys + offset);
        sfn5122f_queue_add_txbuf_devif(q, entry->phys + offset, rid, offset,
                                       length, valid_data, valid_length,
                                       flags);
    }
    update_txtail(q, q->tx_tail);
    return SYS_ERR_OK;
}

static errval_t sfn5122f_enqueue(struct devq* q, regionid_t rid,
                                 genoffset_t offset, genoffset_t length,
                                 genoffset_t valid_data, genoffset_t valid_length,
                                 uint64_t flags)
{
    errval_t err;


    struct sfn5122f_queue* queue = (struct sfn5122f_queue*) q;
    if (flags & NETIF_RXFLAG) {
        /* can not enqueue receive buffer larger than 2048 bytes */
        assert(length <= 2048);

        err = enqueue_rx_buf(queue, rid, offset, length, valid_data, valid_length,
                             flags);
        if (err_is_fail(err)) {
            return err;
        }
    } else if (flags & NETIF_TXFLAG) {
        assert(length <= BASE_PAGE_SIZE);

        err = enqueue_tx_buf(queue, rid, offset, length, valid_data, valid_length,
                             flags);
        if (err_is_fail(err)) {
            return err;
        }
    } else {
        printf("Unknown buffer flags \n");
        return NIC_ERR_ENQUEUE;
    }

    return SYS_ERR_OK;
}

static errval_t sfn5122f_dequeue(struct devq* q, regionid_t* rid, genoffset_t* offset,
                                 genoffset_t* length, genoffset_t* valid_data,
                                 genoffset_t* valid_length, uint64_t* flags)
{
    uint8_t ev_code;
    errval_t err = DEVQ_ERR_QUEUE_EMPTY;
    
    struct sfn5122f_queue* queue = (struct sfn5122f_queue*) q;

    //sfn5122f_evq_rptr_reg_wr(queue->device, queue->id, queue->ev_head);
    //__sync_synchronize();

    if (queue->num_left > 0) {
        *rid = queue->bufs[queue->last_deq].rid;
        *offset = queue->bufs[queue->last_deq].offset;
        *flags = queue->bufs[queue->last_deq].flags;
        *valid_length = queue->bufs[queue->last_deq].valid_length;
        *valid_data = queue->bufs[queue->last_deq].valid_data;
        *length = queue->bufs[queue->last_deq].length;
        queue->num_left--;
        queue->last_deq++;
        return SYS_ERR_OK;
    }

    while(true) {
        ev_code = sfn5122f_get_event_code(queue);
        switch(ev_code){
        case EV_CODE_RX:
            // TODO multiple packets
            err = sfn5122f_queue_handle_rx_ev_devif(queue, rid, offset, length,
                                                    valid_data, valid_length,
                                                    flags);

            DEBUG_QUEUE("RX_EV Q_ID: %d len %ld OK %s \n", queue->id, *valid_length,
                        err_getstring(err));

            sfn5122f_queue_bump_evhead(queue);

            if (err_is_fail(err)) {
                debug_printf("enqueue again: rid=%u, off=%lx\n", *rid, *offset);
                err = enqueue_rx_buf(queue, *rid, *offset, *length,
                                     *valid_data, *valid_length,
                                     *flags);
                if (err_is_fail(err)) {
                    printf("Error receiving packet, could not enqueue buffer\n");
                    /* we need to return the buffer here, and let the networkstack
                     * deal with it */
                    return SYS_ERR_OK;
                }

                /* the packet has been discarded and enqueued successfully,
                 * return emtpy queue */
                err = DEVQ_ERR_QUEUE_EMPTY;
            } else {
                assert(*valid_length > 0);
                return SYS_ERR_OK;
            }
            break;
        case EV_CODE_TX:
            err = sfn5122f_queue_handle_tx_ev_devif(queue, rid, offset, length,
                                                    valid_data, valid_length,
                                                    flags);
            if (*flags & NETIF_RXFLAG) {
                            printf("HUH: reiceived rx buffer in tx event???\n");
                        }

            if (err_is_ok(err)) {
                DEBUG_QUEUE("TX EVENT OK %d \n", queue->id);
            } else {
                DEBUG_QUEUE("TX EVENT ERR %d \n", queue->id);
            }

            sfn5122f_queue_bump_evhead(queue);
            return SYS_ERR_OK;
        case EV_CODE_DRV:
            DEBUG_QUEUE("DRIVER EVENT %d\n", queue->id);
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
            DEBUG_QUEUE("MCDI EVENT \n");
            sfn5122f_queue_handle_mcdi_event(queue);
            sfn5122f_queue_bump_evhead(queue);
            break;
        case EV_CODE_GLOBAL:
            DEBUG_QUEUE("GLOBAL EVENT \n");
            sfn5122f_queue_bump_evhead(queue);
            break;
        case EV_CODE_NONE:
            if(queue->use_interrupts || ((queue->ev_head & ((EV_ENTRIES / 8) - 1)) == 0)) {
                sfn5122f_evq_rptr_reg_wr(queue->device, queue->id, queue->ev_head);
            }

            return err;
        }
    }

    return err;
}

static errval_t sfn5122f_destroy(struct devq* queue)
{
    errval_t err, err2;
    struct sfn5122f_queue* q;

    q = (struct sfn5122f_queue*) queue;

    err = q->b->rpc_tx_vtbl.destroy_queue(q->b, q->id, &err2);
    if (err_is_fail(err) || err_is_fail(err2)) {
        err = err_is_fail(err) ? err: err2;
        return err;
    }

    err = vspace_unmap(q->device_va);
    if (err_is_fail(err)) {
        return err;
    }

    free(q->device);
    free(q->b);

    err = sfn5122f_queue_free(q);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}




static void interrupt_handler(void* arg)
{
    struct sfn5122f_queue* queue = (struct sfn5122f_queue*) arg;

    queue->cb(queue);
}


/**
 * Public functions
 *
 */

errval_t sfn5122f_queue_create(struct sfn5122f_queue** q, sfn5122f_event_cb_t cb, 
                               bool userlevel, bool interrupts, bool qzero)
{
    DEBUG_QUEUE("create called \n");

    errval_t err;
    //struct capref tx_frame, rx_frame, ev_frame;
    struct capref frame;
    //size_t tx_size, rx_size, ev_size;
    size_t total_size;
    void *tx_virt, *rx_virt, *ev_virt;
    struct sfn5122f_queue* queue;
    struct frame_identity id;

    struct sfn5122f_queue_ops ops = {
        .update_txtail = update_txtail,
        .update_rxtail = update_rxtail
     };
   
    /* Allocate memory for descriptor rings
       No difference for userspace networking*/
    total_size = sizeof(uint64_t)*(TX_ENTRIES + RX_ENTRIES + EV_ENTRIES);
    tx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, total_size, &frame);
    if (tx_virt == NULL) {
        return DEVQ_ERR_INIT_QUEUE;
    }

    rx_virt = tx_virt + (sizeof(uint64_t) *TX_ENTRIES);
    ev_virt = rx_virt + (sizeof(uint64_t) *RX_ENTRIES);

    DEBUG_QUEUE("queue init \n");
    // Init queue
    queue = sfn5122f_queue_init(tx_virt, TX_ENTRIES, rx_virt, RX_ENTRIES,
                                ev_virt, EV_ENTRIES, &ops, userlevel);

    queue->frame = frame;
    queue->bound = false;
    queue->cb = cb;
    queue->use_interrupts = interrupts;

    
    iref_t iref;
    const char *name = "sfn5122f_sfn5122fmng_devif";

    // Connect to solarflare card driver
    err = nameservice_blocking_lookup(name, &iref);
    if (err_is_fail(err)) {
        return err;
    }

    DEBUG_QUEUE("binding \n");
    err = sfn5122f_devif_bind(iref, bind_cb, queue, get_default_waitset(),
                              1);
    if (err_is_fail(err)) {
        return err;
    }

    // wait until bound
    while(!queue->bound) {
        event_dispatch(get_default_waitset());
    }

    DEBUG_QUEUE("bound \n");

    errval_t err2;
    struct capref regs;

    // Inform card driver about new queue and get the registers/queue id
    err = slot_alloc(&regs);
    if (err_is_fail(err)) {
        return err;
    }

    if (!interrupts) {
        printf("Solarflare queue used in polling mode (default %d) \n", qzero);
        err = queue->b->rpc_tx_vtbl.create_queue(queue->b, frame, userlevel,
                                                 interrupts, qzero,
                                                 0, 0, &queue->mac ,&queue->id, 
                                                 &regs, &err2);
        if (err_is_fail(err) || err_is_fail(err2)) {
            err = err_is_fail(err) ? err: err2;
            return err;
        }
    } else {
        printf("Solarflare queue used in interrupt mode mode \n");
        err = pci_setup_inthandler(interrupt_handler, queue, &queue->vector);
        assert(err_is_ok(err));

        queue->core = disp_get_core_id();
        
        err = queue->b->rpc_tx_vtbl.create_queue(queue->b, frame, userlevel,
                                                 interrupts, qzero, queue->core,
                                                 queue->vector, &queue->mac, 
                                                 &queue->id, &regs, &err2);
        if (err_is_fail(err) || err_is_fail(err2)) {
            err = err_is_fail(err) ? err: err2;
            printf("Registering interrupt failed, continueing in polling mode \n");
        }
    }

    DEBUG_QUEUE("rpc done \n");
    
    err = invoke_frame_identify(regs, &id);
    if (err_is_fail(err)) {
        return err;
    }

    err = vspace_map_one_frame_attr(&queue->device_va, id.bytes, regs,
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }
      

    DEBUG_QUEUE("mapped \n");
    queue->device = malloc(sizeof(sfn5122f_t));
    sfn5122f_initialize(queue->device, queue->device_va);

    err = devq_init(&queue->q, false);
    if (err_is_fail(err)) {
        return err;
    }
    
    queue->q.f.enq = sfn5122f_enqueue;
    queue->q.f.deq = sfn5122f_dequeue;
    queue->q.f.reg = sfn5122f_register;
    queue->q.f.dereg = sfn5122f_deregister;
    queue->q.f.ctrl = sfn5122f_control;
    queue->q.f.notify = sfn5122f_notify;
    queue->q.f.destroy = sfn5122f_destroy; 
   
    *q = queue;

    queues[queue->id] = queue;

    return SYS_ERR_OK;
}

uint64_t sfn5122f_queue_get_id(struct sfn5122f_queue* q)
{
    return q->id;    
}
