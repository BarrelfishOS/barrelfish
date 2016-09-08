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
#include <barrelfish/deferred.h>
#include <devif/queue_interface.h>
#include <devif/sfn5122f_devif_direct.h>
#include "sfn5122f.h"
#include "sfn5122f_queue.h"

errval_t sfn5122f_create_direct(struct devq* q, uint64_t flags)
{
    struct capref tx_frame, rx_frame, ev_frame;
    size_t tx_size, rx_size, ev_size;
    void *tx_virt, *rx_virt, *ev_virt;
    struct sfn5122f_queue* queue;

    struct sfn5122f_queue_ops ops = {
        .update_txtail = NULL,
        .update_rxtail = NULL
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

    queue = sfn5122f_queue_init(tx_virt, TX_ENTRIES, rx_virt, RX_ENTRIES,
                                ev_virt, EV_ENTRIES, &ops,  NULL, true);

 //   q->q = queue;
    // TODO set queue state
    return SYS_ERR_OK;
}

errval_t sfn5122f_register_direct(struct devq* q, struct capref cap,
                                  regionid_t rid) 
{
   return SYS_ERR_OK;
}

errval_t sfn5122f_deregister_direct(struct devq* q, regionid_t rid) 
{
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


errval_t sfn5122f_enqueue_direct(struct devq* q, regionid_t rid, bufferid_t bid, 
                                 lpaddr_t base, size_t len, uint64_t flags)
{
    return SYS_ERR_OK;
}

errval_t sfn5122f_dequeue_direct(struct devq* q, regionid_t* rid, bufferid_t* bid, 
                                 lpaddr_t* base, size_t* len, uint64_t* flags)
{
    return SYS_ERR_OK;
}

