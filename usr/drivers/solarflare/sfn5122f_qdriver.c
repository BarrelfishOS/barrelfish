/* Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitätstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <net_queue_manager/net_queue_manager.h>

#include <dev/sfn5122f_dev.h>
#include <dev/sfn5122f_q_dev.h>

#include "helper.h"
#include "sfn5122f.h"
#include "sfn5122f_queue.h"
#include "sfn5122f_debug.h"
#include "buffer_tbl.h"
#include "sfn5122f_qdriver.h"

/** Queue index for this manager instance */
static int qi = 0;

/** Mackerel handle for device */
static sfn5122f_t *d = NULL;

/** Queue handle for queue management library */
static sfn5122f_queue_t *q;

/** MAC address to be used */
static uint64_t mac_address = 0;

/** Indicates if the initialization is done */
static bool initialized = false;

/**
 * Indicates whether we should rely on cache coherence for the descriptor
 * rings.
 */
static bool cache_coherence = true;

/** Indicates whether Interrupts should be used */
static bool use_interrupts = true;

/** Capability for hardware TX ring */
static struct capref tx_frame;

/** Capability for hardware RX ring */
static struct capref rx_frame;

/** Capability for hardware EV ring */
static struct capref ev_frame;

//static void* mac_virt;
uint64_t mac_stats_array[NUM_MAC_STATS];


/******************************************************************************/
/* Transmit path */
static uint64_t find_tx_free_slot_count_fn(void)
{
    return sfn5122f_queue_free_txslots(q);
}

static errval_t transmit_pbuf_list_fn(struct driver_buffer *buffers,
                               size_t                count)
{
    size_t i;
    
    if (find_tx_free_slot_count_fn() < count) {
        return ETHERSRV_ERR_CANT_TRANSMIT;
    }

    for (i = 0; i < count; i++) {
        sfn5122f_queue_add_txbuf(q, buffers[i].pa,
                                 buffers[i].len, buffers[i].opaque, 
                                 (i == count - 1)); 
    }

    sfn5122f_queue_bump_txtail(q);

    return SYS_ERR_OK;
}

static bool handle_free_tx_slot_fn(void)
{
    return false;
}



/******************************************************************************/
/* Receive path */

static uint64_t find_rx_free_slot_count_fn(void)
{
   return sfn5122f_queue_free_rxslots(q);
}

static errval_t register_rx_buffer_fn(uint64_t paddr, void *vaddr, void *opaque)
{
    if (find_rx_free_slot_count_fn() == 0) {
        printf("SFN5122F_%d: Not enough space in RX ring, not adding buffer\n", 
                qi);
        return ETHERSRV_ERR_TOO_MANY_BUFFERS;
    }

    sfn5122f_queue_add_rxbuf(q, paddr, opaque);
    sfn5122f_queue_bump_rxtail(q);
    
    return SYS_ERR_OK;
}


/*  polling event queue for new events         */
static size_t check_for_new_events(void)
{
    size_t len = 0;
    size_t count = 0;
    uint8_t ev_code;
    // TODO add constant
    struct driver_rx_buffer buf[16];

    // need to block until initalized
    if (!initialized) {
        return NIC_ERR_QDRIVER;
    }
 
    ev_code = sfn5122f_get_event_code(q);
    while (ev_code != 15 && count < 100){
          void *op = NULL;
          ev_code = sfn5122f_get_event_code(q);
          switch(ev_code){
   
              case EV_CODE_RX:
                   // TODO multiple packets
                   if (sfn5122f_queue_handle_rx_ev(q, &op, &len) == SYS_ERR_OK) {
                        buf[0].len = len;
                        buf[0].opaque = op;
                        process_received_packet(buf, 1, 0);
                   } else {
                        DEBUG_QUEUE("Failed receiveing \n");
                        // TODO how to tell the the upper layer that it can reuse
                        // the rx buffer
                   }

                   DEBUG_QUEUE(" RX_EV Q_ID: %d len %ld \n", qi, len);
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_TX:
                   if (sfn5122f_queue_handle_tx_ev(q) == SYS_ERR_OK) {
                        DEBUG_QUEUE("TX EVENT OK %d \n", qi);
                        uint8_t index = 0;
                        while (q->num_left > 0) {
                            handle_tx_done(q->bufs[index]);
                            index++;
                            q->num_left--;
                        }
                   } else {
                        DEBUG_QUEUE("TX EVENT ERR %d \n", qi);
                   }
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_DRV:
                   DEBUG_QUEUE("DRIVER EVENT %d\n", qi);
                   sfn5122f_handle_drv_ev(q, qi);
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_DRV_GEN:
                   DEBUG_QUEUE("DRIVER GENERATED EVENT \n");
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_USER:
                   DEBUG_QUEUE("USER EVENT \n");
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_MCDI:
                   DEBUG_QUEUE("MCDI EVENT \n");
                   sfn5122f_queue_handle_mcdi_event(q);
                   sfn5122f_queue_bump_evhead(q);
                   break;
              case EV_CODE_GLOBAL:
                   DEBUG_QUEUE("GLOBAL EVENT \n");
                   sfn5122f_queue_bump_evhead(q);
                   break;
          }
          count++;
    }
    /* update event queue tail */
    //if (count > 0) {
        sfn5122f_evq_rptr_reg_wr(d, qi, q->ev_head);
    //}

    return count-1;
}

/**  Misc             */
static errval_t update_rxtail(struct sfn5122f_queue* que, void *opaque, size_t tail)
{
    assert(d != NULL);
    uint64_t reg = 0;

    reg = sfn5122f_rx_desc_upd_reg_hi_rx_desc_wptr_insert(reg, tail);
    /* don't want to push an additional rx descriptor with the write pointer */
    reg = sfn5122f_rx_desc_upd_reg_hi_rx_desc_push_cmd_insert(reg, 0);
    /* the lower register will be ignored   */
    sfn5122f_rx_desc_upd_reg_lo_wr(d, qi, 0);
    sfn5122f_rx_desc_upd_reg_hi_wr(d, qi, reg);

    return SYS_ERR_OK;
}

static errval_t update_txtail(struct sfn5122f_queue* que, void *opaque, size_t tail)
{
    assert(d != NULL);
    uint64_t reg = 0;
    reg = sfn5122f_tx_desc_upd_reg_hi_tx_desc_wptr_insert(reg, tail);
    /* don't want to push an additional tx descriptor with the write pointer */
    reg = sfn5122f_tx_desc_upd_reg_hi_tx_desc_push_cmd_insert(reg, 0);
    reg = sfn5122f_tx_desc_upd_reg_hi_tx_desc_insert(reg, 0);

    /*  the lower register will be ignored  */
    sfn5122f_tx_desc_upd_reg_lo_wr(d, qi, 0);
    sfn5122f_tx_desc_upd_reg_hi_wr(d, qi, reg);
    return SYS_ERR_OK;
}

/** Callback to get card's MAC address */
static void get_mac_address_fn(uint8_t* mac)
{
    memcpy(mac, &mac_address, 6);
}

/******************************************************************************/
/* Device/queue initialization */

/** Allocate queue n and return handle for queue manager */

static void setup_queue(struct capref* ev, struct capref* tx, struct capref* rx)
{
    size_t tx_size, rx_size, ev_size;
    void *tx_virt, *rx_virt, *ev_virt;
    vregion_flags_t flags_vreg;
    
    struct sfn5122f_queue_ops ops = {
        .update_txtail = update_txtail,
        .update_rxtail = update_rxtail
     };

    // Decide on which flags to use for the mappings
    flags_vreg = (cache_coherence ? VREGION_FLAGS_READ_WRITE :
                               VREGION_FLAGS_READ_WRITE_NOCACHE);

   
    /* Allocate memory for descriptor rings  
       No difference for userspace networking*/
    tx_size = sfn5122f_q_tx_ker_desc_size * TX_ENTRIES;
    tx_virt = alloc_map_frame(flags_vreg, tx_size, tx);

    assert(tx_virt != NULL);

    rx_size = sfn5122f_q_rx_ker_desc_size * RX_ENTRIES;

    rx_virt = alloc_map_frame(flags_vreg, rx_size, rx);
    assert(rx_virt != NULL);

    ev_size = sfn5122f_q_event_entry_size * EV_ENTRIES;
    ev_virt = alloc_map_frame(flags_vreg, ev_size, ev);
    assert(ev_virt != NULL);

    // Initialize queue manager
    q = sfn5122f_queue_init(tx_virt, TX_ENTRIES, rx_virt, RX_ENTRIES,
                            ev_virt, EV_ENTRIES, &ops,  NULL, false);
}



void write_queue_tails(void)
{
    DEBUG_QUEUE("idc_write_queue_tails()\n");

    sfn5122f_queue_bump_rxtail(q);
    sfn5122f_queue_bump_txtail(q);
}

size_t check_queue_0(void) 
{
        do_pending_work_for_all();
        return check_for_new_events();
}

static void terminate_queue_fn(void) 
{
    errval_t err;
    err = terminate_queue_0();
    assert(err_is_ok(err));
}

// Callback from device manager
errval_t terminate_queue_0(void)
{
    errval_t err;

    DEBUG_QUEUE("idc_queue_terminated()\n");

    // Free memory for hardware ring buffers
    if (q->userspace) {
        err = vspace_unmap(q->tx_ring.user);
        if (err_is_fail(err)) {
            return err;
        }
        err = vspace_unmap(q->rx_ring.user);
        if (err_is_fail(err)) {
            return err;
        }
    } else {
        err = vspace_unmap(q->tx_ring.ker);
        if (err_is_fail(err)) {
            return err;
        }
        err = vspace_unmap(q->rx_ring.ker);
        if (err_is_fail(err)) {
            return err;
        }
    }

    err = vspace_unmap(q->ev_ring);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_delete(tx_frame);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_delete(rx_frame);
    if (err_is_fail(err)) {
        return err;
    }
    err = cap_delete(ev_frame);
    if (err_is_fail(err)) {
        return err;
    }
    return SYS_ERR_OK;
}

errval_t init_queue_0(char* cname, uint64_t mac_addr, void* device, 
                      bool interrupts, bool userspace, struct capref* ev,
                      struct capref* tx, struct capref* rx) 
{
    use_interrupts = interrupts;
    mac_address = mac_addr;    

    d = malloc(sizeof(*d));
    sfn5122f_initialize(d, device);
    // Initialize queue
    setup_queue(ev, tx, rx);

    ethersrv_init((char*) cname, 0, 
                  get_mac_address_fn, 
                  terminate_queue_fn,
                  transmit_pbuf_list_fn, 
                  find_tx_free_slot_count_fn,
                  handle_free_tx_slot_fn, 
                  MTU_MAX, 
                  register_rx_buffer_fn,
                  find_rx_free_slot_count_fn);
 
    tx_frame = *tx;
    rx_frame = *tx;
    ev_frame = *tx;

    initialized = true;

    return SYS_ERR_OK;      
}
