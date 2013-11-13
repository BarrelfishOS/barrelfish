/**
 * \file
 * \brief Datapath Communication between LWIP and network driver
 *
 * This file manages and performs the datapath communication between LWIP
 * and the network driver
 */

/*
 * Copyright (c) 2007-11 ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <assert.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <contmng/contmng.h>
#include <contmng/netbench.h>
#include <procon/procon.h>
#include "lwip/pbuf.h"
#include "lwip/init.h"
#include "lwip/sys.h"
#include "mem_barrelfish.h"
#include "idc_barrelfish.h"
#include <if/net_queue_manager_defs.h>
//#include <if/net_ports_defs.h>
//#include <if/net_ports_rpcclient_defs.h>
#include <barrelfish/bulk_transfer_arch.h>

#include "lwip_barrelfish_debug.h"

/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define LWIP_TRACE_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE

//#define SPP_CARELESS

static uint8_t new_debug = 0;
static uint8_t benchmark_mode = 0;

struct waitset *lwip_waitset;


/*************************************************************
 * \defGroup LocalStates Local states
 *
 * @{
 *
 ****************************************************************/


/**
 * \brief Link to the network driver
 *
 * As mentionned above, the link to the network driver is compoed by
 * two Chips channels. We store the Flounder closure of these
 * connections in the following array.
 *
 */
struct net_queue_manager_binding *driver_connection[2];

/* FIXME: merge the actual variable and status into one. */
/**
 * \brief Status of the connections to network driver
 *
 * To be operational, LWIP has to wait for both transmit and receive
 * channels to be up. Hence the following array, updated when a
 * channel gets connected.
 *
 */
static bool lwip_connected[2] = { false, false };


/**
 * \brief Number of established connections
 *
 *
 *
 */
static int conn_nr = 0;

static uint64_t alloted_queue_id = 0; // queue_id allocated to this application

/**
 * \brief
 *
 */
static uint8_t *mac;


/**
 * \brief
 *
 */
static bool mac_received = false;

/**
 * \brief
 *
 *
 *
 */
static void (*lwip_rec_handler) (void *, uint64_t, uint64_t, uint64_t,
                                 uint64_t, struct pbuf *) = NULL;


/**
 * \brief
 *
 *
 *
 */
static void *lwip_rec_data;


/**
 * \brief
 *
 *
 *
 */
static void (*lwip_free_handler) (struct pbuf *) = NULL;

// Statistics about driver state
static uint64_t pkt_dropped = 0;        // pkts dropped by the driver
static uint64_t driver_tx_slots_left = 0;       // no. of free slots for TX

uint64_t idc_check_driver_load(void)
{
    return driver_tx_slots_left;
}

uint64_t idc_get_packet_drop_count(void)
{
    return driver_tx_slots_left;
}

static errval_t  send_sp_notification_from_app(struct q_entry e)
{
    uint64_t ts = rdtsc();
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
            e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;

    if (b->can_send(b)) {
#if LWIP_TRACE_MODE
        trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_AO_S, 0);
#endif // LWIP_TRACE_MODE

        if (benchmark_mode > 0) {
            netbench_record_event_simple(nb, TX_SN_WAIT, e.plist[1]);
        }

        errval_t err = b->tx_vtbl.sp_notification_from_app(b,
                          MKCONT(cont_queue_callback, ccnc->q),
                          e.plist[0], e.plist[1], ts);
        // queueid, type, ts
        if (benchmark_mode > 0) {
            netbench_record_event_simple(nb, TX_SN_SEND, ts);
        }
        return err;

    } else {
        printf("sp_notification_from_app: Flounder busy,rtry+++++\n");
        LWIPBF_DEBUG("sp_notification_from_app: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void wrapper_send_sp_notification_from_app(
                                struct net_queue_manager_binding *b)
{
    assert(b != NULL);
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_sp_notification_from_app;
    uint64_t ts = rdtsc();


#if LWIP_TRACE_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_AO_Q, 0);
#endif // LWIP_TRACE_MODE

    entry.binding_ptr = (void *)b;
    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;
    assert(ccnc != NULL);
    // Resetting the send_notification counter as we are sending
    // the notification
    entry.plist[0] = ccnc->queueid;
    entry.plist[1] = 1;
    entry.plist[2] = ts;
    enqueue_cont_q(ccnc->q, &entry);
    if (benchmark_mode > 0) {
        netbench_record_event_simple(nb, TX_SP, ts);
    }
    if (new_debug)
         printf("send_pkt: q len[%d]\n", ccnc->q->head - ccnc->q->tail);
}

static void sp_process_tx_done(bool debug)
{
    if (lwip_free_handler == 0) {
        return;
    }
    struct client_closure_NC *ccnc = (struct client_closure_NC *)
        driver_connection[TRANSMIT_CONNECTION]->st;
    struct shared_pool_private *spp_send = ccnc->spp_ptr;
    assert(spp_send != NULL);

    sp_reload_regs(spp_send);
    struct slot_data d;
    struct pbuf *done_pbuf;

    // Slot pointed by write id should be always free!
    if (sp_is_slot_clear(spp_send, spp_send->c_write_id) != 0) {
        if(!sp_clear_slot(spp_send, &d, spp_send->c_write_id)) {
            printf("sp_clear_slot failed\n");
            abort();
        }
        if (d.client_data != 0) {
            done_pbuf = (struct pbuf *) (uintptr_t) d.client_data;
            lwip_free_handler(done_pbuf);
        }
    }

    if (sp_queue_full(spp_send)) {
        if (debug) printf("sp_process_tx_done: queue full\n");
        return;
    }

    uint64_t start_index;  // starting index to start clearing slot (included)
    uint64_t stop_index;  // stopping index to stop clearing slot (excluded)

    if (sp_queue_empty(spp_send)) {
        if (debug) printf("sp_process_tx_done: queue empty\n");
         start_index = spp_send->pre_write_id;
         stop_index = spp_send->c_write_id;
    } else {
        // FIXME: Validate pre_write_id

         start_index = spp_send->pre_write_id;
         stop_index = spp_send->c_read_id;
    }

    if (start_index == stop_index) {
        return;
    }
    uint64_t i = start_index;
    if (debug)
        printf("sp_process_tx_done trying for element %"PRIu64" in range "
                "[%"PRIu64"- %"PRIu64"]\n",
            i, start_index, stop_index);

    if (debug) sp_print_metadata(spp_send);

    while (sp_c_between(start_index, i, stop_index, spp_send->c_size)) {

        if (debug)
            printf("inside while for %"PRIu64"\n", i);
        if (sp_is_slot_clear(spp_send, i) != 0) {
           if (debug)
            printf("#### problems in clearing the slot %"PRIu64"\n", i);
            if (sp_clear_slot(spp_send, &d, i) == false) {
                USER_PANIC("ERROR: bulk_transport_logic: slot not clear\n");
                abort();
            }
            /*
            printf("sp_process_done for %"PRIu64"\n", i);
            sp_print_slot(&spp_done->sp->slot_list[i].d);
            sp_print_slot(&d);
            */
            if (d.client_data == 0) {
                printf("Failed for id %"PRIu64"\n", i);
                sp_print_metadata(spp_send);
            }
            assert(d.client_data != 0);
            done_pbuf = (struct pbuf *) (uintptr_t) d.client_data;
           // LWIPBF_DEBUG
            if(debug)
                printf("sp_process_done for slot %"PRIu64", freeing pbuf %p\n",
                    i, done_pbuf);
            lwip_free_handler(done_pbuf);
            if(debug)
                printf("Freed up pbuf slot %"PRIu64"\n", i);
        } // end if : sp_is_slot_clear
//        spp_send->pre_write_id = i;
        i = (i + 1) % spp_send->c_size;
    } // end while:
    if (debug) printf("sp_process_tx_done is stopped for %"PRIu64"\n", i);
} // end function: sp_process_tx_done


static void do_pending_work_TX_lwip(void)
{
    struct net_queue_manager_binding *b = driver_connection[TRANSMIT_CONNECTION];
    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;
    struct shared_pool_private *spp_send = ccnc->spp_ptr;
    assert(spp_send != NULL);

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWITXWORK,
                    0);
#endif // TRACE_ONLY_SUB_NNET

    if (spp_send->notify_other_side != 0) {
#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWINOTIF,
                    0);
#endif // TRACE_ONLY_SUB_NNET

        // It seems that there we should send a notification to other side
        spp_send->notify_other_side = 0;
        wrapper_send_sp_notification_from_app(b);
    }

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWITXWORK,
                    1);
#endif // TRACE_ONLY_SUB_NNET

    // check and process any tx_done's
    sp_process_tx_done(false);
} // end function: do_pending_work_lwip

uint64_t idc_send_packet_to_network_driver(struct pbuf * p)
{
    ptrdiff_t offset;
    struct slot_data s;
    struct net_queue_manager_binding *b = driver_connection[TRANSMIT_CONNECTION];
    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;
    struct shared_pool_private *spp_send = ccnc->spp_ptr;
    assert(spp_send != NULL);
    uint64_t ts = rdtsc();
    assert(p != NULL);

    if (benchmark_mode > 0) {
        netbench_record_event_no_ts(nb, TX_SND_PKT_C);
    }

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWISEE,
                (uint32_t) ((uintptr_t)p->payload));
#endif // TRACE_ONLY_SUB_NNET

    // Find out no. of pbufs to send for single packet
    uint8_t numpbufs = 0;
    for (struct pbuf *tmpp = p; tmpp != 0; tmpp = tmpp->next) {
        numpbufs++;
    }

//    assert(numpbufs == 1);
//    assert(p->next == NULL);

    // Make sure that spp has enough slots to accomodate this packet
    sp_reload_regs(spp_send);
    uint64_t free_slots_count = sp_queue_free_slots_count(spp_send);
    // 4 mfence
    if (sp_queue_full(spp_send) || (free_slots_count < numpbufs)
            || (free_slots_count < 10) ) {
        if (benchmark_mode > 0) {
            netbench_record_event_no_ts(nb, TX_SPP_FULL);
            ++pkt_dropped;
        }

        // send a msg to driver saying "READ Packets Quickly!"
        spp_send->notify_other_side = 0;
        wrapper_send_sp_notification_from_app(b);

/*        printf("Not enough (%"PRIu8") space left in shared_pool %"PRIu64"\n",
               numpbufs, free_slots_count);
        sp_print_metadata(spp_send);
*/
//        assert(!"No space left in shared_pool\n");
        // free the pbuf
        return 0;
    }

    // Add all pbufs of this packet into spp
    uint64_t ghost_write_index = sp_get_write_index(spp_send);
    // 8 mfence
    uint64_t queue_size = sp_get_queue_size(spp_send);
    // 12

    LWIPBF_DEBUG("##### send_pkt_to_network: ghost_write_index [%"PRIu64"]"
            " for p %p\n", ghost_write_index, p);

    if (new_debug)
        printf("##### send_pkt_to_network: ghost_write_index [%"PRIu64"]"
            " for p %p\n", ghost_write_index, p);

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWIBFFENCE,
                    (uint32_t) ((uintptr_t) p->payload));
#endif // TRACE_ONLY_SUB_NNET

#if !defined(__scc__)
    mfence();                   // ensure that we flush all of the packet payload
#endif                          // !defined(__scc__)

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWIAFFENCE,
                    (uint32_t) ((uintptr_t) p->payload));
#endif // TRACE_ONLY_SUB_NNET

    uint8_t i = 0;
    for (struct pbuf * tmpp = p; tmpp != 0; tmpp = tmpp->next) {

    if (benchmark_mode > 0) {
        netbench_record_event_no_ts(nb, TX_SND_PKT_S);
    }

#if 0
#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(tmpp->payload, tmpp->len);
#endif // !defined(__scc__)
#endif // 0

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWIFLUSHED,
                    (uint32_t) ((uintptr_t) p->payload));
#endif // TRACE_ONLY_SUB_NNET


#ifndef SPP_CARELESS
        // sanity check: but we have already checked above if there is enough space
        if (sp_queue_full(spp_send)) {
            printf("Error state for pbuf part %"PRIu8", and old free slots "
                    "are %"PRIu64", new %"PRIu64"\n", i, free_slots_count,
                    sp_queue_free_slots_count(spp_send));
            assert(!"This should not happen!");
            return 0;
        }
#endif

        // Get all info about pbuf, and put it in slot data-structure
        struct buffer_desc *buf_p = mem_barrelfish_get_buffer_desc(p->payload);

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWIBDESC,
                    (uint32_t) ((uintptr_t) p->payload));
#endif // TRACE_ONLY_SUB_NNET


        bulk_arch_prepare_send((void *) tmpp->payload, tmpp->len);
        offset = (uintptr_t) tmpp->payload - (uintptr_t) (buf_p->va);

        s.buffer_id = buf_p->buffer_id;
        s.no_pbufs = numpbufs - i++;
        s.pbuf_id = ghost_write_index;
        s.offset = offset;
        s.len = tmpp->len;
        s.client_data = (uintptr_t)tmpp;
        s.ts = rdtsc();

#ifndef SPP_CARELESS
        // Again, sanity check!
        // Making sure that the slot is not active anymore
        if (sp_is_slot_clear(spp_send, ghost_write_index) != 0) {
            // Something unexpected is happening
            // Printing messages to help debugging what is going on.
            printf("############ trying to clear %"PRIu64"\n",
                    ghost_write_index);
            sp_print_metadata(spp_send);
            sp_print_slot(&spp_send->sp->slot_list[ghost_write_index].d);
            sp_process_tx_done(true);

            if (sp_is_slot_clear(spp_send, ghost_write_index) != 0) {
                printf("Slot not clear for index %"PRIu64"\n", ghost_write_index);
                sp_print_metadata(spp_send);
                sp_print_slot(&spp_send->sp->slot_list[ghost_write_index].d);
                assert(!"Slot not clear!!!\n");
            }
        }
#endif

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWISPPSND,
                    (uint32_t) ((uintptr_t) p->payload));
#endif // TRACE_ONLY_SUB_NNET

        // 16 mfence

        // Copy the slot in spp
        if (!sp_ghost_produce_slot(spp_send, &s, ghost_write_index)) {
            printf("sp_ghost_produce_slot: failed, %"PRIu64"\n",
                    ghost_write_index);
            sp_print_metadata(spp_send);
            assert(!"sp_ghost_produce_slot: failed\n");
            return numpbufs;
        }
        LWIPBF_DEBUG("#### to_network_driver, slot %"PRIu64" pbuf %p "
                "of len %"PRIu64"\n", ghost_write_index, tmpp, tmpp->len);
        if (new_debug)
            printf ("#### to_network_driver, slot %"PRIu64" pbuf %p "
                "of len %"PRIu16"\n", ghost_write_index, tmpp, tmpp->len);

        // 25 mfence

        // Increment the ghost write index
        // FIXME: Use the inbuilt spp->ghost_write_id instead of following var.
        ghost_write_index = (ghost_write_index + 1) % queue_size;
    } // end for: for each pbuf in packet

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWISPPIDX,
                    (uint32_t) ((uintptr_t) p->payload));
#endif // TRACE_ONLY_SUB_NNET

    // Added all packets.  Now update the write_index to expose new data
    if (!sp_set_write_index(spp_send, ghost_write_index)) {
        spp_send->ghost_write_id = ghost_write_index;
        assert(!"sp_ghost_produce_slot: failed\n");
        return 0;
    }

    // 34 mfence

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXLWISPPIDX,
                    (uint32_t) ((uintptr_t) p->payload));
#endif // TRACE_ONLY_SUB_NNET

    if (benchmark_mode > 0) {
            netbench_record_event_simple(nb, TX_SP1, ts);
    }


//  printf("idc_send_packet_to_network_driver  is done\n");
    // FIXME: check if there are any packets to send or receive
    do_pending_work_TX_lwip();

    return numpbufs;
} // end function: idc_send_packet_to_network_driver


void debug_show_spp_status(int connection)
{
    struct net_queue_manager_binding *b = driver_connection[connection];
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;
    sp_print_metadata(ccnc->spp_ptr);
}


static errval_t send_buffer_cap(struct q_entry e)
{
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
       e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;
    struct shared_pool_private *spp = ccnc->spp_ptr;
    assert(spp != NULL);
    uint8_t role = ccnc->buff_ptr->role;

    if (b->can_send(b)) {
/*
#if LWIP_TRACE_MODE
    	trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_A_BUFC, 0);
#endif // LWIP_TRACE_MODE
*/
//        printf("send_buffer_cap: sending register_buffer\n");
        errval_t err = b->tx_vtbl.register_buffer(b,
                          MKCONT(cont_queue_callback, ccnc->q),
                          e.cap, spp->cap, ccnc->queueid,
                          spp->sp->size_reg.value, role);

        // buf_cap, sp_cap, queueid, slot_no, role
        if (err_is_fail(err)) {
            printf("send_buffer_cap: failed\n");
        }
        return err;
    } else {
        LWIPBF_DEBUG("send_buffer_cap: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}



void idc_register_buffer(struct buffer_desc *buff_ptr,
        struct shared_pool_private *spp_ptr, uint8_t binding_index)
{

    struct q_entry entry;

    LWIPBF_DEBUG("idc_register_buffer for binding %d called\n", binding_index);
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_buffer_cap;
    entry.fname = "send_buffer_cap";

    struct net_queue_manager_binding *b = driver_connection[binding_index];

    entry.binding_ptr = (void *) b;
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    /* put the buffer into client_closure_NC */
    if (ccnc->buff_ptr != NULL) {
        /* FIXME: this needs better error handing */
        printf("idc_register_buffer: one buffer is already registered\n");
        abort();
        /* On one channel, only one buffer is allowed to register.
         * This restriction is not from the design, but the implementation.
         * Specially the client_closure in network driver has only one pointer
         * for storing the buffers related to it. */
    }

    buff_ptr->con = driver_connection[binding_index];

    ccnc->buff_ptr = buff_ptr;
    ccnc->role = binding_index;

    /* put the buffer into client_closure_NC */
    if (ccnc->spp_ptr != NULL) {
        /* FIXME: this needs better error handing */
        printf("idc_register_buffer: one spp is already registered\n");
        abort();
        /* On one channel, only one spp is allowed to register.
         */
    }
    ccnc->spp_ptr = spp_ptr;

    entry.cap = buff_ptr->cap;
    entry.plist[0] = binding_index;
    enqueue_cont_q(ccnc->q, &entry);

    LWIPBF_DEBUG("idc_register_buffer: terminated\n");
}

int lwip_check_sp_capacity(int direction)
{
    struct net_queue_manager_binding *b = driver_connection[direction];
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;
    return sp_queue_free_slots_count(ccnc->spp_ptr);
}


int idc_check_capacity(int direction)
{
//    RECEIVE_CONNECTION
    struct net_queue_manager_binding *b = driver_connection[direction];
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    return queue_free_slots(ccnc->q);
}


void idc_get_mac_address(uint8_t * mac_client)
{

    LWIPBF_DEBUG("idc_get_mac_address: called #######\n");

    mac = mac_client;
    errval_t err;

    struct net_queue_manager_binding *b = driver_connection[TRANSMIT_CONNECTION];
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;
    // FIXME: broken retry loop
    do {
        err =
          driver_connection[TRANSMIT_CONNECTION]->
          tx_vtbl.get_mac_address(driver_connection[TRANSMIT_CONNECTION],
                                  NOP_CONT,
                                  ccnc->queueid);
                        // queueid
        if (err_is_fail(err)) {
            if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
                err = event_dispatch(lwip_waitset);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "in event_dispatch on LWIP waitset");
                }
            } else {
                USER_PANIC_ERR(err,
                               "unhandled error sending get_mac_address request");
            }
        }
    } while (err_is_fail(err) && err_no(err) == FLOUNDER_ERR_TX_BUSY);

    while (!mac_received) {
        err = event_dispatch(lwip_waitset);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch on LWIP waitset");
        }
    }
    LWIPBF_DEBUG("idc_get_mac_address: terminated ##########\n");

}


static errval_t send_print_statistics_request(struct q_entry e)
{
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
        e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.print_statistics(b,
                          MKCONT(cont_queue_callback, ccnc->q),
                          e.plist[0]);
                // queueid
    } else {
        LWIPBF_DEBUG("send_print_stats_request: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}


void idc_print_statistics(void)
{
    LWIPBF_DEBUG("idc_print_statistics: called\n");

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_print_statistics_request;
    entry.fname = "send_print_statistics_request";

    struct net_queue_manager_binding *b = driver_connection[TRANSMIT_CONNECTION];

    entry.binding_ptr = (void *) b;

    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    entry.plist[0] = ccnc->queueid;
    enqueue_cont_q(ccnc->q, &entry);

    LWIPBF_DEBUG("idc_print_statistics: terminated\n");
}


void idc_print_cardinfo(void)
{
    printf("idc_print_cardinfo: Not yet Implemented\n");
    // FIXME: It should send msg to device driver and not queue manager
}


static errval_t send_benchmark_control_request(struct q_entry e)
{
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
        e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.benchmark_control_request(b,
                  MKCONT(cont_queue_callback, ccnc->q),
                  e.plist[0], (uint8_t) e.plist[1], e.plist[2], e.plist[3]);
                  // queueid,  status,     trigger,       cl
    } else {
        LWIPBF_DEBUG("send_benchmark_control_request: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

void idc_benchmark_control(int connection, uint8_t state, uint64_t trigger,
        uint64_t cl)
{
     LWIPBF_DEBUG("idc_debug_status:  called with status %x [%"PRIu64"]\n",
     state, trigger);
     printf("idc_debug_status:  called with status %x [%"PRIu64"]\n",
     state, trigger);

//    new_debug = state;
    struct q_entry entry;
    benchmark_mode = state;
    if (state == 1) {
        netbench_reset(nb);
        nb->status = 1;
        pkt_dropped = 0;
    }

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_benchmark_control_request;
    struct net_queue_manager_binding *b = driver_connection[connection];
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    ccnc->benchmark_status = state;
    ccnc->benchmark_delta = 0;
    ccnc->benchmark_cl = cl;
    entry.binding_ptr = (void *) b;
    entry.plist[0] = ccnc->queueid;
    entry.plist[1] = state;
    entry.plist[2] = trigger;
    entry.plist[3] = cl;

/*    printf("idc_debug_status: q size [%d]\n",
            ccnc->q->head - ccnc->q->tail);
    cont_queue_show_queue(ccnc->q);
*/
    enqueue_cont_q(ccnc->q, &entry);
/*
    if (b->can_send(b)){
        printf("idc_debug_status: can send packet right now!!\n");
    } else {
        printf("idc_debug_status: can't send packet, putting it in queue\n");
    }

    LWIPBF_DEBUG("idc_debug_status: terminated\n");
    printf("idc_debug_status: terminated\n");
*/
}


/**
 * \brief
 *
 *
 *
 */
void idc_register_receive_callback(void (*f)
                                    (void *, uint64_t, uint64_t, uint64_t,
                                     uint64_t, struct pbuf *), void *data)
{

    LWIPBF_DEBUG("idc_register_receive_callback: called\n");

    assert(f != 0);
    lwip_rec_handler = f;
    lwip_rec_data = data;

    LWIPBF_DEBUG("idc_register_receive_callback: terminated\n");

}


void idc_register_freeing_callback(void (*f) (struct pbuf *))
{

    LWIPBF_DEBUG("idc_register_freeing_callback: called\n");

    lwip_free_handler = f;

    LWIPBF_DEBUG("idc_register_freeing_callback: terminated\n");

}


/*************************************************************
 * \defGroup MessageHandlers Message Handlers
 *
 * (...)
 *
 * @{
 *
 ****************************************************************/


static void new_buffer_id(struct net_queue_manager_binding *st, errval_t err,
                          uint64_t queueid, uint64_t buffer_id)
{
    assert(err_is_ok(err));
    struct client_closure_NC *ccnc = (struct client_closure_NC *) st->st;

    assert(queueid == ccnc->queueid);
    ccnc->buff_ptr->buffer_id = buffer_id;
//    assign_id_to_latest_buffer(buffer_id);
    printf("[%d] new_buffer_id: buffer_id = %" PRIx64 "\n",
           disp_get_core_id(), buffer_id);

    LWIPBF_DEBUG("[%zu] new_buffer_id: buffer_id = %" PRIx64 "\n",
                 disp_get_core_id(), buffer_id);
    LWIPBF_DEBUG("new_buffer_id: EEEEEEE buffer_id = %" PRIx64 "\n", buffer_id);
}


static void get_mac_address_response(struct net_queue_manager_binding *st,
        uint64_t queueid, uint64_t hwaddr)
{
    LWIPBF_DEBUG("get_mac_address_response: called\n");

    *((uint64_t *) mac) = hwaddr;
    mac_received = true;

    LWIPBF_DEBUG("get_mac_address_response: terminated\n");
}


uint8_t get_driver_benchmark_state(int direction,
        uint64_t *delta, uint64_t *cl)
{
    struct net_queue_manager_binding *b = driver_connection[direction];
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;
    *delta = ccnc->benchmark_delta;
    *cl = ccnc->benchmark_cl;
    return ccnc->benchmark_status;
}



static void benchmark_control_response(struct net_queue_manager_binding *b,
        uint64_t queueid, uint8_t state, uint64_t delta, uint64_t cl)
{
    LWIPBF_DEBUG("benchmark_control_response: called\n");
    assert(b != NULL);
    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;
    assert(ccnc != NULL);
    struct buffer_desc *buff = ccnc->buff_ptr;
    assert(buff != NULL);
    ccnc->benchmark_status = state;
    ccnc->benchmark_delta = delta;
    ccnc->benchmark_cl = cl;
    LWIPBF_DEBUG("benchmark_control_response: terminated\n");
}


bool lwip_in_packet_received = false;

static uint32_t handle_incoming_packets(void)
{

    struct client_closure_NC *ccnc = (struct client_closure_NC *)
        driver_connection[RECEIVE_CONNECTION]->st;
//    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;
    assert(ccnc != NULL);
    struct buffer_desc *buff = ccnc->buff_ptr;
    assert(buff != NULL);
    assert(ccnc->spp_ptr != NULL);
    assert(ccnc->spp_ptr->sp != NULL);
/*
    if (benchmark_mode > 0) {
        netbench_record_event_simple(nb, TX_A_SP_RN_CS, rts);
    }
*/

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXLWIINCOM,
                0);
#endif // TRACE_ONLY_SUB_NNET

    if (new_debug) printf("handle_incoming_pkts called\n");
    // Read the slots which are available in spp

    // FIXME: assuming that packet fits into one slot
    struct slot_data sslot;
    bool ans;
    uint32_t count = 0;
    lwip_in_packet_received = true;
    while(1) {
        ans = sp_ghost_read_slot(ccnc->spp_ptr, &sslot);
        if (!ans) {
            // No more slots to read

/*            if (new_debug) {
                printf("@@@@@ Processed %"PRIu32" slots\n", count);
                sp_print_metadata(ccnc->spp_ptr);
            }
*/
            break;
        }
        if (new_debug)
            printf("%d.%d: packet_received: called pbuf = %" PRIx64 ", len %"
               PRIx64 "\n", disp_get_core_id(), disp_get_domain_id(),
               sslot.pbuf_id, sslot.len);

     /* FIXME: enable this.  It is disabled to avoid compiliation issues with ARM
     * Problem is due to use of unint64_t to store paddr. */
//      bulk_arch_prepare_recv((void *)paddr, pbuf_len);
        if (lwip_rec_handler == 0) {
            printf("packet_received: no callback installed\n");
            // pbuf with received packet not consumed by lwip. It can be
            // reused as receive pbuf
            struct pbuf *p = mem_barrelfish_replace_pbuf(sslot.pbuf_id);
            pbuf_free(p);
        } else {
            assert(sslot.no_pbufs == 1);

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXLWIINCOM,
                (uint32_t) sslot.pbuf_id);
#endif // TRACE_ONLY_SUB_NNET

            lwip_rec_handler(lwip_rec_data, sslot.pbuf_id,
                    sslot.offset, sslot.len, sslot.len, NULL);
        }
        if(!sp_ghost_read_confirm(ccnc->spp_ptr)) {
            printf("handle incoming packet: error in confirming ghost read\n");
            abort();
        }
/*
        if(benchmark_mode > 0) {
            netbench_record_event_simple(nb, RE_ALL, ts);
        }
*/
        ++count;

    } // end while:

    lwip_in_packet_received = false;
    return count;
} // end function: handle_incoming_packets

// Does all the work related to incoming and outgoing packets
uint64_t perform_lwip_work(void)
{
    handle_incoming_packets();
    sp_process_tx_done(false);
    return 0;
} // end function: perform_lwip_work



static void sp_notification_from_driver(struct net_queue_manager_binding *b,
       uint64_t queueid, uint64_t type, uint64_t rts)
{
    if (new_debug) printf("news from driver arrived!!\n");
    lwip_mutex_lock();
    uint64_t ts = rdtsc();
    assert(b != NULL);
    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;
    assert(ccnc != NULL);
    assert(ccnc->queueid == queueid);
    struct buffer_desc *buff = ccnc->buff_ptr;
    assert(buff != NULL);
    assert(ccnc->spp_ptr != NULL);
    assert(ccnc->spp_ptr->sp != NULL);

    if (benchmark_mode > 0) {
        netbench_record_event_simple(nb, TX_A_SP_RN_CS, rts);
    }

    perform_lwip_work();
    if (benchmark_mode > 0) {
        netbench_record_event_simple(nb, TX_A_SP_RN_T, ts);
    }
    lwip_mutex_unlock();
} // end function: sp_notification_from_driver


static struct net_queue_manager_rx_vtbl rx_vtbl = {
    .new_buffer_id = new_buffer_id,
    .sp_notification_from_driver = sp_notification_from_driver,
    .get_mac_address_response = get_mac_address_response,
    .benchmark_control_response = benchmark_control_response,
};



static void bind_cb(void *st, errval_t err, struct net_queue_manager_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }
    LWIPBF_DEBUG("connection_service_logic: started\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    struct client_closure_NC *cc = (struct client_closure_NC *)
      malloc(sizeof(struct client_closure_NC));

    memset(cc, 0, sizeof(struct client_closure_NC));
    b->st = cc;
    cc->queueid = alloted_queue_id;  // Assigning the queueid to this connection

    char appname[200];

    snprintf(appname, sizeof(appname), "appl_%d", conn_nr);
    cc->q = create_cont_q(appname);

    /* FIXME: I should not need this afterwards */
    driver_connection[conn_nr] = b;
    lwip_connected[conn_nr] = true;

/*
    printf("lwip: connected to  %d at [%p] + [%lu] = %p \n",
    		conn_nr, b, sizeof(struct net_queue_manager_binding),
    		(void *)((uint64_t)b) + sizeof(struct net_queue_manager_binding));

    LWIPBF_DEBUG("connection_service_logic: connection %d at [%p]\n",
    		conn_nr, b);
*/
    LWIPBF_DEBUG("connection_service_logic: terminated\n");
}



#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have
/**
 * \brief Connects the lwip instance with network card.
 *
 *
 */
static void start_client(char *card_name, uint64_t queueid)
{

    errval_t err;
    iref_t iref;
    char qm_name[MAX_SERVICE_NAME_LEN] = {0};

    alloted_queue_id = queueid;
    LWIPBF_DEBUG("start_client: called\n");

    if (card_name == NULL) {
        printf("No card name was specified, aborting\n");
        abort();
    }

    snprintf(qm_name, sizeof(qm_name), "%s_%"PRIu64"", card_name, queueid);
    LWIPBF_DEBUG("start_client: resolving driver %s\n", qm_name);

    err = nameservice_blocking_lookup(qm_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lwip: could not connect to the device driver.\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    LWIPBF_DEBUG("start_client: connecting\n");

    err = net_queue_manager_bind(iref, bind_cb, NULL, lwip_waitset,
            IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));     // XXX

    LWIPBF_DEBUG("start_client: terminated\n");


}

/**
 * \brief connect to the e1000 driver
 *
 */
//void idc_client_init(char *card_name)
void idc_connect_to_driver(char *card_name, uint64_t queueid)
{
    conn_nr = 0;

    LWIPBF_DEBUG("idc_client_init: start client\n");
    start_client(card_name, queueid);

    LWIPBF_DEBUG("idc_client_init: wait connection 0\n");
    while (!lwip_connected[conn_nr]) {
        messages_wait_and_handle_next();
    }
    conn_nr++;

    start_client(card_name, queueid);
    /* one for sending and one for receiving */
    LWIPBF_DEBUG("idc_client_init: wait connection 1\n");
    while (!lwip_connected[conn_nr]) {
        messages_wait_and_handle_next();
    }
    LWIPBF_DEBUG("idc_client_init: successfully connected with card [%s]\n",
                 card_name);
}


