/**
 * \file
 * \brief Communication between LWIP and network driver
 *
 * This file manages and performs the communication between LWIP
 * and the network driver
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich
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
#include <netif/etharp.h>
#include <netif/bfeth.h>
#include <trace/trace.h>
#include <contmng/contmng.h>
#include <procon/procon.h>
#include "lwip/pbuf.h"
#include "lwip/init.h"
#include "mem_barrelfish.h"
#include "idc_barrelfish.h"
#include <if/ether_defs.h>
#include <if/netd_defs.h>
#include <if/netd_rpcclient_defs.h>
#include <barrelfish/bulk_transfer_arch.h>

#include "lwip_barrelfish_debug.h"

/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define LWIP_TRACE_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE


static uint8_t new_debug = 0;
static uint8_t benchmark_mode = 0;

struct waitset *lwip_waitset;

/* closure for network card client */
struct client_closure_NC {
    struct cont_queue *q;       /* queue to continuation */
    struct buffer_desc *buff_ptr;
};

/*
 * If we are the owner of lwip stack, then we dont need rpc
 */

static bool is_owner = 0;
static uint16_t(*alloc_tcp_port) (void) = NULL;
static uint16_t(*alloc_udp_port) (void) = NULL;
static uint16_t(*bind_port) (uint16_t port, netd_port_type_t type) = NULL;
static void (*close_port) (uint16_t port, netd_port_type_t type) = NULL;


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
static struct ether_binding *driver_connection[2];
static struct netd_rpc_client netd_rpc;

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

static bool netd_service_connected = false;

/**
 * \brief Number of established connections
 *
 *
 *
 */
static int conn_nr = 0;


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
                                 uint64_t) = NULL;


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

static struct netif netif;

static errval_t  send_sp_notification_from_app(struct q_entry e)
{
    uint64_t ts = rdtsc();
    struct ether_binding *b = (struct ether_binding *)e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;

    if (b->can_send(b)) {
#if LWIP_TRACE_MODE
        trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_AO_S, 0);
#endif // LWIP_TRACE_MODE

        if (benchmark_mode > 0) {
            lwip_record_event_simple(TX_SN_WAIT, e.plist[1]);
        }

        errval_t err = b->tx_vtbl.sp_notification_from_app(b,
                          MKCONT(cont_queue_callback, ccnc->q),
                          e.plist[0], ts);
        // type, ts
        if (benchmark_mode > 0) {
            lwip_record_event_simple(TX_SN_SEND, ts);
        }
        return err;

    } else {
        printf("sp_notification_from_app: Flounder busy,rtry+++++\n");
        LWIPBF_DEBUG("sp_notification_from_app: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void sp_process_tx_done(struct buffer_desc *buff)
{
    assert(buff != NULL);
    assert(lwip_free_handler != 0);
    sp_reload_regs(buff->spp);
    struct slot_data d;
    struct pbuf *done_pbuf;
    if (sp_queue_empty(buff->spp)) {
//        printf("sp_process_tx_done, queue empty, stopping\n");
        if (sp_is_slot_clear(buff->spp, buff->spp->c_write_id) != 0) {
            assert(sp_clear_slot(buff->spp, &d, buff->spp->c_write_id));
            /*
            printf("sp_process_done for %"PRIu64"\n", i);
            sp_print_slot(&buff->spp->sp->slot_list[i].d);
            sp_print_slot(&d);
            */
            if (d.client_data == 0) {
                printf("Failed for id %"PRIu64"\n", buff->spp->c_write_id);
                sp_print_metadata(buff->spp);
            }
            assert(d.client_data != 0);
            done_pbuf = (struct pbuf *) (uintptr_t) d.client_data;
            lwip_free_handler(done_pbuf);
        }

        return;
    }
/*    else {
        sp_print_metadata(buff->spp);
        printf("sp_process_tx_done, queue not empty, procedding\n");
    }
*/

/*    printf("inside sp_process_tx_done, slot 0\n");
    sp_print_slot(&buff->spp->sp->slot_list[0].d);
*/
    uint64_t current_read = buff->spp->c_read_id;
//    uint64_t current_write = buff->spp->c_write_id;
    uint64_t i = buff->spp->c_write_id;
    // FIXME: use pre_write_id as cache of how much is already cleared
   i = buff->spp->pre_write_id;
//    printf("sp_process_tx_done trying for range %"PRIu64" - %"PRIu64"\n",
//            i, current_read);
    while (sp_c_between(buff->spp->c_write_id, i, current_read,
                buff->spp->c_size)) {

        if (sp_is_slot_clear(buff->spp, i) != 0) {
            assert(sp_clear_slot(buff->spp, &d, i));
            /*
            printf("sp_process_done for %"PRIu64"\n", i);
            sp_print_slot(&buff->spp->sp->slot_list[i].d);
            sp_print_slot(&d);
            */
            if (d.client_data == 0) {
                printf("Failed for id %"PRIu64"\n", i);
                sp_print_metadata(buff->spp);
            }
            assert(d.client_data != 0);
            done_pbuf = (struct pbuf *) (uintptr_t) d.client_data;
            lwip_free_handler(done_pbuf);
 //           printf("Freed up pbuf slot %"PRIu64"\n", i);
        } // end if : sp_is_slot_clear
//        buff->spp->pre_write_id = i;
        i = (i + 1) % buff->spp->c_size;
    } // end while:
//    printf("sp_process_tx_done is done\n");
}


uint64_t idc_send_packet_to_network_driver(struct pbuf * p)
{
    ptrdiff_t offset;
    struct buffer_desc *buff_ptr;
    struct slot_data s;


    uint64_t ts = rdtsc();
    assert(p != NULL);
    /*
     * Count the number of pbufs to be transmitted as a single message
     * to e1000
     */
    int numpbufs = 0;

    for (struct pbuf *tmpp = p; tmpp != 0; tmpp = tmpp->next) {
        numpbufs++;
    }
    buff_ptr = mem_barrelfish_get_buffer_desc(p->payload);
//    printf("to_network_driver 1, slot 0\n");
//    sp_print_slot(&buff_ptr->spp->sp->slot_list[0].d);

    if (sp_queue_free_slots_count(buff_ptr->spp) < numpbufs) {
        if (benchmark_mode > 0) {
            lwip_record_event_no_ts(TX_SPP_FULL);
            ++pkt_dropped;
        }
        printf("No space left in shared_pool\n");
        sp_print_metadata(buff_ptr->spp);
        assert(!"No space left in shared_pool\n");
        // free the pbuf
        return 0;
    }
    uint64_t ghost_write_index = sp_get_write_index(buff_ptr->spp);
    uint64_t queue_size = sp_get_queue_size(buff_ptr->spp);

#if !defined(__scc__)
    mfence();                   // ensure that we flush all of the packet payload
#endif                          // !defined(__scc__)

//    printf("to_network_driver 2, slot 0\n");
//    sp_print_slot(&buff_ptr->spp->sp->slot_list[0].d);

    int i = 0;
    for (struct pbuf * tmpp = p; tmpp != 0; tmpp = tmpp->next) {

#if !defined(__scc__) && !defined(__i386__)
        cache_flush_range(tmpp->payload, tmpp->len);
#endif // !defined(__scc__)

//    printf("to_network_driver 3, slot 0\n");
//    sp_print_slot(&buff_ptr->spp->sp->slot_list[0].d);

        assert(buff_ptr == mem_barrelfish_get_buffer_desc(tmpp->payload));

        assert(!sp_queue_full(buff_ptr->spp));

        bulk_arch_prepare_send((void *) tmpp->payload, tmpp->len);
        offset = (uintptr_t) tmpp->payload - (uintptr_t) (buff_ptr->va);

        s.buffer_id = buff_ptr->buffer_id;
        s.no_pbufs = numpbufs - i++;
        s.pbuf_id = ghost_write_index;
        s.offset = offset;
        s.len = tmpp->len;
        s.client_data = (uintptr_t)tmpp;
        s.ts = rdtsc();

//    printf("to_network_driver 4, slot 0\n");
//    sp_print_slot(&buff_ptr->spp->sp->slot_list[0].d);
//#ifdef USE_SPP_FOR_TX
        if (sp_is_slot_clear(buff_ptr->spp, ghost_write_index) != 0) {
            /*
            printf("############ trying to clear %"PRIu64"\n",
                    ghost_write_index);
            */
            sp_process_tx_done(buff_ptr);

            if (sp_is_slot_clear(buff_ptr->spp, ghost_write_index) != 0) {
                printf("Slot not clear for index %"PRIu64"\n", ghost_write_index);
                sp_print_metadata(buff_ptr->spp);
                assert(!"Slot not clear!!!\n");
            }
        }
//#endif // USE_SPP_FOR_TX

        if (!sp_ghost_produce_slot(buff_ptr->spp, &s, ghost_write_index)) {
            printf("sp_ghost_produce_slot: failed, %"PRIu64"\n",
                    ghost_write_index);
            sp_print_metadata(buff_ptr->spp);
            assert(!"sp_ghost_produce_slot: failed\n");
            return 0;
        }
//    printf("to_network_driver 5, slot 0\n");
//    sp_print_slot(&buff_ptr->spp->sp->slot_list[0].d);

        ghost_write_index = (ghost_write_index + 1) % queue_size;
    } // end for: for each pbuf in packet

    // Added all packets.  Now update the write_index to expose new data
    if (!sp_set_write_index(buff_ptr->spp, ghost_write_index)) {
        assert(!"sp_ghost_produce_slot: failed\n");
        return 0;
    }

    // FIXME: check if there are any packets to read, or any other work to do

    if (buff_ptr->spp->notify_other_side == 0) {
        // Done with everything!
        // FIXME: change the return value to reflect success/failure
        if (benchmark_mode > 0) {
            lwip_record_event_simple(TX_SP1, ts);
        }
//    printf("to_network_driver 6, slot 0\n");
//    sp_print_slot(&buff_ptr->spp->sp->slot_list[0].d);
    // check and process any tx_done's
    sp_process_tx_done(buff_ptr);

//    printf("idc_send_packet_to_network_driver  is done\n");
        return 0;
    }

    /*
    printf("to_network_driver 7, slot 0\n");
    sp_print_slot(&buff_ptr->spp->sp->slot_list[0].d);
    */
    // It seems that there we should send a notification to other side
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_sp_notification_from_app;
    struct ether_binding *b = buff_ptr->con;

#if LWIP_TRACE_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_AO_Q, 0);
#endif // LWIP_TRACE_MODE

//   printf("app: ending notification #############\n");
    entry.binding_ptr = (void *)b;
    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;
    // Resetting the send_notification counter as we are sending
    // the notification
    buff_ptr->spp->notify_other_side = 0;
    entry.plist[0] = numpbufs;
    entry.plist[1] = rdtsc();
    enqueue_cont_q(ccnc->q, &entry);
    if (benchmark_mode > 0) {
        lwip_record_event_simple(TX_SP, ts);
    }
    if (new_debug)
         printf("send_pkt: q len[%d]\n", ccnc->q->head - ccnc->q->tail);

    // Done with even notification sending!
/*
    printf("to_network_driver 8, slot 0\n");
    sp_print_slot(&buff_ptr->spp->sp->slot_list[0].d);
*/
    // check and process any tx_done's
    sp_process_tx_done(buff_ptr);

    // FIXME: change the return value to reflect success/failure
    return 0;
}


static errval_t send_buffer_cap(struct q_entry e)
{
    struct ether_binding *b = (struct ether_binding *) e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;
    struct shared_pool_private *spp = ccnc->buff_ptr->spp;
    assert(spp != NULL);
    uint8_t role = ccnc->buff_ptr->role;

    if (b->can_send(b)) {
/*
#if LWIP_TRACE_MODE
    	trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_A_BUFC, 0);
#endif // LWIP_TRACE_MODE
*/
        printf("send_buffer_cap: sending register_buffer\n");
        errval_t err = b->tx_vtbl.register_buffer(b,
                          MKCONT(cont_queue_callback, ccnc->q),
                          e.cap, spp->cap, spp->sp->size_reg.value,
                          role);

        /* buf_cap, sp_cap, slot_no, role */
        if (err_is_fail(err)) {
            printf("send_buffer_cap: failed\n");
        } else {
            printf("send_buffer_cap: success!!\n");
        }
        return err;
    } else {
        LWIPBF_DEBUG("send_buffer_cap: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}


void idc_register_buffer(struct buffer_desc *buff_ptr, uint8_t binding_index)
{

    struct q_entry entry;

    LWIPBF_DEBUG("idc_register_buffer for binding %d called\n", binding_index);
    printf("idc_register_buffer for binding %d called\n", binding_index);
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_buffer_cap;
    entry.fname = "send_buffer_cap";

    struct ether_binding *b = driver_connection[binding_index];

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
    entry.cap = buff_ptr->cap;
    entry.plist[0] = binding_index;
    enqueue_cont_q(ccnc->q, &entry);

    LWIPBF_DEBUG("idc_register_buffer: terminated\n");
    printf("idc_register_buffer: terminated\n");
}

/**
 * \brief
 *
 *
 *
 */

static errval_t send_pbuf_request(struct q_entry e)
{
    struct ether_binding *b = (struct ether_binding *) e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    if (b->can_send(b)) {
/* To see if pbuf requests are flying around or not. */

#if LWIP_TRACE_MODE
//      trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_AOR_S, e.plist[0]);
#endif                          // LWIP_TRACE_MODE

        uint64_t cs_counter = disp_run_counter();
        uint64_t ts = rdtsc();
        uint8_t canary = 57;
        queue_set_canary(ccnc->q, canary);

        errval_t err = b->tx_vtbl.register_pbuf(b,
                                        MKCONT(cont_queue_callback, ccnc->q),
                                        e.plist[0], e.plist[1], e.plist[2],
                                        ts);
        if(benchmark_mode > 0) {
            uint64_t c_cs_counter = disp_run_counter();
            if (cs_counter != c_cs_counter) {
                lwip_record_event_simple(RE_PBUF_REPLACE_3,
                        (c_cs_counter - cs_counter));
            }
            lwip_record_event_simple(RE_PBUF_REPLACE_2, ts);
            lwip_record_event_simple(RE_PBUF_QUEUE, e.plist[3]);
        }
        return err;

        /* pbuf_id,   offset,      len */
    } else {
        LWIPBF_DEBUG("send_pbuf_request: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}

void idc_register_pbuf(uint64_t pbuf_id, uint64_t paddr, uint64_t len)
{

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_pbuf_request;
    entry.fname = "send_pbuf_request";
    struct ether_binding *b = driver_connection[RECEIVE_CONNECTION];

    entry.binding_ptr = (void *) b;

    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    entry.plist[0] = pbuf_id;
    entry.plist[1] = paddr;
    entry.plist[2] = len;
    entry.plist[3] = rdtsc();
    /* FIXME: Following call is not strictly needed, so one might
     * want to remove it as data is not modified in calling idc_register_pbuf */
//    bulk_arch_prepare_send((void *)paddr, len);

    /* NOTE: comment it as it will occur with large frequencey */
/*    LWIPBF_DEBUG("idc_register_pbuf: pbuf %lu ==> buff chan %lu \n",
				pbuf_id, ccnc->buff_ptr->buffer_id);
*/
    enqueue_cont_q(ccnc->q, &entry);

}

int lwip_check_sp_capacity(int direction)
{
    struct ether_binding *b = driver_connection[direction];
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;
    return sp_queue_free_slots_count(ccnc->buff_ptr->spp);
}



int idc_check_capacity(int direction)
{
//    RECEIVE_CONNECTION
    struct ether_binding *b = driver_connection[direction];
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    return queue_free_slots(ccnc->q);
}



void idc_get_mac_address(uint8_t * mac_client)
{

    LWIPBF_DEBUG("idc_get_mac_address: called #######\n");

    mac = mac_client;
    errval_t err;

    // FIXME: broken retry loop
    do {
        err =
          driver_connection[TRANSMIT_CONNECTION]->
          tx_vtbl.get_mac_address(driver_connection[TRANSMIT_CONNECTION],
                                  NOP_CONT);
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
    struct ether_binding *b = (struct ether_binding *) e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.print_statistics(b,
                                           MKCONT(cont_queue_callback,
                                                  ccnc->q));
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

    struct ether_binding *b = driver_connection[TRANSMIT_CONNECTION];

    entry.binding_ptr = (void *) b;

    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    enqueue_cont_q(ccnc->q, &entry);

    LWIPBF_DEBUG("idc_print_statistics: terminated\n");
}


static errval_t send_print_cardinfo_handler(struct q_entry e)
{
    struct ether_binding *b = (struct ether_binding *) e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.print_cardinfo(b,
                                         MKCONT(cont_queue_callback, ccnc->q));
    } else {
        LWIPBF_DEBUG("send_print_stats_request: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}


void idc_print_cardinfo(void)
{
    LWIPBF_DEBUG("idc_print_cardinfo: called\n");
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_print_cardinfo_handler;
    entry.fname = "send_print_cardinfo_handler";

    struct ether_binding *b = driver_connection[TRANSMIT_CONNECTION];

    entry.binding_ptr = (void *) b;

    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    enqueue_cont_q(ccnc->q, &entry);

    LWIPBF_DEBUG("idc_print_statistics: terminated\n");
}


static errval_t send_debug_status_request(struct q_entry e)
{
    struct ether_binding *b = (struct ether_binding *) e.binding_ptr;
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.debug_status(b,
                                       MKCONT(cont_queue_callback, ccnc->q),
                                       (uint8_t) e.plist[0], e.plist[1]);
                                    //           status,     trigger
    } else {
        LWIPBF_DEBUG("send_debug_status_request: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}


void idc_debug_status(int connection, uint8_t state, uint64_t trigger)
{
     LWIPBF_DEBUG("idc_debug_status:  called with status %x %[PRIu64]\n",
     state, trigger);
//     printf("idc_debug_status:  called with status %x %[PRIu64]\n",
//     state, trigger);

//    new_debug = state;
    struct q_entry entry;
    benchmark_mode = state;
    if (state == 1) {
        lwip_reset_stats();
        pkt_dropped = 0;
    }

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_debug_status_request;
    struct ether_binding *b = driver_connection[connection];
    struct client_closure_NC *ccnc = (struct client_closure_NC *) b->st;

    entry.binding_ptr = (void *) b;
    entry.plist[0] = state;
    entry.plist[1] = trigger;


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
                                    (void *, uint64_t,
                                     uint64_t, uint64_t, uint64_t), void *data)
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


static void new_buffer_id(struct ether_binding *st, errval_t err,
                          uint64_t buffer_id)
{
    assert(err_is_ok(err));
    struct client_closure_NC *ccnc = (struct client_closure_NC *) st->st;

    ccnc->buff_ptr->buffer_id = buffer_id;
//    assign_id_to_latest_buffer(buffer_id);
    printf("[%d] new_buffer_id: buffer_id = %" PRIx64 "\n",
           disp_get_core_id(), buffer_id);

    LWIPBF_DEBUG("[%zu] new_buffer_id: buffer_id = %" PRIx64 "\n",
                 disp_get_core_id(), buffer_id);
    LWIPBF_DEBUG("new_buffer_id: EEEEEEE buffer_id = %" PRIx64 "\n", buffer_id);
}

static void sp_notification_from_driver(struct ether_binding *b, uint64_t type,
        uint64_t rts)
{
    lwip_mutex_lock();
    uint64_t ts = rdtsc();
    assert(b != NULL);
    struct client_closure_NC *ccnc = (struct client_closure_NC *)b->st;
    assert(ccnc != NULL);
    struct buffer_desc *buff = ccnc->buff_ptr;
    assert(buff != NULL);
    assert(buff->spp != NULL);
    assert(buff->spp->sp != NULL);
    if (benchmark_mode > 0) {
        lwip_record_event_simple(TX_A_SP_RN_CS, rts);
    }

    // FIXME:  trigger the function which checks for released pbufs
    // in form of tx_done and process them

    if (benchmark_mode > 0) {
        lwip_record_event_simple(TX_A_SP_RN_T, ts);
    }
    lwip_mutex_unlock();
} // end function: sp_notification_from_driver


static void tx_done(struct ether_binding *st, uint64_t client_data,
                    uint64_t slots_left, uint64_t dropped)
{
    struct pbuf *done_pbuf = (struct pbuf *) (uintptr_t) client_data;
    struct client_closure_NC *ccnc = (struct client_closure_NC *)st->st;
    assert(ccnc != NULL);
    struct buffer_desc *buff = ccnc->buff_ptr;
    assert(buff != NULL);

    /*
    printf("tx_done for %"PRIu64"\n", client_data);

    printf("tx_done: and slot zero is \n" );
    sp_print_slot(&buff->spp->sp->slot_list[0].d);
    */
    lwip_mutex_lock();

    // FIXME: use the slots_left and dropped info
    if (dropped == 1) {
        pkt_dropped = pkt_dropped + 1;
    }

    driver_tx_slots_left = slots_left;

#if LWIP_TRACE_MODE
    /* FIXME: Need a way to find out the pbuf_id for this tx_done */
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_AIR_R,
                (uint32_t) ((uintptr_t) done_pbuf));

#endif                          // LWIP_TRACE_MODE

    if (new_debug)
        printf("tx_done: %" PRIx64 "\n", client_data);
    if (lwip_free_handler != 0) {
        lwip_free_handler(done_pbuf);
    } else {
        fprintf(stderr, "idc_barrelfish: event_handler: no handler for "
                "freeing pbufs installed.\n");
    }

    /*
    printf("tx_done: and slot zero after lwip_free_handler \n" );
    sp_print_slot(&buff->spp->sp->slot_list[0].d);
    */
    lwip_mutex_unlock();

/*    LWIPBF_DEBUG("tx_done: terminated\n");	*/
}


/**
 * \brief
 *
 *
 *
 */
static void get_mac_address_response(struct ether_binding *st, uint64_t hwaddr)
{
    LWIPBF_DEBUG("get_mac_address_response: called\n");

    *((uint64_t *) mac) = hwaddr;
    mac_received = true;

    LWIPBF_DEBUG("get_mac_address_response: terminated\n");
}

bool lwip_in_packet_received = false;


static void packet_received(struct ether_binding *st, uint64_t pbuf_id,
                            uint64_t paddr, uint64_t pbuf_len,
                            uint64_t pktlen, uint64_t rts)
{
    if(benchmark_mode > 0) {
        lwip_record_event_simple(RE_PKT_RCV_CS, rts);
    }
#if LWIP_TRACE_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_AI_A, (uint32_t) pbuf_id);
#endif                          // LWIP_TRACE_MODE
    uint64_t ts = rdtsc();
    if (new_debug)
        printf("%d.%d: packet_received: called paddr = %" PRIx64 ", len %"
               PRIx64 "\n", disp_get_core_id(), disp_get_domain_id(), paddr,
               pktlen);
    LWIPBF_DEBUG("packet_received: called\n");

    lwip_mutex_lock();

    lwip_in_packet_received = true;

    /* FIXME: enable this.  It is disabled to avoid compiliation issues with ARM
     * Problem is due to use of unint64_t to store paddr. */
//      bulk_arch_prepare_recv((void *)paddr, pbuf_len);
    if (pbuf_len == 0) {
        printf("### Received pbuf_id %" PRIu64 " at %" PRIu64 " of size %"
               PRIu64 " " "and len %" PRIu64 "\n", pbuf_id, paddr, pbuf_len,
               pktlen);
    }
    if (lwip_rec_handler != 0) {
        lwip_rec_handler(lwip_rec_data, pbuf_id, paddr, pbuf_len, pktlen);
    } else {
        LWIPBF_DEBUG("packet_received: no callback installed\n");
        if (new_debug)
            printf("packet_received: no callback installed\n");
        //pbuf with received packet not consumed by lwip. It can be
        //reused as receive pbuf
        idc_register_pbuf(pbuf_id, paddr, pbuf_len);
    }

    lwip_in_packet_received = false;

    lwip_mutex_unlock();

    if(benchmark_mode > 0) {
        lwip_record_event_simple(RE_ALL, ts);
    }

//  LWIPBF_DEBUG("packet_received: terminated\n");
}

/*
 * @}
 */


/****************************************************************
 * \defGroup netd_connectivity  Code to connect and work with netd.
 *
 * @{
 *
 *****************************************************************/
/**
 * \brief Callback function when bind is successful.
 *  Code inspired (ie. copied) from "start_client" function.
 */
static void netd_bind_cb(void *st, errval_t err, struct netd_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed for netd");
        abort();
    }
    LWIPBF_DEBUG("netd_bind_cb: called\n");

    err = netd_rpc_client_init(&netd_rpc, b);
    assert(err_is_ok(err));

    netd_service_connected = true;
    LWIPBF_DEBUG("netd_bind_cb: netd bind successful!\n");
}

/**
 * \brief Connects the lwip instance with netd daemon.
 *  Code inspired (ie. copied) from "start_client" function.
 */
static void init_netd_connection(char *service_name)
{
    LWIPBF_DEBUG("init_netd_connection: called\n");
    assert(service_name != NULL);
    LWIPBF_DEBUG("init_netd_connection: connecting to [%s]\n", service_name);

    errval_t err;
    iref_t iref;

    LWIPBF_DEBUG("init_netd_connection: resolving driver %s\n", service_name);

    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lwip: could not connect to the netd driver.\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    LWIPBF_DEBUG("init_netd_connection: connecting\n");

    err = netd_bind(iref, netd_bind_cb, NULL, lwip_waitset,
                    IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));     // XXX

    LWIPBF_DEBUG("init_netd_connection: terminated\n");
}




/****************************************************************
 * \defGroup FlounderVtables Flounder vtables
 *
 * @{
 *
 *****************************************************************/


static struct ether_rx_vtbl rx_vtbl = {
    .new_buffer_id = new_buffer_id,
    .sp_notification_from_driver = sp_notification_from_driver,
    .tx_done = tx_done,
    .get_mac_address_response = get_mac_address_response,
    .packet_received = packet_received,
};



static void bind_cb(void *st, errval_t err, struct ether_binding *b)
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

    char appname[200];

    snprintf(appname, sizeof(appname), "appl_%d", conn_nr);
    cc->q = create_cont_q(appname);

    /* FIXME: I should not need this afterwards */
    driver_connection[conn_nr] = b;
    lwip_connected[conn_nr] = true;

/*
    printf("lwip: connected to  %d at [%p] + [%lu] = %p \n",
    		conn_nr, b, sizeof(struct ether_binding),
    		(void *)((uint64_t)b) + sizeof(struct ether_binding));

    LWIPBF_DEBUG("connection_service_logic: connection %d at [%p]\n",
    		conn_nr, b);
*/
    LWIPBF_DEBUG("connection_service_logic: terminated\n");
}



/**
 * \brief Connects the lwip instance with network card.
 *
 *
 */
static void start_client(char *card_name)
{

    errval_t err;
    iref_t iref;

    LWIPBF_DEBUG("start_client: called\n");

    if (card_name == NULL) {
        card_name = "e1000";
    }

    LWIPBF_DEBUG("start_client: resolving driver %s\n", card_name);

    err = nameservice_blocking_lookup(card_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lwip: could not connect to the e1000 driver.\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    LWIPBF_DEBUG("start_client: connecting\n");

    err = ether_bind(iref, bind_cb, NULL, lwip_waitset, IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));     // XXX

    LWIPBF_DEBUG("start_client: terminated\n");


}


/****************************************************************
 * \defGroup IdcAPI Connection management
 *
 * @{
 *
 *****************************************************************/

struct thread *trace_thread = NULL;
void thread_debug_regs(struct thread *t);

/**
 * \brief handle msgs on the tx, rx and then the rest connections in that priority
 */
void network_polling_loop(void)
{
    errval_t err;

    while (1) {
        err = event_dispatch(lwip_waitset);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
#if 0
        if (trace_thread != NULL) {
            static int iter = 0;

            iter++;
            if (iter % 10 == 0) {
                thread_debug_regs(trace_thread);
            }
        }
#endif
    }
}

/*
static void network_messages_wait_and_handle_next(void)
{
    errval_t err = event_dispatch(lwip_waitset);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in event_dispatch for network_messages_wait_and_handle_next hack");
    }
}
*/


void idc_connect_to_netd(char *server_name)
{
    LWIPBF_DEBUG("idc_connect_to_netd: wait for netd connection\n");

    /* FIXME: decide if this is the best place to connect with netd */
    init_netd_connection(server_name);

    // XXX: dispatch on default waitset until bound
    struct waitset *dws = get_default_waitset();

    while (!netd_service_connected) {
        errval_t err = event_dispatch(dws);

        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch while binding");
        }
    }
    LWIPBF_DEBUG("idc_connect_to_netd: terminated\n");
}


/**
 * \brief connect to the e1000 driver
 *
 */
//void idc_client_init(char *card_name)
void idc_connect_to_driver(char *card_name)
{
    conn_nr = 0;

    LWIPBF_DEBUG("idc_client_init: start client\n");
    start_client(card_name);

    LWIPBF_DEBUG("idc_client_init: wait connection 0\n");
    while (!lwip_connected[conn_nr]) {
        messages_wait_and_handle_next();
    }
    conn_nr++;

    start_client(card_name);
    /* one for sending and one for receiving */
    LWIPBF_DEBUG("idc_client_init: wait connection 1\n");
    while (!lwip_connected[conn_nr]) {
        messages_wait_and_handle_next();
    }
    LWIPBF_DEBUG("idc_client_init: successfully connected with card [%s]\n",
                 card_name);
}



/**
* \brief: this function is to be only used for testing purpose
*/
void idc_just_to_test(void)
{
    if (lwip_connected[0]) {
        printf("TEST: ether[0] up\n");
    } else {
        printf("TEST: ether[0] down!!!!\n");
        printf("Aborting remaining test\n");
        return;
    }

    if (lwip_connected[1]) {
        printf("TEST: ether[1] up\n");
    } else {
        printf("TEST: ether[1] down!!!!\n");
        printf("Aborting remaining test\n");
        return;
    }

    if (netd_service_connected) {
        printf("TEST: netd up\n");
    } else {
        printf("TEST: netd down!!!!\n");
        printf("Aborting remaining test\n");
        return;
    }

    printf("testing get_port\n");

}

/*
 * @}
 */

errval_t lwip_err_to_errval(err_t e)
{
    switch (e) {
        case ERR_OK:
            return SYS_ERR_OK;
        case ERR_MEM:
            return LWIP_ERR_MEM;
        case ERR_BUF:
            return LWIP_ERR_BUF;
        case ERR_TIMEOUT:
            return LWIP_ERR_TIMEOUT;
        case ERR_RTE:
            return LWIP_ERR_RTE;
        case ERR_ABRT:
            return LWIP_ERR_ABRT;
        case ERR_RST:
            return LWIP_ERR_RST;
        case ERR_CLSD:
            return LWIP_ERR_CLSD;
        case ERR_CONN:
            return LWIP_ERR_CONN;
        case ERR_VAL:
            return LWIP_ERR_VAL;
        case ERR_ARG:
            return LWIP_ERR_ARG;
        case ERR_USE:
            return LWIP_ERR_USE;
        case ERR_IF:
            return LWIP_ERR_IF;
        case ERR_ISCONN:
            return LWIP_ERR_ISCONN;
        case ERR_INPROGRESS:
            return LWIP_ERR_INPROGRESS;
        default:
            USER_PANIC("unknown LWIP error in lwip_err_to_errval");
    }
}


/***************************************************************
    Adding new code to communicate with netd server
*/

void idc_get_ip(void)
{
    if (is_owner) {
        assert(!"owner of lwip should never ask for ip through API\n");
    }
    LWIPBF_DEBUG("On the way of getting IP\n");

    errval_t err;
    struct ip_addr ip, gw, nm;

    err = netd_rpc.vtbl.get_ip_info(&netd_rpc, &ip.addr, &gw.addr, &nm.addr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("got answer, now setting up things\n");
    netif_add(&netif, &ip, &nm, &gw, NULL, bfeth_init, ethernet_input);
    netif_set_default(&netif);
    netif_set_up(&netif);

    LWIPBF_DEBUG("client: owner has the IP address %d.%d.%d.%d\n",
                 ip4_addr1(&netif.ip_addr), ip4_addr2(&netif.ip_addr),
                 ip4_addr3(&netif.ip_addr), ip4_addr4(&netif.ip_addr));
}



/***********************************************************/
/************* Port management *******************/

static err_t idc_close_port(uint16_t port, int port_type)
{
    LWIPBF_DEBUG("idc_close_port: called\n");
    if (is_owner) {
        close_port((uint64_t) port, port_type);
        return ERR_OK;
    }

    LWIPBF_DEBUG("idc_close_port: called\n");

    errval_t err, msgerr;

    err = netd_rpc.vtbl.close_port(&netd_rpc, port_type, port, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("idc_close_tcp_port: returning\n");

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    }                           // FIXME: other errors?
    return ERR_OK;
}


err_t idc_close_udp_port(uint16_t port)
{
    return idc_close_port(port, netd_PORT_UDP);
}


err_t idc_close_tcp_port(uint16_t port)
{
    return idc_close_port(port, netd_PORT_TCP);
}

static err_t idc_bind_port(uint16_t port, netd_port_type_t port_type)
{
    if (is_owner) {
        LWIPBF_DEBUG("idc_bind_port: called by owner\n");
        return bind_port(port, port_type);
    }

    LWIPBF_DEBUG("idc_bind_port: called\n");

    errval_t err, msgerr;

    /* getting the proper buffer id's here */
    err = netd_rpc.vtbl.bind_port(&netd_rpc, port_type, port,
                                  /* buffer for RX */
                                  ((struct client_closure_NC *)
                                   driver_connection[RECEIVE_CONNECTION]->st)->
                                  buff_ptr->buffer_id,
                                  /* buffer for TX */
                                  ((struct client_closure_NC *)
                                   driver_connection[TRANSMIT_CONNECTION]->st)->
                                  buff_ptr->buffer_id, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("idc_new_tcp_port: terminated\n");

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    }                           // FIXME: other errors?
    return ERR_OK;
}


err_t idc_bind_udp_port(uint16_t port)
{
    return idc_bind_port(port, netd_PORT_UDP);
}


err_t idc_bind_tcp_port(uint16_t port)
{
    return idc_bind_port(port, netd_PORT_TCP);
}

static err_t idc_new_port(uint16_t * port_no, netd_port_type_t port_type)
{
    /* NOTE: function with same name exists in Kaver's code for reference
       purpose */
    errval_t err, msgerr;

    LWIPBF_DEBUG("idc_new_port: called\n");


    /* getting the proper buffer id's here */
    err = netd_rpc.vtbl.get_port(&netd_rpc, port_type,
                                 /* buffer for RX */
                                 ((struct client_closure_NC *)
                                  driver_connection[RECEIVE_CONNECTION]->st)->
                                 buff_ptr->buffer_id,
                                 /* buffer for TX */
                                 ((struct client_closure_NC *)
                                  driver_connection[TRANSMIT_CONNECTION]->st)->
                                 buff_ptr->buffer_id, &msgerr, port_no);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("idc_new_tcp_port: terminated\n");
    return msgerr;
}

err_t idc_tcp_new_port(uint16_t * port_no)
{
    if (is_owner) {
        *port_no = alloc_tcp_port();
        return SYS_ERR_OK;
    }

    return idc_new_port(port_no, netd_PORT_TCP);
}


err_t idc_udp_new_port(uint16_t * port_no)
{
    if (is_owner) {
        *port_no = alloc_udp_port();
        return SYS_ERR_OK;

    }

    return idc_new_port(port_no, netd_PORT_UDP);
}


static err_t idc_redirect(struct ip_addr *local_ip, u16_t local_port,
                          struct ip_addr *remote_ip, u16_t remote_port,
                          netd_port_type_t port_type)
{
    if (is_owner) {
        // redirecting doesn't make sense if we are the owner
        return ERR_USE;         // TODO: correct error
    }

    errval_t err, msgerr;

    /* getting the proper buffer id's here */
    err =
      netd_rpc.vtbl.redirect(&netd_rpc, port_type, local_ip->addr, local_port,
                             remote_ip->addr, remote_port,
                             /* buffer for RX */
                             ((struct client_closure_NC *)
                              driver_connection[RECEIVE_CONNECTION]->st)->
                             buff_ptr->buffer_id,
                             /* buffer for TX */
                             ((struct client_closure_NC *)
                              driver_connection[TRANSMIT_CONNECTION]->st)->
                             buff_ptr->buffer_id, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending redirect");
    }

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    } else if (msgerr == PORT_ERR_REDIRECT) {
        return ERR_USE;         // TODO: correct error
    }
// FIXME: other errors?
    return ERR_OK;
}

static err_t idc_pause(struct ip_addr *local_ip, u16_t local_port,
                       struct ip_addr *remote_ip, u16_t remote_port,
                       netd_port_type_t port_type)
{
    if (is_owner) {
        // redirecting doesn't make sense if we are the owner
        return ERR_USE;         // TODO: correct error
    }

    errval_t err, msgerr;

    /* getting the proper buffer id's here */
    err =
      netd_rpc.vtbl.redirect_pause(&netd_rpc, port_type, local_ip->addr,
                                   local_port, remote_ip->addr, remote_port,
                                   /* buffer for RX */
                                   ((struct client_closure_NC *)
                                    driver_connection[RECEIVE_CONNECTION]->st)->
                                   buff_ptr->buffer_id,
                                   /* buffer for TX */
                                   ((struct client_closure_NC *)
                                    driver_connection[TRANSMIT_CONNECTION]->
                                    st)->buff_ptr->buffer_id, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending pause");
    }

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    } else if (msgerr == PORT_ERR_REDIRECT) {
        return ERR_USE;         // TODO: correct error
    }
// FIXME: other errors?
    return ERR_OK;
}


/*
err_t idc_redirect_udp_port(uint16_t port)
{
    return idc_redirect_port(port, netd_PORT_UDP);
}
*/

err_t idc_redirect_tcp(struct ip_addr * local_ip, u16_t local_port,
                       struct ip_addr * remote_ip, u16_t remote_port)
{
    return idc_redirect(local_ip, local_port, remote_ip, remote_port,
                        netd_PORT_TCP);
}

err_t idc_pause_tcp(struct ip_addr * local_ip, u16_t local_port,
                    struct ip_addr * remote_ip, u16_t remote_port)
{
    return idc_pause(local_ip, local_port, remote_ip, remote_port,
                     netd_PORT_TCP);
}


void perform_ownership_housekeeping(uint16_t(*alloc_tcp_ptr) (void),
                                    uint16_t(*alloc_udp_ptr) (void),
                                    uint16_t(*bind_port_ptr) (uint16_t,
                                                              netd_port_type_t),
                                    void (*close_port_ptr) (uint16_t,
                                                            netd_port_type_t))
{
    is_owner = true;
    alloc_tcp_port = alloc_tcp_ptr;
    alloc_udp_port = alloc_udp_ptr;
    bind_port = bind_port_ptr;
    close_port = close_port_ptr;
}
