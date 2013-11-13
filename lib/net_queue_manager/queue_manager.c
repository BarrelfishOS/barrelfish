/**
 * \file
 * \brief Generic server part for most ethernet drivers.
 * Current drivers using this server code are
 * -- e1000n
 * -- rtl8029
 * -- eMAC
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/net_constants.h>
#include <stdio.h>
#include <string.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <net_queue_manager/net_queue_manager.h>
#include <if/net_queue_manager_defs.h>

#include "QM_benchmark.h"
#include "queue_manager_debug.h"
#include "queue_manager_local.h"


/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif                          // CONFIG_TRACE && NETWORK_STACK_TRACE



// FIXME: This is repeated, make it common
#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have

/*****************************************************************
 * Global datastructure
 *****************************************************************/
struct netbench_details *bm = NULL; // benchmarking data holder

struct buffer_descriptor *buffers_list = NULL;

/*****************************************************************
 * Prototypes
 *****************************************************************/

static void register_buffer(struct net_queue_manager_binding *cc,
        struct capref cap, struct capref sp, uint64_t queueid,
        uint64_t slots, uint8_t role);
static void sp_notification_from_app(struct net_queue_manager_binding *cc,
        uint64_t queueid, uint64_t type, uint64_t ts);
static void get_mac_addr_qm(struct net_queue_manager_binding *cc,
        uint64_t queueid);
static void print_statistics_handler(struct net_queue_manager_binding *cc,
        uint64_t queueid);

static void do_pending_work(struct net_queue_manager_binding *b);

/*****************************************************************
 * VTABLE
 *****************************************************************/

// Initialize service
static struct net_queue_manager_rx_vtbl rx_nqm_vtbl = {
    .register_buffer = register_buffer,
    .sp_notification_from_app = sp_notification_from_app,
    .get_mac_address = get_mac_addr_qm,
    .print_statistics = print_statistics_handler,
    .benchmark_control_request = benchmark_control_request,
};

/*****************************************************************
 * Pointers to driver functionalities:
 *****************************************************************/
static ether_get_mac_address_t ether_get_mac_address_ptr = NULL;
static ether_transmit_pbuf_list_t ether_transmit_pbuf_list_ptr = NULL;
static ether_get_tx_free_slots tx_free_slots_fn_ptr = NULL;
static ether_handle_free_TX_slot handle_free_tx_slot_fn_ptr = NULL;

/*****************************************************************
 * Local states:
 *****************************************************************/
static char exported_queue_name[MAX_SERVICE_NAME_LEN] = {0}; // exported Q name
static uint64_t exported_queueid = 0; // id of queue
static int client_no = 0;  // number of clients(apps) connected
static uint64_t buffer_id_counter = 0; // number of buffers registered
// FIXME: following should be gone in next version of code
static uint64_t netd_buffer_count = 0; // way to identify the netd
static uint64_t dropped_pkt_count = 0; // Counter for dropped packets

static struct buffer_descriptor *first_app_b = NULL;


// *************************************************************
// Client and buffer management code
// *************************************************************

// Creates a new client for given connection
static struct client_closure *create_new_client(
        struct net_queue_manager_binding *b)
{
    struct client_closure *cc =
      (struct client_closure *) malloc(sizeof(struct client_closure));
    if (cc == NULL) {
        ETHERSRV_DEBUG("create_new_client: out of memory\n");
        return NULL;
    }
    memset(cc, 0, sizeof(struct client_closure));

    struct buffer_descriptor *buffer =
      (struct buffer_descriptor *) malloc(sizeof(struct buffer_descriptor));
    if (buffer == NULL) {
        ETHERSRV_DEBUG("create_new_client: out of memory\n");
        free(cc);
        return NULL;
    }
    memset(buffer, 0, sizeof(struct buffer_descriptor));

    cc->spp_ptr = (struct shared_pool_private *)
                        malloc(sizeof(struct shared_pool_private));
    if (cc->spp_ptr == NULL) {
        ETHERSRV_DEBUG("create_new_client: out of memory\n");
        free(cc);
        free(buffer);
        return NULL;
    }
    memset(cc->spp_ptr, 0, sizeof(struct shared_pool_private));
    buffer->spp_prv = cc->spp_ptr;

    b->st = cc;
    cc->app_connection = b;

    // FIXME: I should not need this as now netd is normal app
    // save it if it is netd app
    if (client_no < 2) {
        netd[client_no] = b;
    }

    cc->buffer_ptr = buffer;
    cc->debug_state = 0;        // Default debug state : no debug
    reset_client_closure_stat(cc);
    cc->start_ts = rdtsc();

    cc->cl_no = client_no++;

    char name[64];
    sprintf(name, "ether_a_%d", cc->cl_no);
    cc->q = create_cont_q(name);
    if (cc->q == NULL) {
        ETHERSRV_DEBUG("create_new_client: queue allocation failed\n");
        free(buffer);
        free(cc);
        return NULL;
    }
    return cc;
} // end function: create_new_client

// populates the given buffer with given capref
static errval_t populate_buffer(struct buffer_descriptor *buffer,
        struct capref cap)
{

    buffer->cap = cap;
    struct frame_identity pa;
    errval_t err = invoke_frame_identify(cap, &pa);
    if (!err_is_ok(err)) {
        printf("invoke_frame_identify failed\n");
        abort();
    }
    buffer->pa = pa.base;
    buffer->bits = pa.bits;

#ifdef __scc__
    err = vspace_map_one_frame_attr(&buffer->va, (1L << buffer->bits), cap,
                                  VREGION_FLAGS_READ_WRITE_MPB, NULL, NULL);
#else
    err = vspace_map_one_frame(&buffer->va, (1L << buffer->bits), cap,
            NULL, NULL);
#endif

/*
    err = vspace_map_one_frame_attr(&buffer->va, (1L << buffer->bits), cap,
                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
*/

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        // FIXME: report more sensible error
        return(ETHERSRV_ERR_TOO_MANY_BUFFERS);
    }

    buffer_id_counter++;
    buffer->buffer_id = buffer_id_counter;
//    printf("### buffer gets id %"PRIu64"\n", buffer->buffer_id);
    if (buffer->buffer_id == 3) {
        first_app_b = buffer;
    }

    buffer->next = buffers_list;
    // Adding the buffer on the top of buffer list.
//    buffers_list = buffer;
    if (buffer->role == RX_BUFFER_ID) {
        // This is a buffer for receiving
    }
    return SYS_ERR_OK;
} // end function: populate_buffer


// Find buffer in the list of all registered buffer using buffer_id
// FIXME: it is singly linked list, hence very slow if no of apps increases
struct buffer_descriptor *find_buffer(uint64_t buffer_id)
{
    struct buffer_descriptor *elem = buffers_list;
    while(elem) {
        if (elem->buffer_id == buffer_id) {
            return elem;
        }
        elem = elem->next;
    }
    printf("Could not find buffer with id %"PRIu64"\n", buffer_id);
    // abort here because in some cases, returning NULL crashes the driver
    // specially in e1000n.c: transmit_pbuf_list_fn() call
    abort();
    return NULL;
} // end function: buffer_descriptor


// **********************************************************
// Interface  implementation
// **********************************************************

// *********** Interface: register_buffer *******************
static errval_t send_new_buffer_id(struct q_entry entry)
{
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
                    entry.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.new_buffer_id(b, MKCONT(cont_queue_callback, ccl->q),
                                        entry.plist[0],
                                        entry.plist[1],
                                        entry.plist[2]);
        // entry.error, entry.queueid, entry.buffer_id
    } else {
        ETHERSRV_DEBUG("send_new_buffer_id Flounder busy.. will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void report_register_buffer_result(struct net_queue_manager_binding *cc,
        errval_t err, uint64_t queueid, uint64_t buffer_id)
{
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_new_buffer_id;
    entry.binding_ptr = (void *) cc;
    struct client_closure *ccl = (struct client_closure *) cc->st;

    entry.plist[0] = err;
    entry.plist[1] = queueid;
    entry.plist[2] = buffer_id;
    //   error, queue_id, buffer_id
    enqueue_cont_q(ccl->q, &entry);
}


// Actual register_buffer function with all it's logic
static void register_buffer(struct net_queue_manager_binding *cc,
            struct capref cap, struct capref sp, uint64_t queueid,
            uint64_t slots, uint8_t role)
{

    ETHERSRV_DEBUG("ethersrv:register buffer called with slots %"PRIu64"\n",
            slots);
    errval_t err;
    int i;
    struct client_closure *closure = (struct client_closure *)cc->st;
    assert(exported_queueid == queueid);
    closure->queueid = queueid;

    struct buffer_descriptor *buffer = closure->buffer_ptr;
    err = populate_buffer(buffer, cap);
    if (err_is_fail(err)) {
        report_register_buffer_result(cc, err, queueid, 0);
        return;
    }
    buffer->role = role;
    buffer->con = cc;
    buffer->queueid = queueid;

    err = sp_map_shared_pool(closure->spp_ptr, sp, slots, role);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "sp_map_shared_pool failed");
        // FIXME: report more sensible error
        report_register_buffer_result(cc, err, queueid, 0);
        return;
    }

   closure->tx_index = closure->spp_ptr->c_write_id;
   closure->rx_index = closure->spp_ptr->c_read_id;

    /* FIXME: replace the name netd with control_channel */
    for (i = 0; i < NETD_BUF_NR; i++) {
        if (netd[i] == cc) {
            ETHERSRV_DEBUG("buffer registered with netd connection\n");
            netd_buffer_count++;
        }
    }

    ETHERSRV_DEBUG("register_buffer:buff_id[%" PRIu64 "] pa[%" PRIuLPADDR
                   "] va[%p] bits[%" PRIu64 "] Role[%"PRIu8"]#####\n",
                   buffer->buffer_id, buffer->pa, buffer->va, buffer->bits,
                   buffer->role);

    // Adding the buffer on the top of buffer list.
    buffers_list = buffer;

    sp_reload_regs(closure->spp_ptr);
    report_register_buffer_result(cc, err, queueid, buffer->buffer_id);
    assert(buffer->con != NULL);
} // end function: register_buffer

// *********** Interface: sp_send_notification_from_driver ****************

// wrapper function:
static errval_t wrapper_send_sp_notification_from_driver(struct q_entry e)
{
//    ETHERSRV_DEBUG("send_sp_notification_from_driver-----\n");
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
        e.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXESVAPPNOTIF,
                0);
#endif // TRACE_ONLY_SUB_NNET

    if (ccl->debug_state_tx == 4) {
        netbench_record_event_simple(bm, RE_TX_SP_MSG_Q, e.plist[1]);
    }
    uint64_t ts = rdtsc();
    if (b->can_send(b)) {
        errval_t err = b->tx_vtbl.sp_notification_from_driver(b,
                MKCONT(cont_queue_callback, ccl->q),
                e.plist[0], e.plist[1], ts);
                // queueid, type, ts
        if (ccl->debug_state_tx == 4) {
            netbench_record_event_simple(bm, RE_TX_SP_MSG, ts);
        }
        return err;
    } else {
        ETHERSRV_DEBUG("send_sp_notification_from_driver: Flounder busy.."
                "retry --------\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

// notifiy application that something interesting has happened
// and it should check the spp
static bool send_notification_to_app(struct client_closure *cl)
{

    if (cl->spp_ptr->notify_other_side == 0) {
        return false;
    }
    // Send notification to application
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = wrapper_send_sp_notification_from_driver;
    entry.binding_ptr = (void *) cl->app_connection;
    entry.plist[0] = cl->queueid;
    entry.plist[1] = 1; // FIXME: will have to define these values
    // FIXME: Get the remaining slots value from the driver
    entry.plist[2] = rdtsc();
    enqueue_cont_q(cl->q, &entry);
    cl->spp_ptr->notify_other_side = 0;
    return true;
} // end function: send_notification_to_app


// *********** Interface: sp_notification_from_app ****************

// Notification received from the application
// Most probably, it needs some attention
static void sp_notification_from_app(struct net_queue_manager_binding *cc,
        uint64_t queueid, uint64_t type, uint64_t rts)
{
    struct client_closure *closure = (struct client_closure *)cc->st;
    assert(closure != NULL);
    assert(closure->queueid == queueid);
    if (closure->debug_state_tx == 4) {
        netbench_record_event_simple(bm, RE_TX_NOTI_CS, rts);
    }

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXESVNOTIF,
                0);
#endif // TRACE_ONLY_SUB_NNET

    // FIXME : call schedule work
    do_pending_work(cc);

} // end function:  sp_notification_from_app


// *********** Interface: get_mac_address ****************

// wrapper function for responce
static errval_t wrapper_send_mac_addr_response(struct q_entry entry)
{
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
        entry.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.get_mac_address_response(b,
                          MKCONT(cont_queue_callback, ccl->q),
                          entry.plist[0], entry.plist[1]);
        // queueid, hwaddr
    } else {
        ETHERSRV_DEBUG("send_mac_addr_response Flounder busy.. will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

uint64_t get_mac_addr_from_device(void)
{
    union {
        uint8_t hwaddr[6];
        uint64_t hwasint;
    } u;

    u.hwasint = 0;
    ether_get_mac_address_ptr(u.hwaddr);
    return u.hwasint;
}

// function to handle incoming mac address requests
static void get_mac_addr_qm(struct net_queue_manager_binding *cc,
        uint64_t queueid)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = wrapper_send_mac_addr_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure *ccl = (struct client_closure *) cc->st;
    assert(ccl->queueid == queueid);

    entry.plist[0] = queueid;
    entry.plist[1] = get_mac_addr_from_device();
       // queueid,  hwaddr

    enqueue_cont_q(ccl->q, &entry);
}

// *********** Interface: print_statistics ****************

// FIXME: NYI
static void print_statistics_handler(struct net_queue_manager_binding *cc,
        uint64_t queueid)
{
    //ETHERSRV_DEBUG
    printf("ETHERSRV: print_statistics_handler: called.\n");
}


// **********************************************************
// Functionality: packet sending (TX path)
// **********************************************************

// send one packet from given connection
static bool send_single_pkt_to_driver(struct net_queue_manager_binding *cc)
{
    errval_t err;

    // sanity checks: making sure function is not called with invalid parameters
    struct client_closure *cl = (struct client_closure *)cc->st;
    assert(cl != NULL);
    struct shared_pool_private *spp = cl->spp_ptr;
    assert(spp != NULL);
    assert(spp->sp != NULL);

    if (cl->tx_index == spp->c_write_id) {
        return false;
    }

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXESVSSPOW,
            (uint32_t)cl->tx_index);
#endif // TRACE_ONLY_SUB_NNET

    struct slot_data *sld = &spp->sp->slot_list[cl->tx_index].d;

    // check if there are all pbufs needed for this packet
    uint64_t available_slots = sp_c_range_size(cl->tx_index, spp->c_write_id,
            spp->c_size);
    if (available_slots < sld->no_pbufs) {
        USER_PANIC("Incomplete packet sent by app!");
        return false;
    }
    // TODO: send the list to driver
    err = ether_transmit_pbuf_list_ptr(cl);
#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXESVSSPOW,
            (uint32_t)cl->tx_index);
#endif // TRACE_ONLY_SUB_NNET
    cl->tx_index = (cl->tx_index + sld->no_pbufs) % spp->c_size;

    if (err_is_fail(err)) {
        dropped_pkt_count++;
        // FIXME: Need to mark the spp slot as free
        return false;
    }

    // successfull transfer!
    if (cl->debug_state_tx == 4) {
        // Benchmarking mode is on
        ++cl->pkt_count;
        if (cl->pkt_count == cl->out_trigger_counter) {
            benchmark_control_request(cc, cl->queueid,  BMS_STOP_REQUEST, 0, 0);
        }
    } // end if: benchmarking mode is on

    if (cl->debug_state_tx == 3) {
        // Benchmarking mode should be started here!
        ++cl->pkt_count;
        assert(cl->pkt_count == 1);
        // This is the first packet, so lets restart the timer!!
        cl->start_ts_tx = rdtsc();
        cl->debug_state_tx = 4;
    } // end if: Starting benchmarking mode


    return true;
} // end function: send_single_pkt_to_driver


// Send all available packets from given connection
// FIXME: It is quite bad policy as in one application can dominate
// TX path
static uint64_t send_packets_on_wire(struct net_queue_manager_binding *cc)
{
    uint64_t pkts = 0;
    struct client_closure *closure = (struct client_closure *)cc->st;
    assert(closure != NULL);
    struct buffer_descriptor *buffer = closure->buffer_ptr;
    assert(buffer != NULL);
    if (buffer->role == RX_BUFFER_ID) {
//        printf("####### ERROR: send_packets_on_wire called on wrong buff\n");
            return 0;
    }

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXESVSPOW,
                0);
#endif // TRACE_ONLY_SUB_NNET

    struct shared_pool_private *spp = closure->spp_ptr;
    assert(spp != NULL);
    assert(spp->sp != NULL);

    sp_reload_regs(spp);
    if (sp_queue_empty(spp)) {
        return 0;
    }
        // There are no packets
    while (send_single_pkt_to_driver(cc)) {
        sp_reload_regs(spp);
        ++pkts;
    }

    if (pkts > 0) {
        if (closure->debug_state_tx == 4) {
            netbench_record_event(bm, RE_TX_T, pkts);
        }
    }
    return pkts;
}

// function to do housekeeping after descriptors are transferred
bool handle_tx_done(struct net_queue_manager_binding * b, uint64_t spp_index)
{
    uint64_t rts = rdtsc();
    assert(b != NULL);
    struct client_closure *cc = (struct client_closure *)b->st;
    assert(cc != NULL);
    struct shared_pool_private *spp = cc->spp_ptr;
    assert(spp != NULL);
    assert(spp->sp != NULL);


    ETHERSRV_DEBUG("handle_tx_done called for %"PRIu64"\n", spp_index);

    if(spp->sp->read_reg.value != spp_index) {
        printf("handle_tx_done: read reg[%"PRIu64"] == "
               "spp_index [%"PRIu64"]\n",
               spp->sp->read_reg.value, spp_index);
//        abort();
    }
//    assert(spp->sp->read_reg.value == spp_index);

    if(!sp_set_read_index(spp, ((spp_index + 1) % spp->c_size))) {
        // FIXME:  This is dengarous!  I should increase read index,
        // only when all the packets till that read index are sent!
//        printf("failed for %"PRIu64"\n",spp_index);
//        sp_print_metadata(spp);
        abort();
        assert(!"sp_set_read_index failed");
    }

    if (spp->notify_other_side == 0) {

        if (cc->debug_state_tx == 4) {
            netbench_record_event_simple(bm, RE_TX_DONE_NN, rts);
        }
//        printf("handle_tx_done: sending no notifications!\n");
        return true;
    }

    if (cc->debug_state_tx == 4) {
        netbench_record_event_simple(bm, RE_TX_DONE_N, rts);
    }
    return true;
}// end function: handle_tx_done



// Do all the work related TX path for perticular client
// It includes
//   * Send all pending packets.
//   * Take care of descriptors which are sent.
//   * If needed, notifiy application.
static void do_pending_work(struct net_queue_manager_binding *b)
{
    uint64_t ts = rdtsc();
    struct client_closure *closure = (struct client_closure *)b->st;

    assert(closure != NULL);
    struct buffer_descriptor *buffer = closure->buffer_ptr;
    assert(buffer != NULL);
    struct shared_pool_private *spp = closure->spp_ptr;
    assert(spp != NULL);
    assert(spp->sp != NULL);

    // Check if there are more packets to be sent on wire
    uint64_t pkts = 0;
    pkts = send_packets_on_wire(b);
    if (closure->debug_state == 4) {
        netbench_record_event_simple(bm, RE_PENDING_1, ts);
    }

//    printf("do_pending_work: sent packets[%"PRIu64"]\n", pkts);
    // Check if there are more pbufs which are to be marked free
    if (pkts > 0) {
        uint64_t tts = rdtsc();
        while (handle_free_tx_slot_fn_ptr());

        if (sp_queue_full(spp)) {
            // app is complaining about TX queue being full
            // FIXME: Release TX_DONE
            while (handle_free_tx_slot_fn_ptr());
        }
        if (closure->debug_state == 4) {
            netbench_record_event_simple(bm, RE_PENDING_2, tts);
        }
    }

    send_notification_to_app(closure);

    if (closure->debug_state_tx == 4) {
        netbench_record_event_simple(bm, RE_TX_W_ALL, ts);
    }
}



// Do all the TX path related work for all the clients
void do_pending_work_for_all(void)
{
    struct buffer_descriptor *next_buf = buffers_list;
    while (next_buf != NULL) {
        do_pending_work(next_buf->con);
        next_buf = next_buf->next;
    }
}


// **********************************************************
// Functionality: packet receiving (RX path)
// **********************************************************

bool copy_packet_to_user(struct buffer_descriptor *buffer,
                         void *data, uint64_t len)
{
    assert(len > 0);
    assert(data != NULL);
    assert(buffer != NULL);
    struct net_queue_manager_binding *b = buffer->con;
    assert(b != NULL);
    struct client_closure *cl = (struct client_closure *) b->st;
    assert(cl != NULL);
    struct shared_pool_private *spp = cl->spp_ptr;
    assert(spp != NULL);

    sp_reload_regs(spp);
    if (sp_queue_full(spp)) {
        //printf
        ETHERSRV_DEBUG("[%d]no space in userspace 2cp pkt buf [%" PRIu64 "]: "
                "read[%"PRIu64"] write[%"PRIu64"]\n", disp_get_domain_id(),
                buffer->buffer_id, spp->c_read_id, spp->c_write_id);

        if (cl->debug_state == 4) {
            ++cl->in_dropped_app_buf_full;
        }
        return false;
    }

    uint64_t offset = spp->sp->slot_list[spp->c_write_id].d.offset;
    assert(offset < (1L << buffer->bits));
    void *dst = (void *) (uintptr_t) buffer->va + offset;

    ETHERSRV_DEBUG("Copy packet pos %p %p %p\n", buffer->va, dst,
                   (buffer->va + (1L << buffer->bits)));

    uint64_t ts = rdtsc();

    memcpy_fast((void *) (uintptr_t)dst, data, len);
    if (cl->debug_state == 4) {
        netbench_record_event_simple(bm, RE_COPY, ts);
    }

    // add trace pkt cpy
#if TRACE_ETHERSRV_MODE
    uint32_t pkt_location = (uint32_t) ((uintptr_t) data);

    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_PKT_CPY, pkt_location);
#endif // TRACE_ETHERSRV_MODE

#if TRACE_ONLY_SUB_NNET
    uint32_t pkt_location = (uint32_t) ((uintptr_t) data);
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXESVCOPIED,
                pkt_location);
#endif // TRACE_ONLY_SUB_NNET

    // update the length of packet in sslot.len field
    spp->sp->slot_list[spp->c_write_id].d.len = len;
    spp->sp->slot_list[spp->c_write_id].d.no_pbufs = 1;

    if(!sp_increment_write_index(spp)){
        printf("########### ERROR: cp_pkt_2_usr: sp_set_write_index failed\n");
        USER_PANIC("increment_write_index problem\n");
        abort();
        return false;
    }

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXESVSPPDONE,
                pkt_location);
#endif // TRACE_ONLY_SUB_NNET

    send_notification_to_app(cl);
    return true;
} // end function: copy_packet_to_user


/*****************************************************************
 * Interface related: Exporting and handling connections
 ****************************************************************/

static void export_ether_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "service [%s] export failed", exported_queue_name);
        abort();
    }

   // ETHERSRV_DEBUG
    printf("service [%s] exported at iref %"PRIu32"\n", exported_queue_name,
           (uint32_t)iref);

    // register this iref with the name service
    err = nameservice_register(exported_queue_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for [%s]",
                exported_queue_name);
        abort();
    }
}

static void error_handler(struct net_queue_manager_binding *b, errval_t err)
{
    ETHERSRV_DEBUG("ether service error_handler: called\n");
    if (err == SYS_ERR_CAP_NOT_FOUND) {
        struct client_closure *cc = b->st;

        assert(cc != NULL);
        struct buffer_descriptor *buffer = cc->buffer_ptr;

        assert(buffer != NULL);
        free(buffer);
        free(cc);
    }
    ETHERSRV_DEBUG("ether service error_handler: terminated\n");
}

static errval_t connect_ether_cb(void *st, struct net_queue_manager_binding *b)
{
    ETHERSRV_DEBUG("ether service got a connection!44\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_nqm_vtbl;
    b->error_handler = error_handler;

    // Create a new client for this connection
    struct client_closure *cc = create_new_client(b);
    if (cc == NULL) {
        return ETHERSRV_ERR_NOT_ENOUGH_MEM;
    }

    return SYS_ERR_OK;
} // end function: connect_ether_cb


/*****************************************************************
 * ethersrv initialization wrapper:
 * Equivalent of main function
 ****************************************************************/
void ethersrv_init(char *service_name, uint64_t queueid,
                   ether_get_mac_address_t get_mac_ptr,
                   ether_transmit_pbuf_list_t transmit_ptr,
                   ether_get_tx_free_slots tx_free_slots_ptr,
                   ether_handle_free_TX_slot handle_free_tx_slot_ptr)
{
    errval_t err;

    ETHERSRV_DEBUG("in the server_init\n");
    assert(service_name != NULL);
    assert(get_mac_ptr != NULL);
    assert(transmit_ptr != NULL);
    assert(tx_free_slots_ptr != NULL);
    assert(handle_free_tx_slot_ptr != NULL);

    exported_queueid = queueid;
    ether_get_mac_address_ptr = get_mac_ptr;
    ether_transmit_pbuf_list_ptr = transmit_ptr;
    tx_free_slots_fn_ptr = tx_free_slots_ptr;
    handle_free_tx_slot_fn_ptr = handle_free_tx_slot_ptr;
    snprintf(exported_queue_name, sizeof(exported_queue_name),
            "%s_%"PRIu64"", service_name, queueid);

    buffers_list = NULL;
    netd[0] = NULL;
    netd[1] = NULL;
    buffer_id_counter = 0;
    netd_buffer_count = 0;
    client_no = 0;


    uint8_t my_mac[6] = {0};
    ether_get_mac_address_ptr(my_mac);
    printf("############################################\n");
    printf("For service [%s] MAC= %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n",
                            service_name,  my_mac[0], my_mac[1], my_mac[2],
                             my_mac[3], my_mac[4], my_mac[5]);
    bm = netbench_alloc("DRV", EVENT_LIST_SIZE);

    /* FIXME: populate the receive ring of device driver with local pbufs */

    /* exporting ether interface */
    err = net_queue_manager_export(NULL, // state for connect/export callbacks
                       export_ether_cb, connect_ether_cb, get_default_waitset(),
                       IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "%s export failed", exported_queue_name);
        abort();
    }

    // start software filtering service
    init_soft_filters_service(service_name, queueid);
}


// **********************************************************
// Additional functions
// **********************************************************

// This function tells if netd is registered or not.
bool waiting_for_netd(void)
{
    return (netd_buffer_count < 2);
//    return ((netd[RECEIVE_CONNECTION] == NULL)
//            || (netd[TRANSMIT_CONNECTION] == NULL));
} // end function: is_netd_registered


// Optimzed memcpy function which chooses proper memcpy function automatically.
void *
memcpy_fast(void *dst0, const void *src0, size_t length)
{
//    return memcpy(dst0, src0, length);
#if defined(__scc__) && defined(SCC_MEMCPY)
    return memcpy_scc2(dst0, src0, length);
#else // defined(__scc__) && defined(SCC_MEMCPY)
    return memcpy(dst0, src0, length);
#endif // defined(__scc__) && defined(SCC_MEMCPY)
}

