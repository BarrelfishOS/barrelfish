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
#include <barrelfish/waitset_chan.h>
#include <devif/queue_interface.h>
#include <devif/backends/descq.h>

#include "QM_benchmark.h"
#include "queue_manager_debug.h"
#include "queue_manager_local.h"


/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif                          // CONFIG_TRACE && NETWORK_STACK_TRACE



// FIXME: This is repeated, make it common
#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have

#define QUEUE_SIZE 2048

/*****************************************************************
 * Global datastructure
 *****************************************************************/
// True iff we use software filtering
static bool use_sf;

// True iff we use the raw interface (implies !use_sf)
static bool use_raw_if = true;

// True if sofware filtering was disabled using the command-line parameter, this
// means that software filtering is not used, even if we are on queue 0.
static bool force_disable_sf = false;

struct netbench_details *bm = NULL; // benchmarking data holder

struct buffer_descriptor *buffers_list = NULL;

/*****************************************************************
 * Prototypes
 *****************************************************************/

// static errval_t register_buffer(struct net_queue_manager_binding *cc,
//         struct capref cap, struct capref sp, uint64_t queueid,
//         uint64_t slots, uint8_t role, struct capref queue_cap, uint64_t *idx);
// static void raw_add_buffer_signal(struct net_queue_manager_binding *cc,
//                            uint64_t offset, uint64_t length,
//                            uint64_t more, uint64_t flags);
// static void raw_add_buffer(struct client_closure *cl,
//                            uint64_t offset, uint64_t length,
//                            uint64_t more, uint64_t flags);
// static errval_t get_mac_addr_qm(struct net_queue_manager_binding *cc,
//         uint64_t queueid, uint64_t *mac);
// static void print_statistics_handler(struct net_queue_manager_binding *cc,
//         uint64_t queueid);
// static void terminate_queue(struct net_queue_manager_binding *cc);

static void do_pending_work(struct devq *q);

/*****************************************************************
 * VTABLE
 *****************************************************************/

// // Initialize service
// static struct net_queue_manager_rx_vtbl rx_nqm_vtbl = {
//     .raw_add_buffer = raw_add_buffer_signal,
//     .print_statistics = print_statistics_handler,
//     .benchmark_control_request = benchmark_control_request,
//     .terminate_queue = terminate_queue,
// };
//
// static struct net_queue_manager_rpc_rx_vtbl rpc_rx_nqm_vtbl = {
//     .register_buffer_call = register_buffer,
//     .get_mac_address_call = get_mac_addr_qm,
// };

/*****************************************************************
 * Pointers to driver functionalities:
 *****************************************************************/
static ether_terminate_queue ether_terminate_queue_ptr = NULL;
static ether_get_mac_address_t ether_get_mac_address_ptr = NULL;
static ether_transmit_pbuf_list_t ether_transmit_pbuf_list_ptr = NULL;
static ether_get_tx_free_slots tx_free_slots_fn_ptr = NULL;
static ether_handle_free_TX_slot handle_free_tx_slot_fn_ptr = NULL;
ether_rx_register_buffer rx_register_buffer_fn_ptr = NULL;
ether_rx_get_free_slots rx_get_free_slots_fn_ptr = NULL;

/*****************************************************************
 * Local states:
 *****************************************************************/
static char exported_queue_name[MAX_SERVICE_NAME_LEN] = {0}; // exported Q name
static uint64_t exported_queueid = 0; // id of queue
static size_t rx_buffer_size = 0;

// client_no used to give id's to clients
// WARN: should start at 0 as loopback table lookup depends on this assumption
static int client_no = 0;  // number of clients(apps) connected
static struct devq *all_apps[1024];

static uint64_t buffer_id_counter = 0; // number of buffers registered
// FIXME: following should be gone in next version of code
static uint64_t netd_buffer_count = 0; // way to identify the netd

// *************************************************************
//  local loopback device related code
// *************************************************************
// support for loopback device
bool is_loopback_device = false;

struct loopback_mapping{
    int tx_cl_no;   // sender client number
    int rx_cl_no;   // corrosponding rx client number
    struct buffer_descriptor *lo_rx_buf;
};
// We currently support only two applications on loopback device.
static struct loopback_mapping lo_map_tbl[4] = {{0,0, NULL},};

// to ensure that lo_map indexes will work irrespective of
// client_no initialization value
static int lo_tbl_idx = 0;

// fill up the lo_map_tbl with valid entries
// Assumptions:
//  * client_no starts at 0
//  * First connection is RX (ie 0) and second is TX (ie 1)
static void populate_lo_mapping_table(int cur_cl_no,
            struct buffer_descriptor *buffer_ptr)
{
    // sanity checks

    printf("populate called for client %d\n", cur_cl_no);
    assert(is_loopback_device); // ensuring loopback device
    assert(cur_cl_no == lo_tbl_idx); // ensure monotonic increase
    assert(lo_tbl_idx < 4); // we currently support only 2 applications for lo

    // and I should validate the buffer types
    assert(RX_BUFFER_ID == 0); // ensure RX is 0

    if((lo_tbl_idx % 4) != (buffer_ptr->role)) {
        printf(" tlb_idx %d, role %"PRIu8"\n", lo_tbl_idx, buffer_ptr->role);
    }
    assert((lo_tbl_idx % 4) == (buffer_ptr->role));

    // populate the table entries
    lo_map_tbl[lo_tbl_idx].tx_cl_no = -1;
    lo_map_tbl[lo_tbl_idx].rx_cl_no = -1;
    lo_map_tbl[lo_tbl_idx].lo_rx_buf = buffer_ptr;

    switch(lo_tbl_idx) {
        case 0:
        case 2:
            // intermediate state, nothing to do!
            break;

        case 1:
            printf("populate case 1 crossing 0 %d\n", cur_cl_no);
            // Assuming only one app, so mapping tx to rx
            lo_map_tbl[lo_tbl_idx].tx_cl_no = cur_cl_no;
            lo_map_tbl[lo_tbl_idx].rx_cl_no = 0;
            lo_map_tbl[lo_tbl_idx].lo_rx_buf = lo_map_tbl[0].lo_rx_buf;
            break;

        case 3:
            // Assuming are two apps, so mapping them

            // mapping 3 to 0
            lo_map_tbl[lo_tbl_idx].tx_cl_no = cur_cl_no;
            lo_map_tbl[lo_tbl_idx].rx_cl_no = 0;
            lo_map_tbl[lo_tbl_idx].lo_rx_buf = lo_map_tbl[0].lo_rx_buf;

            // mapping 1 to 2
            lo_map_tbl[1].tx_cl_no = 1;
            lo_map_tbl[1].rx_cl_no = 2;
            lo_map_tbl[1].lo_rx_buf = lo_map_tbl[2].lo_rx_buf;
            break;

        default:
            USER_PANIC("More than two clients are not supported for lo");
            abort();
            break;
    } // end switch:

    ++lo_tbl_idx;
} // end function: populate_lo_mapping_table


struct buffer_descriptor *get_lo_receiver(void *opaque)
{
    // making sure that this is indeed loopback device
    assert(is_loopback_device);
    assert(opaque != NULL);

    struct buffer_state_metadata *bsm = opaque;

    struct devq *q = bsm->device_queue;

    // find client_no of sending app
    assert(q != NULL);
    struct client_closure *cc = devq_get_state(q);
    assert(cc != NULL);

    int cl_no = cc->cl_no;

    // Make sure that cc->cl_no is valid for lo_map table
    assert(lo_map_tbl[cl_no].rx_cl_no != -1);

    // get buffer_ptr from the lookup table
    return (lo_map_tbl[cl_no].lo_rx_buf);
} // end function: get_lo_receiver

// *************************************************************
// Client and buffer management code
// *************************************************************



// Creates a new client for given connection
static struct client_closure *create_new_client(struct devq *q, uint8_t role, uint64_t *queue_id)
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
    
    devq_set_state(q, cc);
    cc->app_connection = q;

    // FIXME: I should not need this as now netd is normal app
    // save it if it is netd app
    if (client_no < 2) {
        netd[client_no] = q;
    }

    cc->buffer_ptr = buffer;
    cc->debug_state = 0;        // Default debug state : no debug
    reset_client_closure_stat(cc);
    cc->start_ts = rdtsc();

    all_apps[client_no] = q;

    cc->cl_no = client_no++;
    cc->role = role;
    buffer_id_counter++;
    cc->buffer_id = buffer_id_counter;
    *queue_id = buffer_id_counter;
    char name[64];
    sprintf(name, "ether_a_%d_%s", cc->cl_no,
                ((cc->cl_no % 2) == 0)? "RX" : "TX");
    return cc;
} // end function: create_new_client

// populates the given buffer with given capref
static errval_t populate_buffer(struct client_closure *closure,
        struct buffer_descriptor *buffer, struct capref cap)
{

    buffer->cap = cap;
    struct frame_identity pa;
    errval_t err = frame_identify(cap, &pa);
    if (!err_is_ok(err)) {
        printf("frame_identify failed\n");
        abort();
    }
    buffer->pa = pa.base;
    buffer->bytes = pa.bytes;

    err = vspace_map_one_frame(&buffer->va, buffer->bytes, cap,
            NULL, NULL);

/*
    err = vspace_map_one_frame_attr(&buffer->va, (1L << buffer->bits), cap,
                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
*/

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        // FIXME: report more sensible error
        return(ETHERSRV_ERR_TOO_MANY_BUFFERS);
    }
    netd_buffer_count++;
    buffer->buffer_id = closure->buffer_id;
//    printf("### buffer gets id %"PRIu64"\n", buffer->buffer_id);
    buffer->next = buffers_list;
    // Adding the buffer on the top of buffer list.
//    buffers_list = buffer;
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

// register region
static errval_t register_region(struct descq* q, struct capref cap,
                    regionid_t rid)
{
    debug_printf("Register \n");
// Actual register_buffer function with all it's logic
    ETHERSRV_DEBUG("ethersrv:register buffer called \n");
    errval_t err;
    struct client_closure *closure = (struct client_closure *)devq_get_state((struct devq *)q);
    closure->queueid = exported_queueid;
    closure->region_id = rid;
    
    struct buffer_descriptor *buffer = closure->buffer_ptr;
    err = populate_buffer(closure, buffer, cap);
    assert(err_is_ok(err));

    buffer->role = closure->role;
    buffer->device_queue = (struct devq *)q;
    buffer->queueid = exported_queueid;

    struct capability capability;
    err = debug_cap_identify(cap, &capability);
    assert(err_is_ok(err));
    assert(capability.type == ObjType_Frame);

    // Create a list to hold metadata for sending
    uint64_t slots = buffer->bytes / 2048;
    buffer->rxq.buffer_state_size = slots;
    buffer->rxq.buffer_state = calloc(slots,
            sizeof(struct buffer_state_metadata));
    assert(buffer->rxq.buffer_state != NULL);
    buffer->rxq.buffer_state_head = 0;
    buffer->rxq.buffer_state_used = 0;

    // Create a list to hold metadata for receiving
    buffer->txq.buffer_state_size = slots;
    buffer->txq.buffer_state = calloc(slots,
            sizeof(struct buffer_state_metadata));
    assert(buffer->txq.buffer_state != NULL);
    buffer->txq.buffer_state_head = 0;
    buffer->txq.buffer_state_used = 0;

    if (is_loopback_device) {
        populate_lo_mapping_table(closure->cl_no, closure->buffer_ptr);
    } // end if: loopback device

    // Use raw interface if desired
    printf("net_queue_manager: Use raw interface\n");
    use_raw_if = true;

    buffers_list = buffer;

    return SYS_ERR_OK;
} // end function: register_buffer


static __attribute__((unused))  void
handle_single_event_nonblock(struct waitset *ws)
{
    errval_t err;

    while (1) {

        do_pending_work_for_all();

        err = event_dispatch_non_block(ws); // nonblocking for polling mode
        if (err != LIB_ERR_NO_EVENT && err_is_fail(err)) {
            ETHERSRV_DEBUG("Error in event_dispatch_non_block, returned %d\n",
                        (unsigned int)err);
            // There should be a serious panic and failure here
            USER_PANIC_ERR(err, "event_dispatch_non_block failed in handle_single_event\n");
            break;
        } else {
            // Successfully handled the event
           return;
        }
    } // end while: infinite
} // end function: handle_single_event_nonblock


static errval_t send_raw_xmit_done(struct devq *queue,
                                   uint64_t offset, uint64_t length,
                                   uint64_t more, uint64_t flags)
{
    if (flags & NETIF_TXFLAG) {
        ETHERSRV_DEBUG("Sending TX buf on queue\n");
    } else {
        ETHERSRV_DEBUG("Sending RX buf on queue\n");
    }
    struct client_closure *cl = (struct client_closure *)devq_get_state(queue);
    errval_t err;
    err = devq_enqueue(queue, cl->region_id, offset, length, 0, length, flags);
    assert(err_is_ok(err));
    err = devq_notify(queue);
    return err;
} // end function: send_raw_xmit_done

// *********** Interface: get_mac_address ****************

// wrapper function for responce
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

// *********** Interface: print_statistics ****************

// FIXME: NYI


// **********************************************************
// Functionality: packet sending (TX path)
// **********************************************************

// function to do housekeeping after descriptors are transferred
bool handle_tx_done(void *opaque)
{

    assert(opaque != NULL);
    struct buffer_state_metadata *bsm = opaque;
    struct client_closure *cl = devq_get_state(bsm->device_queue);
    struct buffer_descriptor *buffer = cl->buffer_ptr;

    assert((buffer->txq.buffer_state_used > 0));
    --buffer->txq.buffer_state_used;

    // Handle raw interface
    errval_t err = send_raw_xmit_done(bsm->device_queue, (uintptr_t)bsm->offset, 0,
            0, NETIF_TXFLAG | NETIF_TXFLAG_LAST);
    if (err_is_ok(err)) {
        return true;
    } else {
        printf("handle_tx_done failed for client_no %d\n", cl->cl_no);
        return false;
    }

}// end function: handle_tx_done


// Do all the work related TX path for perticular client
// It includes
//   * Take care of descriptors which are sent.
static void do_pending_work(struct devq *queue)
{
    // Handle raw interface
    if (use_raw_if) {
        while (handle_free_tx_slot_fn_ptr());
        return;
    }

} // end function: do_pending_work


// Do all the TX path related work for all the clients
void do_pending_work_for_all(void)
{
    struct buffer_descriptor *next_buf = buffers_list;
    while (next_buf != NULL) {
        do_pending_work(next_buf->device_queue);
        next_buf = next_buf->next;
    }
}

/**
 * Called by driver when it receives a new packet.
 */
void process_received_packet(struct driver_rx_buffer* bufs, size_t count,
        uint64_t flags)
{
    size_t i;
    if (use_sf) {
        // FIXME: this is broken quite badly
        sf_process_received_packet(bufs, count, flags);
        return;
    }
    // If we do no software filtering we basically only have to tell the
    // application that a new packet is ready.

    // Handle raw interface
    if (use_raw_if) {
        for (i = 0; i < count; i++) {
            assert(bufs[i].opaque != NULL);
            struct buffer_state_metadata *bsm = bufs[i].opaque;
            struct client_closure *cl = devq_get_state(bsm->device_queue);
            struct buffer_descriptor *buf = cl->buffer_ptr;
            assert(buf->rxq.buffer_state_used > 0);

            errval_t err = send_raw_xmit_done(bsm->device_queue, bsm->offset,
                    bufs[i].len, (i != count - 1), flags | NETIF_RXFLAG);
            if (err_is_ok(err)) {
                --buf->rxq.buffer_state_used;
                return;
            } else {
                // As application is not able to process the packet
                // we will drop this one
                USER_PANIC("send_raw_xmit_done failed as queue full, can't go further: 1\n");
                // FIXME: Don't crash. figure out how can you drop the packet
                // and continue working after dropping the packet
                --buf->rxq.buffer_state_used;
                return;
            }
        }
    }
} // end function: process_received_packet

static uint64_t rx_added = 0; // FIXME: for debugging. Remove this
static uint64_t sent_packets = 0; // FIXME: remove this
/**
 * Used in combination with software filtering, to copy a packet into a user
 * buffer.
 */
// FIXME: why is this not in soft filter management?
bool copy_packet_to_user(struct buffer_descriptor *buffer,
                         void *data, uint64_t len, uint64_t flags)
{
    // Must only be called if we use software filtering
    assert(use_sf);

    assert(len > 0);
    assert(data != NULL);
    assert(buffer != NULL);
    struct devq *q = buffer->device_queue;
    assert(q != NULL);
    struct client_closure *cl = devq_get_state(q);
    assert(cl != NULL);

    // check if there are slots which can be used in app (!isempty)
    if(buffer->rxq.buffer_state_used == 0) {
        printf("[%s] Dropping packet as no space in userspace "
                "2cp pkt buf [%" PRIu64 "]: "
                "size[%zu] used[%zu], after [%"PRIu64"] sent"
                " added [%"PRIu64"] \n", disp_name(),
                buffer->buffer_id, buffer->rxq.buffer_state_size,
                buffer->rxq.buffer_state_used, sent_packets, rx_added);
        if (cl->debug_state == 4) {
            ++cl->in_dropped_app_buf_full;
        }
        //abort(); // optional, should be removed
        return false;
    }

    // pop the latest buffer from head of queue (this is stack)
    --buffer->rxq.buffer_state_head;
    struct buffer_state_metadata *bsm = buffer->rxq.buffer_state +
        buffer->rxq.buffer_state_head;
    assert(bsm != NULL);
    uint64_t offset = bsm->offset;
    --buffer->rxq.buffer_state_used;

    assert(offset < buffer->bytes);
    void *dst = (void *) (uintptr_t) buffer->va + offset;

    ETHERSRV_DEBUG("Copy packet pos %p %p %p\n", buffer->va, dst,
                   (buffer->va + buffer->bytes));

    uint64_t ts = rdtsc();

    memcpy_fast((void *) (uintptr_t)dst, data, len);
    if (cl->debug_state == 4) {
        netbench_record_event_simple(bm, RE_COPY, ts);
    }

    ++sent_packets; // FIXME: remove this!
    // add trace pkt cpy
#if TRACE_ETHERSRV_MODE
    uint32_t pkt_location = (uint32_t) ((uintptr_t) data);

    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_NI_PKT_CPY, pkt_location);
#endif // TRACE_ETHERSRV_MODE

    // Handle raw interface
    errval_t err = send_raw_xmit_done(q, offset, len, 0, flags | NETIF_RXFLAG);
    if (err_is_ok(err)) {
        return true;
    } else {
        // As application is not able to process the packet
        // we will drop this one
        USER_PANIC("send_raw_xmit_done failed as queue full, can't go further: 2\n");
        // FIXME: Don't crash. ignore the packet, undo any changes done by it
        // and continue.  Ideally this shouldn't happen as we are checking for
        // free space before actually sending the packt.
        return false;
    }

    return true;
} // end function: copy_packet_to_user

/*****************************************************************
 * Interface related: raw interface
 ****************************************************************/
static void raw_add_buffer_tx(struct client_closure *cl,
                           uint64_t offset, uint64_t length,
                           uint64_t more, uint64_t flags)
{
    struct buffer_descriptor *buffer = cl->buffer_ptr;
    errval_t err;
    uint64_t paddr;
    void *vaddr, *opaque;

    paddr = ((uint64_t)(uintptr_t) buffer->pa) + offset;
    vaddr = (void*) ((uintptr_t) buffer->va + (size_t)offset);

    // debug_printf("%s: %p: %lx:%ld\n", __func__, cc, offset, length);
    // Make sure that there is opaque slot available (isfull)
    assert(buffer->txq.buffer_state_used < (buffer->txq.buffer_state_size - 1));

    // Save state for handle_tx_done()/handle_receive_packet
    struct buffer_state_metadata *bsm = buffer->txq.buffer_state +
        buffer->txq.buffer_state_head;
    buffer->txq.buffer_state_head = (buffer->txq.buffer_state_head + 1)
        % buffer->txq.buffer_state_size;
    bsm->device_queue = cl->app_connection;
    bsm->offset = offset;
    ++buffer->txq.buffer_state_used;

    opaque = (void*)bsm;

    // save information as list of packet-chunks before sending to HW
    cl->driver_buff_list[cl->chunk_counter].va = vaddr;
    cl->driver_buff_list[cl->chunk_counter].pa = paddr;
    cl->driver_buff_list[cl->chunk_counter].len = length;
    cl->driver_buff_list[cl->chunk_counter].opaque = opaque;
    cl->driver_buff_list[cl->chunk_counter].flags = flags;
    ++cl->chunk_counter;
    if (more == 0) {
        // ETHERSRV_DEBUG
//            printf("sending out packet\n");
        if (cl->chunk_counter > 1) {
            ETHERSRV_DEBUG
            //printf
                ("%s:%s: handle=%p\n", disp_name(), __func__,
                    opaque);
        }
        err = ether_transmit_pbuf_list_ptr(cl->driver_buff_list,
                cl->chunk_counter);
        assert(err_is_ok(err));
        cl->chunk_counter = 0;
    }
} // end function: raw_add_buffer

static void raw_add_buffer_rx(struct client_closure *cl,
                           uint64_t offset, uint64_t length,
                           uint64_t more, uint64_t flags)
{
    struct buffer_descriptor *buffer = cl->buffer_ptr;
    uint64_t paddr;
    void *vaddr, *opaque;

    paddr = ((uint64_t)(uintptr_t) buffer->pa) + offset;
    vaddr = (void*) ((uintptr_t) buffer->va + (size_t)offset);

    // Sanity check.  Making sure that more flag is not set
    if (more == 1) {
        USER_PANIC("broken buffer registerd with for RX buffer\n");
    }

    // Make sure that there is opaque slot available (isfull)
    assert(buffer->rxq.buffer_state_used <
            (buffer->rxq.buffer_state_size - 1));

    // Save state for handle_tx_done()/handle_receive_packet
    struct buffer_state_metadata *bsm = buffer->rxq.buffer_state +
        buffer->rxq.buffer_state_head;
    buffer->rxq.buffer_state_head = (buffer->rxq.buffer_state_head + 1)
        % buffer->rxq.buffer_state_size;
    bsm->device_queue = cl->app_connection;
    bsm->offset = offset;
    ++buffer->rxq.buffer_state_used;
    ++rx_added;
    opaque = (void*)bsm;

    // role == RX_BUFFER_ID
    if (use_sf) {
        // nothing to do!
    } else {
        assert(length == rx_buffer_size);
        rx_register_buffer_fn_ptr(paddr, vaddr, opaque);
    }
    // FIXME: send a message back acking receiving of message.
} // end function: raw_add_buffer

static errval_t notify_queue(struct descq *queue)
{
    struct client_closure *cl = devq_get_state((struct devq *)queue);

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

        if (flags & NETIF_TXFLAG) {
            // debug_printf("notify_queue_tx: %p: %d:%lx:%ld\n", queue, rid, offset, length);
            raw_add_buffer_tx(cl, offset, length, 0, flags);
        } else {
            // debug_printf("notify_queue_rx: %p: %d:%lx:%ld\n", queue, rid, offset, length);
            raw_add_buffer_rx(cl, offset, length, 0, flags);
        }
    }
    return SYS_ERR_OK;
}

/*****************************************************************
 * Interface related: Exporting and handling connections
 ****************************************************************/

// static void export_ether_cb(void *st, errval_t err, iref_t iref)
// {
//     if (err_is_fail(err)) {
//         DEBUG_ERR(err, "service [%s] export failed", exported_queue_name);
//         abort();
//     }
//
//    // ETHERSRV_DEBUG
//     debug_printf("service [%s] exported at iref %"PRIu32"\n", exported_queue_name,
//            (uint32_t)iref);
//
//     // register this iref with the name service
//     debug_printf("%s: nqm export [%s]\n", __func__, exported_queue_name);
//     err = nameservice_register(exported_queue_name, iref);
//     if (err_is_fail(err)) {
//         DEBUG_ERR(err, "nameservice_register failed for [%s]",
//                 exported_queue_name);
//         abort();
//     }
//     debug_printf("service [%s] registered\n", exported_queue_name);
// }
//
// static void error_handler(struct net_queue_manager_binding *b, errval_t err)
// {
//     ETHERSRV_DEBUG("ether service error_handler: called\n");
//     if (err == SYS_ERR_CAP_NOT_FOUND) {
//         struct client_closure *cc = b->st;
//
//         assert(cc != NULL);
//         struct buffer_descriptor *buffer = cc->buffer_ptr;
//
//         assert(buffer != NULL);
//         free(buffer);
//         free(cc);
//     }
//     ETHERSRV_DEBUG("ether service error_handler: terminated\n");
// }

//static errval_t connect_ether_cb(void *st, struct net_queue_manager_binding *b)
static errval_t create_queue(struct descq* q, bool notifications, uint8_t role, uint64_t *queue_id)
{
    ETHERSRV_DEBUG("ether service got a connection!\n");

    // copy my message receive handler vtable to the binding
    // b->rx_vtbl = rx_nqm_vtbl;
    // b->rpc_rx_vtbl = rpc_rx_nqm_vtbl;
    // b->error_handler = error_handler;
    // b->control(b, IDC_CONTROL_CLEAR_SYNC);

    // Create a new client for this connection
    struct client_closure *cc = create_new_client((struct devq *)q, role, queue_id);
    debug_printf("%s: %p regid:%ld\n", __func__, q, *queue_id);
    if (cc == NULL) {
        return ETHERSRV_ERR_NOT_ENOUGH_MEM;
    }

    return SYS_ERR_OK;
} // end function: connect_ether_cb

static errval_t destroy_queue(struct descq* q)
{
    debug_printf("Destroy \n");
    return SYS_ERR_OK;
}

static errval_t deregister_region(struct descq* q, regionid_t rid)
{
    debug_printf("Deregister \n");
    return SYS_ERR_OK;
}

static errval_t control_queue(struct descq* q, uint64_t cmd, uint64_t value, uint64_t *result)
{
    debug_printf("Control \n");
    if (cmd == 0) // get mac
        *result = get_mac_addr_from_device();

    return SYS_ERR_OK;
}



/*****************************************************************
 * ethersrv initialization wrapper:
 * Equivalent of main function
 ****************************************************************/
void ethersrv_init(char *service_name, uint64_t queueid,
                   ether_get_mac_address_t get_mac_ptr,
                   ether_terminate_queue terminate_queue_ptr,
                   ether_transmit_pbuf_list_t transmit_ptr,
                   ether_get_tx_free_slots tx_free_slots_ptr,
                   ether_handle_free_TX_slot handle_free_tx_slot_ptr,
                   size_t rx_bufsz,
                   ether_rx_register_buffer rx_register_buffer_ptr,
                   ether_rx_get_free_slots rx_get_free_slots_ptr)
{
    errval_t err;

    ETHERSRV_DEBUG("in the server_init\n");
    assert(service_name != NULL);
    assert(get_mac_ptr != NULL);
    assert(transmit_ptr != NULL);
    assert(tx_free_slots_ptr != NULL);
    assert(handle_free_tx_slot_ptr != NULL);
    assert(rx_register_buffer_ptr != NULL);
    assert(rx_get_free_slots_ptr != NULL);

    exported_queueid = queueid;
    rx_buffer_size = rx_bufsz;
    ether_terminate_queue_ptr = terminate_queue_ptr;
    ether_get_mac_address_ptr = get_mac_ptr;
    ether_transmit_pbuf_list_ptr = transmit_ptr;
    tx_free_slots_fn_ptr = tx_free_slots_ptr;
    handle_free_tx_slot_fn_ptr = handle_free_tx_slot_ptr;
    rx_register_buffer_fn_ptr = rx_register_buffer_ptr;
    rx_get_free_slots_fn_ptr = rx_get_free_slots_ptr;
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

    size_t driver_supported_buffers = tx_free_slots_fn_ptr();
    printf("using %zd slots for internal buffer\n", driver_supported_buffers);
    assert(driver_supported_buffers >= 1);

    debug_printf("%s: exporting\n", __func__);
    
    /* exporting ether interface */

    struct descq_func_pointer f;

    f.notify = notify_queue;
    f.create = create_queue;
    f.destroy = destroy_queue;
    f.reg = register_region;
    f.dereg = deregister_region;
    f.control = control_queue;
    
    struct descq* exp_queue;
    uint64_t queue_id;
    
    debug_printf("Creating queue...\n");
    err = descq_create(&exp_queue, DESCQ_DEFAULT_SIZE, exported_queue_name,
                       true, true, 0, &queue_id, &f);

    assert(err_is_ok(err));

    debug_printf("Queue created\n");

    // FIXME: How do we decide this reasonably
    use_sf = !force_disable_sf && (queueid == 0);

    if (use_sf || is_loopback_device) {
        // start software filtering service
        struct net_soft_filter_state *state;
        
        state = malloc(sizeof(struct net_soft_filter_state));
        waitset_chanstate_init(&state->initialization_completed, CHANTYPE_OTHER);
        state->initialization_completed.trigger = get_monitor_binding_chanstate();
        err = waitset_chan_register(get_default_waitset(), &state->initialization_completed,
                               NOP_CLOSURE);
        assert(err_is_ok(err));
        
        init_soft_filters_service(state, service_name, queueid, rx_bufsz);
        
        errval_t err2;
        err2 = SYS_ERR_OK;
        err = wait_for_channel(get_default_waitset(), &state->initialization_completed, &err2);
        assert(err_is_ok(err));
        assert(err_is_ok(err2));
    }
} // end function: ethersrv_init

void ethersrv_argument(const char* arg)
{
    static uint64_t minbase = -1ULL;
    static uint64_t maxbase = -1ULL;
    static bool affinity_set = false;

    if (!strncmp(arg, "affinitymin=", strlen("affinitymin="))) {
        minbase = atol(arg + strlen("affinitymin="));
    } else if(!strncmp(arg, "affinitymax=", strlen("affinitymax="))) {
        maxbase = atol(arg + strlen("affinitymax="));
    } else if (!strncmp(arg, "disable_sf=", strlen("disable_sf="))) {
        force_disable_sf = !!atol(arg + strlen("disable_sf="));
    }

    if (!affinity_set && minbase != -1ULL && maxbase != -1ULL) {
        ram_set_affinity(minbase, maxbase);
        affinity_set = true;
    }
}

// static void terminate_queue(struct net_queue_manager_binding *cc)
// {
//     errval_t err;
//     struct buffer_descriptor *buffer;
//
//     // Free buffers
//     for (buffer = buffers_list; buffer != NULL; buffer = buffer->next) {
//         err = vspace_unmap(buffer->va);
//         assert(err_is_ok(err));
//         err = cap_delete(buffer->cap);
//         assert(err_is_ok(err));
//     }
//
//     assert(ether_terminate_queue_ptr != NULL);
//     ether_terminate_queue_ptr();
// }

// **********************************************************
// Additional functions
// **********************************************************

// This function tells if netd is registered or not.
bool waiting_for_netd(void)
{
    return (netd_buffer_count < 1);
//    return ((netd[RECEIVE_CONNECTION] == NULL)
//            || (netd[TRANSMIT_CONNECTION] == NULL));
} // end function: is_netd_registered


// Optimzed memcpy function which chooses proper memcpy function automatically.
void *
memcpy_fast(void *dst0, const void *src0, size_t length)
{
    return memcpy(dst0, src0, length);
}
