/**
 * \file
 * \brief Generic server part for most ethernet drivers.
 * Current drivers using this server code are
 * -- e1000n
 * -- rtl8029
 * -- eMAC
 */

/*
 * Copyright (c) 2007-11 ETH Zurich.
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
#include <ethersrv/ethersrv.h>
#include <if/ether_defs.h>
#include "ethersrv_local.h"
#include "ethersrv_debug.h"

#define APP_QUEUE_SIZE  (RECEIVE_BUFFERS + 1)

#define USE_SPP_FOR_TX_DONE 1  // FIXME: I should not need this anymore

/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif                          // CONFIG_TRACE && NETWORK_STACK_TRACE

/*****************************************************************
 * Constants:
 *****************************************************************/

#define LAST_ACCESSED_BYTE_ARP 12
#define LAST_ACCESSED_BYTE_TRANSPORT 36


// Measurement purpose, counting interrupt numbers
uint64_t interrupt_counter = 0;
uint64_t interrupt_loop_counter = 0;

/* ethersrv specific debug state indicator */
static int debug_state = 0;

// Counter for dropped packets
static uint64_t dropped_pkt_count = 0;

/*****************************************************************
 * Prototypes
 *****************************************************************/

static void register_buffer(struct ether_binding *cc, struct capref cap,
                        struct capref sp, uint64_t slots, uint8_t role);
static void sp_notification_from_app(struct ether_binding *cc, uint64_t type,
                uint64_t ts);

static void get_mac_addr(struct ether_binding *cc);
static void print_statistics_handler(struct ether_binding *cc);
static void print_cardinfo_handler(struct ether_binding *cc);
static void benchmark_control_request(struct ether_binding *cc, uint8_t state,
        uint64_t trigger, uint64_t cl);

/*****************************************************************
 * VTABLE
 *****************************************************************/

// Initialize service
static struct ether_rx_vtbl rx_ether_vtbl = {
    .register_buffer = register_buffer,
    .sp_notification_from_app = sp_notification_from_app,
    .get_mac_address = get_mac_addr,
    .print_statistics = print_statistics_handler,
    .print_cardinfo = print_cardinfo_handler,
    .benchmark_control_request = benchmark_control_request,
};

/*****************************************************************
 * Pointers to driver functionalities:
 *****************************************************************/
static ether_get_mac_address_t ether_get_mac_address_ptr = NULL;

//static ether_can_transmit_t ether_can_transmit_ptr = NULL;
static ether_transmit_pbuf_list_t ether_transmit_pbuf_list_ptr = NULL;
static ether_get_tx_free_slots tx_free_slots_fn_ptr = NULL;
static ether_handle_free_TX_slot handle_free_tx_slot_fn_ptr = NULL;

/*****************************************************************
 * Local states:
 *****************************************************************/
static char *my_service_name = NULL;
static int client_no = 0;
static uint64_t buffer_id_counter = 0;
static uint64_t netd_buffer_count = 0;

static struct buffer_descriptor *first_app_b = NULL;

static uint64_t my_avg(uint64_t sum, uint64_t n) {
    if (n == 0) {
        return sum;
    } else {
        return (sum/n);
    }
}

static uint64_t metadata_size = sizeof(struct pbuf_desc) * APP_QUEUE_SIZE;

static void print_app_stats(struct buffer_descriptor *buffer)
{
    uint16_t i = 0;
    uint64_t avg = 0;
    uint64_t sd = 0;

    uint64_t n_sum = 0;
    uint64_t stat_sum = 0;
    uint64_t stat_min = 0;
    uint64_t stat_max = 0;

    struct pbuf_desc *pbuf = (struct pbuf_desc *) (buffer->pbuf_metadata_ds);


    uint8_t et = PBUF_REGISTERED;  // event type
    for (i = 0; i < RECEIVE_BUFFERS; ++i){
        avg = my_avg(pbuf[i].event_sum[et], pbuf[i].event_n[et]);
        sd = (pbuf[i].event_sum2[et] - (my_avg(pbuf[i].event_sum2[et], avg)) )/
            (pbuf[i].event_n[et] - 1);
/*
        printf("pbuf %"PRIu16": N[%"PRIu64"], AVG[%"PRIu64"], SD[%"PRIu64"],"
               "MAX[%"PRIu64"], MIN[%"PRIu64"]\n",
                i, pbuf[i].event_n[et], avg, sd,
                pbuf[i].event_max[et], pbuf[i].event_min[et]);
*/

        // Averaging above stats
        n_sum += pbuf[i].event_n[et];
        uint64_t to_avg = avg;
        stat_sum += to_avg;
        if (i == 0) {
            stat_min = to_avg;
            stat_max = to_avg;
        } else {
            if (to_avg > stat_max) {
                stat_max = to_avg;
            }
            if (to_avg < stat_min) {
                stat_min = to_avg;
            }
        }
    } // end for
/*
    printf("For %s (%"PRIu8"): N[%"PRIu64"], AVG[%"PRIu64"],"
            "MAX[%"PRIu64"], MIN[%"PRIu64"]\n",
            "PBUF_REGISTER", et, (n_sum / (RECEIVE_BUFFERS)),
            (stat_sum/RECEIVE_BUFFERS),
            (stat_max), (stat_min));
*/

    printf("For %s (%"PRIu8"): N[%"PRIu64"], AVG[%"PU"],"
            "MAX[%"PU"], MIN[%"PU"]\n",
            "PBUF_REGISTER", et, my_avg(n_sum, (RECEIVE_BUFFERS)),
          in_seconds(my_avg(stat_sum, (RECEIVE_BUFFERS))),
          in_seconds(stat_max), in_seconds(stat_min));
} // end function: reset_stats



static void reset_app_stats(struct buffer_descriptor *buffer)
{
    int i = 0;
    struct pbuf_desc *pbuf = (struct pbuf_desc *) (buffer->pbuf_metadata_ds);

    for (i = 0; i < RECEIVE_BUFFERS; ++i){
        for (int j = 0; j < MAX_STAT_EVENTS; ++j) {
            pbuf[i].event_ts[j] = 0;
            pbuf[i].event_n[j] = 0;
            pbuf[i].event_sum[j] = 0;
            pbuf[i].event_sum2[j] = 0;
            pbuf[i].event_max[j] = 0;
            pbuf[i].event_min[j] = 0;
            pbuf[i].event_sum_i[j] = 0;
            pbuf[i].event_sum2_i[j] = 0;
            pbuf[i].event_max_i[j] = 0;
            pbuf[i].event_min_i[j] = 0;
        }
    } // end for
} // end function: reset_stats


static void add_event_stat(struct pbuf_desc *pbuf_d, int event_type)
{
    uint64_t ts = rdtsc();
    uint64_t delta = 0;
//    uint64_t delta_i = 0;
    if(pbuf_d->event_n[event_type] > 0) {
        delta = ts - pbuf_d->event_ts[event_type];
    }
    pbuf_d->event_sum[event_type] += delta;
    pbuf_d->event_sum2[event_type] += ( delta * delta);

    // Recording max, min
    if(pbuf_d->event_n[event_type] == 1) {
        pbuf_d->event_max[event_type] = delta;
        pbuf_d->event_min[event_type] = delta;
    } else {
        if (delta > pbuf_d->event_max[event_type]) {
            pbuf_d->event_max[event_type] = delta;
        }
        if (delta < pbuf_d->event_min[event_type]) {
            pbuf_d->event_max[event_type] = delta;
        }
    }
    ++pbuf_d->event_n[event_type];
    pbuf_d->event_ts[event_type] = ts;
    // FIXME: collect stats for incremental events as well
}

static void reset_client_closure_stat(struct client_closure *cc)
{
    cc->start_ts = 0;
    cc->start_ts_tx = 0;
    cc->pkt_count = 0;
    cc->hw_queue = 0;
    cc->tx_explicit_msg_needed = 0;
    cc->tx_notification_sent = 0;
    cc->dropped_pkt_count = 0;
    cc->in_dropped_q_full = 0;
    cc->in_dropped_invalid_pkt = 0;
    cc->in_dropped_no_app = 0;
    cc->in_dropped_app_buf_full = 0;
    cc->in_dropped_app_invalid_buf = 0;
    cc->in_dropped_notification_prob = 0;
    cc->in_dropped_notification_prob2 = 0;
    cc->tx_done_count = 0;
    cc->in_dropped_q_full = 0;
    cc->in_success = 0;
    cc->in_trigger_counter = 0;
    cc->out_trigger_counter = 0;
    cc->filter_matched = 0;
    cc->in_other_pkts = 0;
    cc->in_arp_pkts = 0;
    cc->in_netd_pkts = 0;
    cc->in_paused_pkts = 0;
    cc->in_filter_matched = 0;
    cc->in_filter_matched_p = 0;
    cc->in_filter_matched_f = 0;
    cc->in_queue_len_n = 0;
    cc->in_queue_len_sum = 0;
    cc->in_app_time_n = 0;
    cc->in_app_time_sum = 0;
    cc->in_app_time_min = 0;
    cc->in_app_time_max = 0;
    cc->pbuf_count = 0;
}

struct buffer_descriptor *find_buffer(uint64_t buffer_id)
{
    struct buffer_descriptor *elem = buffers_list;
    while(elem) {
        if (elem->buffer_id == buffer_id) {
            return elem;
        }
        elem = elem->next;
    }
    return NULL;
} // end function: buffer_descriptor


static uint64_t add_receive_pbuf_app(uint64_t pbuf_id, uint64_t paddr,
                                     uint64_t vaddr, uint64_t len,
                                     uint64_t spp_index,
                                     struct ether_binding *b)
{

    struct client_closure *cc = (struct client_closure *)b->st;
    struct buffer_descriptor *buffer = cc->buffer_ptr;

/*      (struct buffer_descriptor *) ((struct client_closure *) (b->st))->
      buffer_ptr;
*/
    assert(buffer != NULL);

    struct pbuf_desc *pbuf = (struct pbuf_desc *) (buffer->pbuf_metadata_ds);
    uint32_t new_tail = (buffer->pbuf_tail_rx + 1) % APP_QUEUE_SIZE;

    if (buffer->pbuf_metadata_ds == NULL) {
        ETHERSRV_DEBUG("memory is yet not provided by the client "
                       "for pbuf management\n");
        return -1;
    }

    /* check if there is a space in app ring before trying to
     * insert the buffer */
    if (new_tail == buffer->pbuf_head_rx) {
        ETHERSRV_DEBUG("no space to add a new receive pbuf\n");
        printf("no space to add a new receive pbuf[%"PRIu64"] new_tail"
                " %u msg_hd %u\n",
              pbuf_id, new_tail, buffer->pbuf_head_rx);
        return -1;
    }

    /* FIXME: following is precautionary call.  This flow of code is not
     * sending and data but only sending back the empty buffer,
     * so prepare_recv is not strictly necessary. */
    bulk_arch_prepare_recv((void *) (uintptr_t) vaddr, len);

    pbuf[buffer->pbuf_tail_rx].sr = b;
    pbuf[buffer->pbuf_tail_rx].pbuf_id = pbuf_id;
    pbuf[buffer->pbuf_tail_rx].paddr = paddr; // asq: remember for later freeing
    pbuf[buffer->pbuf_tail_rx].vaddr = vaddr;
    pbuf[buffer->pbuf_tail_rx].len = len;
    pbuf[buffer->pbuf_tail_rx].event_sent = false;
    pbuf[buffer->pbuf_tail_rx].spp_index = spp_index;
    // Set the statistics

    if (cc->debug_state == 4) {
        add_event_stat(&pbuf[buffer->pbuf_tail_rx], PBUF_REGISTERED);
    }

    buffer->pbuf_tail_rx = new_tail;
    /*
       ETHERSRV_DEBUG("pbuf added head %u msg_hd %u tail %u in buffer %lu\n",
       buffer->pbuf_head, buffer->pbuf_head_msg, buffer->pbuf_tail,
       buffer->buffer_id);
     */
    return 0;
}

static void register_pbuf_v2(struct ether_binding *b, uint64_t buf_id,
                    uint64_t pbuf_id, uint64_t offset, uint64_t len,
                    uint64_t rts, uint64_t spp_index)
{

#if TRACE_ETHERSRV_MODE
//      trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_N_SPBUF, (uint32_t)pbuf_id);
#endif                          // TRACE_ETHERSRV_MODE

    errval_t r;

    uint64_t ts = rdtsc();
/*	ETHERSRV_DEBUG("ETHERSRV: register_pbuf: %"PRIx64" registering ++++++++\n",
	        pbuf_id);
*/
    struct buffer_descriptor *buffer = find_buffer(buf_id);
    assert(buffer != NULL);
/*    struct buffer_descriptor *buffer = (struct buffer_descriptor *)
      ((struct client_closure *) (b->st))->buffer_ptr;
*/
      struct client_closure *cl = (struct client_closure *)b->st;

    if (cl->debug_state == 4) {
        bm_record_event_simple(RE_PBUF_REG_CS, ts);
    }

    /* Calculating the physical address of this pbuf. */
    uint64_t virtual_addr = (uint64_t) (uintptr_t) buffer->va + offset;
    uint64_t paddr = (uint64_t) (uintptr_t) buffer->pa + offset;
    /* NOTE: virtual address = virtual base + physical offset */
/*
	ETHERSRV_DEBUG("register_pbuf: pbuf id %"PRIx64" on buff_id %"PRIx64"\n",
	 pbuf_id, buffer->buffer_id);
*/

    r = add_receive_pbuf_app(pbuf_id, paddr, virtual_addr, len, spp_index, b);
    assert(err_is_ok(r));

    if (cl->debug_state == 4) {
        bm_record_event_simple(RE_PBUF_REG, ts);
    }
}


static uint64_t add_new_pbufs_2_app_ring(struct ether_binding *b,
        struct shared_pool_private *spp_ptr,
        uint64_t buffer_id)
{
    uint64_t count = 0; // no. of slots added to app ring

    struct slot_data sslot; // temp slot to hold information
    memset(&sslot, 0, sizeof(sslot));
    // Check if ghost_write_id is valid

    sp_reload_regs(spp_ptr);
    while(1) {

        // Check for terminating condition
        if (((spp_ptr->ghost_write_id + 1) % spp_ptr->c_size)
                == spp_ptr->c_read_id) {
            return count;
        }

        sp_copy_slot_data(&sslot,
                &spp_ptr->sp->slot_list[spp_ptr->ghost_write_id].d);
        assert(sslot.client_data != 0);
        assert(sslot.len != 0);
        assert(sslot.no_pbufs != 0);
        // Assigning buffer id here because it was not known to application
        // FIXME: This should be done by the application
        spp_ptr->sp->slot_list[spp_ptr->ghost_write_id].d.buffer_id = buffer_id;
        sslot.buffer_id = buffer_id;
        register_pbuf_v2(b, buffer_id, sslot.pbuf_id, sslot.offset,
                            sslot.len, sslot.ts, spp_ptr->ghost_write_id);
        spp_ptr->ghost_write_id = (spp_ptr->ghost_write_id + 1)
            % spp_ptr->c_size;
        ++count;
    } // end while:

    return count;
} // end function: add_new_pbufs_2_app_ring

static errval_t send_new_buffer_id(struct q_entry entry)
{
    //    ETHERSRV_DEBUG("send_new_buffer_id -----\n");

    struct ether_binding *b = (struct ether_binding *) entry.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.new_buffer_id(b, MKCONT(cont_queue_callback, ccl->q),
                                        entry.plist[0], entry.plist[1]);
        /* entry.error,    entry.buffer_id */
    } else {
        ETHERSRV_DEBUG("send_new_buffer_id Flounder busy.. will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void report_register_buffer_result(struct ether_binding *cc,
        errval_t err, uint64_t buffer_id)
{
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_new_buffer_id;
    entry.binding_ptr = (void *) cc;
    struct client_closure *ccl = (struct client_closure *) cc->st;

    entry.plist[0] = err;
    entry.plist[1] = buffer_id;
    //   error, buffer_id
    enqueue_cont_q(ccl->q, &entry);
}

static void register_buffer(struct ether_binding *cc, struct capref cap,
                        struct capref sp, uint64_t slots, uint8_t role)
{

    ETHERSRV_DEBUG("ethersrv:register buffer called with slots %"PRIu64"\n",
            slots);
    errval_t err;
    int i;
    struct client_closure *closure = (struct client_closure *)cc->st;
    struct buffer_descriptor *buffer = closure->buffer_ptr;

    buffer->role = role;

    // FIXME: Map the memory of sp into address-space as sp

    buffer->con = cc;
    buffer->cap = cap;
    //    buffer->type = type;

    struct frame_identity pa;
    err = invoke_frame_identify(cap, &pa);
    assert(err_is_ok(err));
    buffer->pa = pa.base;
    buffer->bits = pa.bits;

#ifdef __scc__
    r = vspace_map_one_frame_attr(&buffer->va, (1L << buffer->bits), cap,
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
        report_register_buffer_result(cc, ETHERSRV_ERR_TOO_MANY_BUFFERS, 0);
        return;
    }

    err = sp_map_shared_pool(closure->spp_ptr, sp, slots, role);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "sp_map_shared_pool failed");
        // FIXME: report more sensible error
        report_register_buffer_result(cc, err, 0);
        return;
    }


    /* FIXME: cheat, driver is allocating some memory on behalf of client.
     * but this memory should come from "register_metadata_mem"
     * I need to implement that */

    buffer->pbuf_metadata_ds = malloc(metadata_size);
    if (buffer->pbuf_metadata_ds == NULL) {
        printf("CHEAT part: not enough internal memory to support buffer\n");
        abort();
    }

    memset(buffer->pbuf_metadata_ds, 0, metadata_size);
    // FIXME: Where is following variable used?
    buffer->pbuf_metadata_ds_tx = buffer->pbuf_metadata_ds
      + sizeof(struct pbuf_desc) * (APP_QUEUE_SIZE);


    /* FIXME: replace the name netd with control_channel */
    for (i = 0; i < NETD_BUF_NR; i++) {
        if (netd[i] == cc) {
            ETHERSRV_DEBUG("buffer registered with netd connection\n");
            netd_buffer_count++;
        }
    }

    buffer_id_counter++;
    buffer->buffer_id = buffer_id_counter;
//      printf("buffer gets id %lu\n", buffer->buffer_id);
    if (buffer->buffer_id == 3) {
        first_app_b = buffer;
//              printf("setting up first app %lu\n", first_app_b->buffer_id);
    }

    /* Adding the buffer on the top of buffer list. */
    buffer->next = buffers_list;
    buffers_list = buffer;

    ETHERSRV_DEBUG("register_buffer:buff_id[%" PRIu64 "] pa[%" PRIuLPADDR
                   "] va[%p] bits[%" PRIu64 "] Role[%"PRIu8"]#####\n",
                   buffer->buffer_id, buffer->pa, buffer->va, buffer->bits,
                   buffer->role);

    sp_reload_regs(closure->spp_ptr);
    if (buffer->role == RX_BUFFER_ID) {

        uint64_t count = add_new_pbufs_2_app_ring(cc, closure->spp_ptr,
                buffer->buffer_id);
        printf("#### Register_buffer: added %"PRIu64" pbufs\n", count);
    }
    report_register_buffer_result(cc, err, buffer->buffer_id);
}


static bool send_single_pkt_to_driver(struct ether_binding *cc)
{
    errval_t r;
//    uint64_t ts = rdtsc();
    struct client_closure *closure = (struct client_closure *)cc->st;
    assert(closure != NULL);
    struct shared_pool_private *spp = closure->spp_ptr;
    assert(spp != NULL);
    assert(spp->sp != NULL);


    sp_reload_regs(spp);
    // Keep the copy of ghost_read_id so that it can be restored
    // if something goes wrong
    uint64_t ghost_read_id_copy = spp->ghost_read_id;
    uint64_t current_spp_slot_id = spp->ghost_read_id;

    struct slot_data s;
    if (!sp_ghost_read_slot(spp, &s)) {
        return false;
    }

    // Some sanity checks
    assert(s.no_pbufs <= MAX_NR_TRANSMIT_PBUFS);

    // FIXME: Make sure that there are s.no_pbuf slots available

    closure->nr_transmit_pbufs = s.no_pbufs;
    closure->rtpbuf = 0;
    closure->len = 0;

    do {
        closure->pbuf[closure->rtpbuf].buffer_id = s.buffer_id;
        struct buffer_descriptor *buffer = find_buffer(s.buffer_id);
        assert(buffer != NULL);
        closure->pbuf[closure->rtpbuf].buffer = buffer;
        closure->pbuf[closure->rtpbuf].sr = cc;
        closure->pbuf[closure->rtpbuf].cc = closure;
        closure->pbuf[closure->rtpbuf].len = s.len;
        closure->pbuf[closure->rtpbuf].offset = s.offset;
        closure->pbuf[closure->rtpbuf].client_data = s.client_data;
        closure->pbuf[closure->rtpbuf].spp_index = current_spp_slot_id;
        closure->len = closure->len + s.len;  // total lengh of packet
        // making the buffer memory cache coherent.
        bulk_arch_prepare_recv((void *) buffer->pa + s.offset,
                s.len);

        closure->rtpbuf++;
        if (closure->rtpbuf == closure->nr_transmit_pbufs) {
            // If entire packet is here
            ETHERSRV_DEBUG("Sending packet with [%"PRIu16"]pbufs, offset "
                   "[%"PRIu64"] and pbuf[%"PRIx64"]\n",
                    closure->rtpbuf, s.offset, s.client_data);
            break;
        }

        current_spp_slot_id = spp->ghost_read_id;
        if (!sp_ghost_read_slot(spp, &s)) {
            spp->ghost_read_id = ghost_read_id_copy;
            closure->rtpbuf = 0;
            closure->nr_transmit_pbufs = 0;

            assert(!"half packet sent!");
            return false;
        }
    } while(1);

    closure->hw_queue = closure->hw_queue + closure->nr_transmit_pbufs;
    r = ether_transmit_pbuf_list_ptr(closure);

    if (err_is_fail(r)) {
    //in case we cannot transmit, discard the _whole_ packet (just don't
    //enqueue transmit descriptors in the network card's ring)
        dropped_pkt_count++;
//                printf("Transmit_packet dropping %"PRIu64"\n",
//                        dropped_pkt_count);

        //if we have to drop the packet, we still need to send a tx_done
        //to make sure that lwip frees the pbuf. If we drop it, the driver
        //will never find it in the tx-ring from where the tx_dones
        //are usually sent

        /* BIG FIXME: This should free all the pbufs and not just pbuf[0].
         * Also, is it certain that all packets start with pbuf[0]??
         * Mostly this will lead to re-write of bulk-transfer mode.  */

        if (closure->debug_state_tx == 4) {
            ++closure->dropped_pkt_count;
//            bm_record_event_simple(RE_TX_SP_F, ts);
        }

        assert(closure->pbuf[0].sr);
        bool ret = notify_client_free_tx(closure->pbuf[0].sr,
                                         closure->pbuf[0].client_data,
                                         closure->pbuf[0].spp_index,
                                         rdtsc(),
                                         tx_free_slots_fn_ptr(), 1);

        if (ret == false) {
            printf("Error: Bad things are happening."
                   "TX packet dropped, TX_done MSG dropped\n");
            // This can lead to pbuf leak
            // FIXME: What type of error handling to use?
        }
        return false;
    } // end if: failed in transfer

    // successfull transfer!
    if (closure->debug_state_tx == 4) {
        // Benchmarking mode is on
        ++closure->pkt_count;
        closure->pbuf_count = closure->pbuf_count +
            closure->nr_transmit_pbufs;

        if (closure->pkt_count == closure->out_trigger_counter) {
            benchmark_control_request(cc, BMS_STOP_REQUEST, 0, 0);
        }
    } // end if: benchmarking mode is on

    if (closure->debug_state_tx == 3) {
        // Benchmarking mode should be started here!
        ++closure->pkt_count;
        assert(closure->pkt_count == 1);
        // This is the first packet, so lets restart the timer!!
        closure->start_ts_tx = rdtsc();
        closure->debug_state_tx = 4;
        closure->pbuf_count = closure->nr_transmit_pbufs;
    } // end if: Starting benchmarking mode

    return true;
} // end function: send_single_pkt_to_driver

static uint64_t send_packets_on_wire(struct ether_binding *cc)
{
    uint64_t pkts = 0;
    struct client_closure *closure = (struct client_closure *)cc->st;
    assert(closure != NULL);
    struct buffer_descriptor *buffer = closure->buffer_ptr;
    assert(buffer != NULL);
    struct shared_pool_private *spp = closure->spp_ptr;
    assert(spp != NULL);
    assert(spp->sp != NULL);
    if (buffer->role == RX_BUFFER_ID) {
//        printf("####### ERROR: send_packets_on_wire called on wrong buff\n");
            return 0;
    }
//    uint64_t ts = rdtsc();

    sp_reload_regs(spp);
    if (!sp_queue_empty(spp)) {
        // There are no packets
        while (send_single_pkt_to_driver(cc));
        sp_reload_regs(spp);
        ++pkts;
    }

    if (pkts > 0) {
        if (closure->debug_state_tx == 4) {
            bm_record_event_no_ts(RE_TX_T, pkts);
        }
    }
    return pkts;
}

static errval_t send_sp_notification_from_driver(struct q_entry e)
{
//    ETHERSRV_DEBUG("send_sp_notification_from_driver-----\n");
    struct ether_binding *b = (struct ether_binding *) e.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;
    if (ccl->debug_state_tx == 4) {
        bm_record_event_simple(RE_TX_SP_MSG_Q, e.plist[1]);
    }
    uint64_t ts = rdtsc();
    if (b->can_send(b)) {
        ++ccl->tx_done_count;
        errval_t err = b->tx_vtbl.sp_notification_from_driver(b,
                MKCONT(cont_queue_callback, ccl->q), e.plist[0], ts);
                // type, ts
        if (ccl->debug_state_tx == 4) {
            bm_record_event_simple(RE_TX_SP_MSG, ts);
        }
        return err;
    } else {
        ETHERSRV_DEBUG("send_sp_notification_from_driver: Flounder busy.."
                "retry --------\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}


static void do_pending_work(struct ether_binding *b)
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

//    printf("do_pending_work: sent packets[%"PRIu64"]\n", pkts);
    // Check if there are more pbufs which are to be marked free
    while (handle_free_tx_slot_fn_ptr());


    if (sp_queue_full(spp)) {
        // app is complaining about TX queue being full
        // FIXME: Release TX_DONE
        while (handle_free_tx_slot_fn_ptr());
    }

    if (spp->notify_other_side) {
        // Send notification to application, telling that there is
        // no more data
        ++closure->tx_notification_sent;
        struct q_entry entry;
        memset(&entry, 0, sizeof(struct q_entry));
        entry.handler = send_sp_notification_from_driver;
        entry.binding_ptr = (void *) b;
        entry.plist[0] = 1; // FIXME: will have to define these values
        // FIXME: Get the remaining slots value from the driver
        entry.plist[1] = rdtsc();
        enqueue_cont_q(closure->q, &entry);
//        printf("notfify client 1: notification enqueued\n");
        spp->notify_other_side = 0;
    }

    if (closure->debug_state_tx == 4) {
        bm_record_event_simple(RE_TX_W_ALL, ts);
    }
}


void do_pending_work_for_all(void)
{

    struct buffer_descriptor *next_buf = buffers_list;
    while (next_buf != NULL) {
        do_pending_work(next_buf->con);
        next_buf = next_buf->next;
    }
}


static void sp_notification_from_app(struct ether_binding *cc, uint64_t type,
                uint64_t rts)
{

    struct client_closure *closure = (struct client_closure *)cc->st;
    assert(closure != NULL);
    if (closure->debug_state_tx == 4) {
        bm_record_event_simple(RE_TX_NOTI_CS, rts);
    }
    // FIXME : call schedule work
    do_pending_work(cc);

} // end function:  sp_notification_from_app

static errval_t send_mac_addr_response(struct q_entry entry)
{
    //    ETHERSRV_DEBUG("send_mac_addr_response -----\n");
    struct ether_binding *b = (struct ether_binding *) entry.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.get_mac_address_response(b,
                                                   MKCONT(cont_queue_callback,
                                                          ccl->q),
                                                   entry.plist[0]);
        /* entry.hwaddr */
    } else {
        ETHERSRV_DEBUG("send_mac_addr_response Flounder busy.. will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}


static void get_mac_addr(struct ether_binding *cc)
{
    union {
        uint8_t hwaddr[6];
        uint64_t hwasint;
    } u;

    u.hwasint = 0;
    ether_get_mac_address_ptr(u.hwaddr);

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_mac_addr_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure *ccl = (struct client_closure *) cc->st;

    entry.plist[0] = u.hwasint;
    /* entry.plist[0]);
       entry.hwaddr */

    enqueue_cont_q(ccl->q, &entry);

}

static void print_statistics_handler(struct ether_binding *cc)
{
    ETHERSRV_DEBUG("ETHERSRV: print_statistics_handler: called.\n");
//      print_statistics();
}

static void print_cardinfo_handler(struct ether_binding *cc)
{

}

/*****************************************************************
 * Chips Handlers:
 ****************************************************************/
//static int client_conn_nr = 0;

static void export_ether_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "service [%s] export failed", my_service_name);
        abort();
    }

    ETHERSRV_DEBUG("service [%s] exported at iref %u\n", my_service_name, iref);

    // register this iref with the name service
    err = nameservice_register(my_service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for [%s]", my_service_name);
        abort();
    }
}


static void error_handler(struct ether_binding *b, errval_t err)
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

static errval_t connect_ether_cb(void *st, struct ether_binding *b)
{
    ETHERSRV_DEBUG("ether service got a connection!44\n");
    errval_t err = SYS_ERR_OK;

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_ether_vtbl;
    b->error_handler = error_handler;

    struct client_closure *cc =
      (struct client_closure *) malloc(sizeof(struct client_closure));
    if (cc == NULL) {
        err = ETHERSRV_ERR_NOT_ENOUGH_MEM;
        ETHERSRV_DEBUG("Ether connection: out of memory\n");
        return err;
    }
    memset(cc, 0, sizeof(struct client_closure));

    struct buffer_descriptor *buffer =
      (struct buffer_descriptor *) malloc(sizeof(struct buffer_descriptor));
    if (buffer == NULL) {
        err = ETHERSRV_ERR_NOT_ENOUGH_MEM;
        ETHERSRV_DEBUG("connection_service_logic: out of memory\n");
        free(cc);
        return err;
    }
    memset(buffer, 0, sizeof(struct buffer_descriptor));

    cc->spp_ptr = (struct shared_pool_private *)
                        malloc(sizeof(struct shared_pool_private));
    if (cc->spp_ptr == NULL) {
        err = ETHERSRV_ERR_NOT_ENOUGH_MEM;
        ETHERSRV_DEBUG("connection_service_logic: out of memory\n");
        free(cc);
        free(buffer);
        return err;
    }
    memset(cc->spp_ptr, 0, sizeof(struct shared_pool_private));
    buffer->spp_prv = cc->spp_ptr;
    b->st = cc;
    cc->buffer_ptr = buffer;
    cc->nr_transmit_pbufs = 0;
    cc->rtpbuf = 0;
    cc->debug_state = 0;        // Default debug state : no debug
    cc->app_connection = b;
    reset_client_closure_stat(cc);
    cc->start_ts = rdtsc();

    if (client_no < 2) {
        netd[client_no] = b;
    }
    cc->cl_no = client_no++;

    char name[64];

    sprintf(name, "ether_a_%d", cc->cl_no);
    cc->q = create_cont_q(name);
    if (cc->q == NULL) {
        ETHERSRV_DEBUG("connection_service_logic: queue allocation failed\n");
        free(buffer);
        free(cc);
        return err;
    }
    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
} // end function: connect_ether_cb


/*****************************************************************
 * ethersrv initialization wrapper:
 ****************************************************************/
void ethersrv_init(char *service_name,
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

    ether_get_mac_address_ptr = get_mac_ptr;
    ether_transmit_pbuf_list_ptr = transmit_ptr;
    tx_free_slots_fn_ptr = tx_free_slots_ptr;
    handle_free_tx_slot_fn_ptr = handle_free_tx_slot_ptr;
    my_service_name = service_name;

    buffers_list = NULL;
    netd[0] = NULL;
    netd[1] = NULL;
    buffer_id_counter = 0;
    netd_buffer_count = 0;
    client_no = 0;

    /* FIXME: populate the receive ring of device driver with local pbufs */

    /* exporting ether interface */
    err = ether_export(NULL /* state pointer for connect/export callbacks */ ,
                       export_ether_cb, connect_ether_cb, get_default_waitset(),
                       IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "%s export failed", my_service_name);
        abort();
    }

    init_ether_control_service(my_service_name);
}


/*******************************************/
bool notify_client_free_tx(struct ether_binding * b,
                           uint64_t client_data,
                           uint64_t spp_index,
                           uint64_t rts,
                           uint64_t slots_left,
                           uint64_t dropped)
{
//    uint64_t ts = rdtsc();
    assert(b != NULL);
    struct client_closure *cc = (struct client_closure *)b->st;
    assert(cc != NULL);
    struct shared_pool_private *spp = cc->spp_ptr;
    assert(spp != NULL);
    assert(spp->sp != NULL);


    cc->hw_queue = cc->hw_queue - 1;
    ETHERSRV_DEBUG("Notifying the app for %"PRIu64"\n", spp_index);

    if(spp->sp->read_reg.value != spp_index) {
       printf("notify_client_free_tx: prob: read reg[%"PRIu64"] == "
               "spp_index [%"PRIu64"]\n",
               spp->sp->read_reg.value, spp_index);
    }
    assert(spp->sp->read_reg.value == spp_index);

    if(!sp_set_read_index(spp, ((spp_index + 1) % spp->c_size))) {
        // FIXME:  This is dengarous!  I should increase read index,
        // only when all the packets till that read index are sent!
//        printf("failed for %"PRIu64"\n",spp_index);
//        sp_print_metadata(spp);
        assert(!"sp_set_read_index failed");
    }

    if (spp->notify_other_side == 0) {

        if (cc->debug_state_tx == 4) {
            bm_record_event_simple(RE_TX_DONE_NN, rts);
        }
//        printf("notfify client 1: sending no notifications!\n");
        return true;
    }

    // We need to send notification to other side saying that
    // conditions are changed
//    if (queue_free_slots(cc->q) < 10) {
//            printf("sending TX_done too fast %d\n",
//            queue_free_slots(cc->q));
//        return false;
//    }

    cc->tx_explicit_msg_needed++;
    if (cc->debug_state_tx == 4) {
        bm_record_event_simple(RE_TX_DONE_N, rts);
    }
    return true;
}

bool copy_packet_to_user(struct buffer_descriptor * buffer,
                         void *data, uint64_t len)
{

    uint32_t phead_rx, ptail_rx;

    if (buffer == NULL) {
        /* Invalid buffer */
        printf("ERROR: copy_packet_to_user: Invalid buffer.\n");
        return false;
    }

    struct ether_binding *b = buffer->con;
    if(b == NULL) {
        printf("copy_packet_to_user: failed as b is NULL\n");
        printf("callstack: %p %p %p %p\n",
	     __builtin_return_address(0),
	     __builtin_return_address(1),
	     __builtin_return_address(2),
	     __builtin_return_address(3));
        assert(b != NULL);
        return false;
    }
    struct client_closure *cl = (struct client_closure *) b->st;
    assert(cl != NULL);
    uint64_t queue_len = queue_free_slots(cl->q);
    if (cl->debug_state == 4) {
        ++cl->in_queue_len_n;
        cl->in_queue_len_sum += queue_len;
    }
    if ( queue_len < 10) {
        if (cl->debug_state == 4) {
            ++cl->in_dropped_q_full;
        }
        return false;
    }


//    assert(buffer != NULL);
    struct pbuf_desc *pbuf_list =
      (struct pbuf_desc *) (buffer->pbuf_metadata_ds);

    if (len <= 0 || data == NULL) {
        ETHERSRV_DEBUG("[%d]ERROR: copy_packet_to_user: Invalid packet of len %"
                       PRIu64 " and ptr [%p]\n", disp_get_core_id(), len, data);

        if (cl->debug_state == 4) {
            ++cl->in_dropped_invalid_pkt;
        }
        return false;
        /* This is just another error, don't abort, ignore the packet
         * and continue. */
    }

    phead_rx = buffer->pbuf_head_rx;
    ptail_rx = buffer->pbuf_tail_rx;

    ETHERSRV_DEBUG("Copy_packet_2_usr_buf [%" PRIu64 "]: phead[%u] ptail[%u]\n",
                   buffer->buffer_id, pbuf_head_rx, pbuf_tail_rx);

    struct pbuf_desc *upbuf = &pbuf_list[phead_rx];

//    assert(upbuf != NULL);
    if (upbuf == NULL) {
        /* pbufs are not yet registered, so can't send packet to userspace. */
        ETHERSRV_DEBUG
          ("ERROR: copy_packet_to_user: No pbufs registered for selected buffer\n");
        if (cl->debug_state == 4) {
            ++cl->in_dropped_no_app;
        }
        return false;
    }

    if (((phead_rx + 1) % APP_QUEUE_SIZE) == ptail_rx) {

        ETHERSRV_DEBUG("[%d]no space in userspace 2cp pkt buf [%" PRIu64
                       "]: phead[%u] ptail[%u]\n", disp_get_domain_id(),
                       buffer->buffer_id, buffer->pbuf_head_rx,
                       buffer->pbuf_tail_rx);
        if (cl->debug_state == 4) {
            ++cl->in_dropped_app_buf_full;
        }
        return false;
    }

    void *dst = (void *) (uintptr_t) upbuf->vaddr;

    ETHERSRV_DEBUG("Copy packet pos %p %p %p\n", buffer->va, dst,
                   (buffer->va + (1L << buffer->bits)));

    if ((dst < (buffer->va + (1L << buffer->bits))) && (dst >= buffer->va)) {
        uint64_t ts = rdtsc();

        memcpy_fast((void *) (uintptr_t) upbuf->vaddr, data, len);
        if (cl->debug_state == 4) {
            bm_record_event_simple(RE_COPY, ts);
        }

    } else {
        // k: naughty client does not deserve a packet :)
        ETHERSRV_DEBUG("naughty client detected\n");
        if (cl->debug_state == 4) {
            ++cl->in_dropped_app_invalid_buf;
        }
        return false;
    }
    // add trace pkt cpy
#if TRACE_ETHERSRV_MODE
    uint32_t pkt_location = (uint32_t) ((uintptr_t) data);

    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_PKT_CPY, pkt_location);
#endif // TRACE_ETHERSRV_MODE

    upbuf->packet_size = len;

    sp_reload_regs(cl->spp_ptr);
    // Make sure that spp_index is the slot which will be next written
    assert(upbuf->spp_index == cl->spp_ptr->c_write_id);

    // update the length of packet in sslot.len field
    struct slot_data sslot;
    sp_copy_slot_data_from_index(cl->spp_ptr, upbuf->spp_index, &sslot);
    sslot.len = len;
    sslot.no_pbufs = 1;
    sslot.ts = rdtsc();

    // update the spp indicating the new packet
    // need to increment write pointer
    assert(sp_produce_slot(cl->spp_ptr, &sslot));

    phead_rx = (phead_rx + 1) % APP_QUEUE_SIZE;
    buffer->pbuf_head_rx = phead_rx;

    // add newly available pbuf slots into app-queue and hardware queue
    uint64_t count = 0;
    count = add_new_pbufs_2_app_ring(buffer->con, cl->spp_ptr,
            buffer->buffer_id);
    ETHERSRV_DEBUG("cp_pkt_2_usr: added %"PRIu64" pbufs into app ring\n",
            count);

    if (cl->spp_ptr->notify_other_side) {
        // Send notification to application, telling that there is
        // no more data
        ++cl->rx_notification_sent;
        struct q_entry entry;
        memset(&entry, 0, sizeof(struct q_entry));
        entry.handler = send_sp_notification_from_driver;
        entry.binding_ptr = (void *) b;
        entry.plist[0] = 1; // FIXME: will have to define these values
        // FIXME: Get the remaining slots value from the driver
        entry.plist[1] = rdtsc();
        enqueue_cont_q(cl->q, &entry);
        cl->spp_ptr->notify_other_side = 0;
    }
    return true;
}


/* This function tells if netd is registered or not. */
bool waiting_for_netd(void)
{
    return ((netd[RECEIVE_CONNECTION] == NULL)
            || (netd[TRANSMIT_CONNECTION] == NULL));
} // end function: is_netd_registered


static errval_t send_benchmark_control_response(struct q_entry entry)
{
    //    ETHERSRV_DEBUG("send_mac_addr_response -----\n");
    struct ether_binding *b = (struct ether_binding *) entry.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.benchmark_control_response(b,
                           MKCONT(cont_queue_callback, ccl->q),
                           entry.plist[0], entry.plist[1], entry.plist[2]);
                // state, delta, cl
    } else {
        ETHERSRV_DEBUG("send_benchmark_control_response Flounder busy.."
                " will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void send_benchmark_control(struct ether_binding *cc, uint64_t state,
        uint64_t delta, uint64_t cl)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_benchmark_control_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure *ccl = (struct client_closure *) cc->st;

    entry.plist[0] = state;
    entry.plist[1] = delta;
    entry.plist[2] = cl;
    /* entry.plist[0]);
       entry.hwaddr */
    enqueue_cont_q(ccl->q, &entry);
}


static void benchmark_control_request(struct ether_binding *cc, uint8_t state,
        uint64_t trigger, uint64_t cl_data)
{
    uint64_t ts;
    uint8_t bm_type = 1; // 0 = RX benchmark, 1 = TX benchmark
//    printf("setting the debug status to %x and trigger [%"PRIu64"]\n",
//            state, trigger);
    struct client_closure *cl = ((struct client_closure *) (cc->st));

    cl->debug_state = state;
    debug_state = state;
    switch (state) {

        case BMS_STOP_REQUEST:  // PRINTING stats

            if (bm_type == 0) {
                ts = rdtsc() - cl->start_ts;
            } else {
                ts = rdtsc() - cl->start_ts_tx;
            }
            printf("#### Stopping MBM Cycles[%"PU"],"
                   "Pbufs[%" PRIu64 "], pkts[%" PRIu64 "], D[%" PRIu64 "], "
                   "in SP Q[%" PRIu64 "], HW_Q[%"PRIu64"]\n",
                   in_seconds(ts), cl->pbuf_count, cl->pkt_count,
                   cl->dropped_pkt_count,
                   sp_queue_elements_count(cl->spp_ptr),
                   cl->hw_queue);
            printf("D TX Explicit msg needed [%"PRIu64"], sent[%"PRIu64"]\n",
                    cl->tx_explicit_msg_needed, cl->tx_notification_sent);
            printf("### RX OK[%"PRIu64"], D_CQ_full[%"PRIu64"], "
                  "D_invalid[%"PRIu64"], D_NO_APP[%"PRIu64"], "
                  "D_APP_BUF_FULL[%"PRIu64"], D_APP_BUF_INV[%"PRIu64"]\n",
                  cl->in_success, cl->in_dropped_q_full,
                  cl->in_dropped_invalid_pkt, cl->in_dropped_no_app,
                  cl->in_dropped_app_buf_full, cl->in_dropped_app_invalid_buf);
            printf("### RX D_NTF_PROB[%"PRIu64"], D_NTF_PRO2[%"PRIu64"], "
                  "Other_pkt_OK[%"PRIu64"], in_ARP[%"PRIu64"], "
                  "in_NETD[%"PRIu64"], in_paused[%"PRIu64"]\n",
                  cl->in_dropped_notification_prob,
                  cl->in_dropped_notification_prob2, cl->in_other_pkts,
                  cl->in_arp_pkts, cl->in_netd_pkts, cl->in_paused_pkts);
            printf( "### RX FM[%"PRIu64"], FMF[%"PRIu64"], FMP[%"PRIu64"]\n",
                  cl->in_filter_matched, cl->in_filter_matched_f,
                  cl->in_filter_matched_p);
            printf( "### RX AVG QL[%"PRIu64"] on [%"PRIu64"]calls\n",
                    my_avg(cl->in_queue_len_sum,cl->in_queue_len_n),
                    cl->in_queue_len_n);
            printf( "### RX APP N[%"PRIu64"] avg[%"PU"], MAX[%"PU"]"
                    "MAX[%"PU"]\n", cl->in_app_time_n,
                    in_seconds(my_avg(cl->in_app_time_sum,cl->in_app_time_n)),
                    in_seconds(cl->in_app_time_max),
                    in_seconds(cl->in_app_time_min));
            printf( "### RX APP N[%"PRIu64"] avg[%"PRIu64"], MAX[%"PRIu64"] "
                    "MIN[%"PRIu64"]\n", cl->in_app_time_n,
                    (my_avg(cl->in_app_time_sum,cl->in_app_time_n)),
                    (cl->in_app_time_max),
                    (cl->in_app_time_min));
            print_app_stats(cl->buffer_ptr);
            bm_print_interesting_stats(bm_type);
            printf("Interrupt count [%"PRIu64"], loop count[%"PRIu64"]\n",
                    interrupt_counter, interrupt_loop_counter);

            printf("Driver spp state");
            sp_print_metadata(cl->spp_ptr);
            send_benchmark_control(cc, BMS_STOPPED, ts,
                    (cl->pkt_count - cl->dropped_pkt_count));
            cl->in_trigger_counter = trigger;
            cl->out_trigger_counter = trigger;
            cl->debug_state = BMS_STOPPED;
            debug_state = BMS_STOPPED;
            cl->debug_state_tx = BMS_STOPPED;

            break;

        case BMS_START_REQUEST:  // Resetting stats, for new round of recording
            interrupt_counter = 0;
            interrupt_loop_counter = 0;
            reset_client_closure_stat(cl);
            // FIXME: Remove it, only for specific debugging!!!!
            if (cl->spp_ptr->sp->read_reg.value != 0) {
                printf("#### reset_client_closure_stat: read_reg == %"PRIu64""
                    "instead of 0\n",
                cl->spp_ptr->sp->read_reg.value);
            }
//            assert(cl->spp_ptr->sp->read_reg.value == 0);

            cl->in_trigger_counter = trigger;
            cl->out_trigger_counter = trigger;
            cl->debug_state = 3;
            debug_state = 3;
            cl->debug_state_tx = 3;
            cl->pkt_count = 0;
            // Resetting receive path stats
            reset_app_stats(cl->buffer_ptr);
            bm_reset_stats();
            printf("#### Starting MBM now \n");
            cl->start_ts = rdtsc();
            cl->start_ts_tx = rdtsc();
            send_benchmark_control(cc, BMS_RUNNING, cl->start_ts, trigger);
            break;


        default:
            printf("#### MBM: invalid state %x \n", state);
    } // end switch:

} // end function: benchmark_control_request


void ethersrv_debug_printf(const char *fmt, ...)
{
    uint8_t dbg = debug_state;

    if (dbg == 0) {
        return;
    }

    va_list argptr;
    char str[512];
    size_t len;

    len =
      snprintf(str, sizeof(str), "%.*s.%u: ETHERSRV:", DISP_NAME_LEN,
               disp_name(), disp_get_core_id());
    if (len < sizeof(str)) {
        va_start(argptr, fmt);
        vsnprintf(str + len, sizeof(str) - len, fmt, argptr);
        va_end(argptr);
    }
    sys_print(str, sizeof(str));
}


// ******************** For recording stats *******************
// NOTE: Code duplication (Copied from lib/lwip/src/core/init.c)
// FIXME:  Move to a library
// For recording stats
static uint64_t stats[EVENT_LIST_SIZE][EVENT_ELEMENTS];
void bm_reset_stats(void)
{
    for (int i = 0; i < EVENT_LIST_SIZE; ++i) {
        for (int j = 0; j < EVENT_ELEMENTS; ++j) {
            stats[i][j] = 0;
        }
    }
} // end function: reset_stats


void bm_record_event(uint8_t event_type, uint64_t delta)
{
    uint8_t et = event_type;
    ++stats[et][RDT_COUNT];
    stats[et][RDT_SUM] += delta;
    if (stats[et][RDT_COUNT] == 1) {
        stats[et][RDT_MAX] = delta;
        stats[et][RDT_MIN] = delta;
    } else {
        if (delta > stats[et][RDT_MAX]) {
            stats[et][RDT_MAX]= delta;
        }
        if (delta < stats[et][RDT_MIN]) {
            stats[et][RDT_MIN]= delta;
        }
    }
} // end function: record_event

void bm_record_event_simple(uint8_t event_type, uint64_t ts)
{
    uint64_t delta = rdtsc() - ts;
    bm_record_event(event_type, delta);
}

void bm_record_event_no_ts(uint8_t event_type, uint64_t val)
{
    bm_record_event(event_type, val);
}


void bm_print_event_stat(uint8_t event_type, char *event_name)
{
    uint8_t et = event_type;
    printf("Event %20s (%"PRIu8"): N[%"PRIu64"], AVG[%"PU"], "
            "MAX[%"PU"], MIN[%"PU"], TOTAL[%"PU"]\n", event_name, et,
            stats[et][RDT_COUNT],
            in_seconds(my_avg(stats[et][RDT_SUM],stats[et][RDT_COUNT])),
            in_seconds(stats[et][RDT_MAX]),
            in_seconds(stats[et][RDT_MIN]),
            in_seconds(stats[et][RDT_SUM]));
} // end function: print_event_stat

static void bm_print_event_stat_no(uint8_t event_type, char *event_name)
{
    uint8_t et = event_type;
    printf("Event %20s (%"PRIu8"): N[%"PRIu64"], AVG[%"PRIu64"], "
            "MAX[%"PRIu64"], MIN[%"PRIu64"], TOTAL[%"PRIu64"]\n", event_name,
            et, stats[et][RDT_COUNT],
            (my_avg(stats[et][RDT_SUM],stats[et][RDT_COUNT])),
            (stats[et][RDT_MAX]),
            (stats[et][RDT_MIN]),
            (stats[et][RDT_SUM]));
}
void bm_print_interesting_stats(uint8_t type)
{
    switch (type) {
        case 0:
            bm_print_event_stat(RE_FILTER,       "D: RX Filter time");
            bm_print_event_stat(RE_COPY,         "D: RX copy time");
            bm_print_event_stat(RE_PBUF_REG,     "D: RX pbuf reg time");
            bm_print_event_stat(RE_PKT_RECV_MSG, "D: RX pkt recv ntf");
            bm_print_event_stat(RE_DROPPED,      "D: RX dropped time");
            bm_print_event_stat(RE_USEFUL,       "D: RX useful time");
            bm_print_event_stat(RE_PKT_RECV_Q,   "D: RX queue");
            bm_print_event_stat(RE_PBUF_REG_CS,  "D: RX REG pbuf CS");
            break;

        case 1:
            bm_print_event_stat(RE_TX_NOTI_CS,   "D: NOTI FROM APPLI");
            bm_print_event_stat_no(RE_TX_T,         "D: TX T");
            bm_print_event_stat(RE_TX_SP_S,      "D: TX SP_S");
            bm_print_event_stat(RE_TX_SP_F,      "D: TX SP_F");
            bm_print_event_stat(RE_TX_DONE,      "D: TX DONE");
            bm_print_event_stat(RE_TX_W_ALL,     "D: TX W_ALL");
            bm_print_event_stat(RE_TX_DONE_NN,   "D: TX DONE_NN");
            bm_print_event_stat(RE_TX_DONE_N,    "D: TX DONE_N");
            bm_print_event_stat(RE_TX_SP_MSG,    "D: TX SP_MSG");
            bm_print_event_stat(RE_TX_SP_MSG_Q,  "D: TX SP_MSG_Q");
        break;

        default:
            printf("Invalid type given to print stats\n");
    } // end switch
}


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

