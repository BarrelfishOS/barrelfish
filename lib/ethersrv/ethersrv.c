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
#include <bfdmuxvm/vm.h>
#include <if/ether_defs.h>
#include <if/ether_control_defs.h>
#include "ethersrv_debug.h"

#define APP_QUEUE_SIZE  (RECEIVE_BUFFERS + 1)

#define USE_SPP_FOR_TX_DONE 1

/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif                          // CONFIG_TRACE && NETWORK_STACK_TRACE
/*****************************************************************
 * Constants:
 *****************************************************************/

#define LAST_ACCESSED_BYTE_ARP 12
#define LAST_ACCESSED_BYTE_TRANSPORT 36

#define MACHINE_CLK_UNIT    (1000000)

#if !defined(__scc__)
#define MACHINE_CLOCK_SPEED  (2800)
#else
#define MACHINE_CLOCK_SPEED  (533)
#endif // !defined(__scc__)

#define IN_SECONDS(x)   (((x)/(MACHINE_CLOCK_SPEED))/(MACHINE_CLK_UNIT))

#define CONVERT_TO_SEC

#ifdef CONVERT_TO_SEC
#define PU "f"
float in_seconds(uint64_t cycles);
float in_seconds(uint64_t cycles)
{
    float ans;
    ans = cycles / MACHINE_CLOCK_SPEED;
    ans = ans / MACHINE_CLK_UNIT;
    return ans;
}
#else
#define PU PRIu64
uint64_t in_seconds(uint64_t cycles);
uint64_t in_seconds(uint64_t cycles)
{
    return cycles;
}

#endif // CONVERT_TO_SEC

/* This is client_closure for filter management */
struct client_closure_FM {
    struct ether_control_binding *app_connection;       /* FIXME: Do I need this? */
    struct cont_queue *q;
/* FIXME: this should contain the registered buffer ptr */
};

// Measurement purpose, counting interrupt numbers
uint64_t interrupt_counter = 0;
uint64_t interrupt_loop_counter = 0;

/* ethersrv specific debug state indicator */
static int debug_state = 0;

/* NETD connections */
#define NETD_BUF_NR 2
static struct ether_binding *netd[NETD_BUF_NR];

// Counter for dropped packets
static uint64_t dropped_pkt_count = 0;

/*****************************************************************
 * Prototypes
 *****************************************************************/

static void register_buffer(struct ether_binding *cc, struct capref cap,
                        struct capref sp, uint64_t slots, uint8_t role);
static void register_pbuf(struct ether_binding *b, uint64_t pbuf_id,
                          uint64_t paddr, uint64_t len, uint64_t rts);
/*
static void transmit_packet(struct ether_binding *cc, uint64_t nr_pbufs,
                uint64_t buffer_id, uint64_t len, uint64_t offset,
                uint64_t client_data);
*/
static void sp_notification_from_app(struct ether_binding *cc, uint64_t type,
                uint64_t ts);

static void get_mac_addr(struct ether_binding *cc);
static void print_statistics_handler(struct ether_binding *cc);
static void print_cardinfo_handler(struct ether_binding *cc);
static void benchmark_control_request(struct ether_binding *cc, uint8_t state,
        uint64_t trigger, uint64_t cl);

static void register_filter_memory_request(struct ether_control_binding *cc,
                                           struct capref mem_cap);
static void register_filter(struct ether_control_binding *cc, uint64_t id,
                            uint64_t len_rx, uint64_t len_tx,
                            uint64_t buffer_id_rx, uint64_t buffer_id_tx,
                            uint64_t ftype, uint64_t paused);
static void register_arp_filter(struct ether_control_binding *cc, uint64_t id,
                                uint64_t len_rx, uint64_t len_tx);
static void deregister_filter(struct ether_control_binding *cc,
                              uint64_t filter_id);
static void re_register_filter(struct ether_control_binding *cc,
                               uint64_t filter_id, uint64_t buffer_id_rx,
                               uint64_t buffer_id_tx);
static void pause_filter(struct ether_control_binding *cc, uint64_t filter_id,
                         uint64_t buffer_id_rx, uint64_t buffer_id_tx);


/*****************************************************************
 * VTABLE
 *****************************************************************/

// Initialize service
static struct ether_rx_vtbl rx_ether_vtbl = {
    .register_buffer = register_buffer,
    .register_pbuf = register_pbuf,
    .sp_notification_from_app = sp_notification_from_app,
    .get_mac_address = get_mac_addr,
    .print_statistics = print_statistics_handler,
    .print_cardinfo = print_cardinfo_handler,
    .benchmark_control_request = benchmark_control_request,
};

// Initialize interface for ether_control channel
static struct ether_control_rx_vtbl rx_ether_control_vtbl = {
    .register_filter_memory_request = register_filter_memory_request,
    .register_filter_request = register_filter,
    .re_register_filter_request = re_register_filter,
    .deregister_filter_request = deregister_filter,
    .register_arp_filter_request = register_arp_filter,
    .pause = pause_filter,
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

int client_no = 0;
static uint64_t buffer_id_counter = 0;
static uint64_t filter_id_counter = 0;
uint64_t netd_buffer_count = 0;
static char *my_service_name = NULL;

static struct buffer_descriptor *first_app_b = NULL;


/* Memory used for filter registration : ps */
// registered buffers:
static struct buffer_descriptor *buffers_list;

// filters state:
static struct filter *rx_filters;
static struct filter arp_filter_rx;
static struct filter arp_filter_tx;

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
    cc->pbuf_count = 0;
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
}

/*****************************************************************
 * Message handlers:
 ****************************************************************/

static struct buffer_descriptor *find_buffer(uint64_t buffer_id)
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
    uint32_t new_tail = (buffer->pbuf_tail + 1) % APP_QUEUE_SIZE;

    if (buffer->pbuf_metadata_ds == NULL) {
        ETHERSRV_DEBUG("memory is yet not provided by the client "
                       "for pbuf management\n");
        return -1;
    }

    /* check if there is a space in app ring before trying to
     * insert the buffer */
    if (new_tail == buffer->pbuf_head_msg) {
        ETHERSRV_DEBUG("no space to add a new receive pbuf\n");
        printf("no space to add a new receive pbuf[%"PRIu64"] new_tail"
                " %u msg_hd %u\n",
              pbuf_id, new_tail, buffer->pbuf_head_msg);
        return -1;
    }

    /* FIXME: following is precautionary call.  This flow of code is not
     * sending and data but only sending back the empty buffer,
     * so prepare_recv is not strictly necessary. */
    bulk_arch_prepare_recv((void *) (uintptr_t) vaddr, len);

    pbuf[buffer->pbuf_tail].sr = b;
    pbuf[buffer->pbuf_tail].pbuf_id = pbuf_id;
    pbuf[buffer->pbuf_tail].paddr = paddr; // asq: remember for later freeing
    pbuf[buffer->pbuf_tail].vaddr = vaddr;
    pbuf[buffer->pbuf_tail].len = len;
    pbuf[buffer->pbuf_tail].event_sent = false;
    pbuf[buffer->pbuf_tail].spp_index = spp_index;
    // Set the statistics

    if (cc->debug_state == 4) {
        add_event_stat(&pbuf[buffer->pbuf_tail], PBUF_REGISTERED);
    }

    buffer->pbuf_tail = new_tail;
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
/*
        struct slot_data sslot;
        memset(&sslot, 0, sizeof(sslot));
        // register all the pbufs
        for (uint64_t id = 0; id < closure->spp_ptr->c_size; ++id) {
            sp_copy_slot_data(&sslot, &closure->spp_ptr->sp->slot_list[id].d);
            assert(sslot.client_data != 0);
            assert(sslot.len != 0);
            assert(sslot.no_pbufs != 0);
            // Assigning buffer id here because it was not known to application
            closure->spp_ptr->sp->slot_list[id].d.buffer_id = buffer->buffer_id;
            sslot.buffer_id = buffer->buffer_id;
            register_pbuf_v2(cc, sslot.buffer_id, sslot.pbuf_id, sslot.offset,
                    sslot.len, sslot.ts, id);
            closure->spp_ptr->ghost_write_id = id;
        } // end for:
*/

    }
    report_register_buffer_result(cc, err, buffer->buffer_id);
}

static void register_pbuf(struct ether_binding *b, uint64_t pbuf_id,
                          uint64_t offset, uint64_t len, uint64_t rts)
{

#if TRACE_ETHERSRV_MODE
//      trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_N_SPBUF, (uint32_t)pbuf_id);
#endif                          // TRACE_ETHERSRV_MODE

    errval_t r;

    uint64_t ts = rdtsc();
/*	ETHERSRV_DEBUG("ETHERSRV: register_pbuf: %"PRIx64" registering ++++++++\n",
	        pbuf_id);
*/
    struct buffer_descriptor *buffer = (struct buffer_descriptor *)
      ((struct client_closure *) (b->st))->buffer_ptr;
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

    assert(!"NYI");
    r = add_receive_pbuf_app(pbuf_id, paddr, virtual_addr, len, 0, b);
    assert(err_is_ok(r));

    if (cl->debug_state == 4) {
        bm_record_event_simple(RE_PBUF_REG, ts);
    }
}

#if 0
static void transmit_packet(struct ether_binding *cc, uint64_t nr_pbufs,
                            uint64_t buffer_id, uint64_t len, uint64_t offset,
                            uint64_t client_data)
{

#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NO_A, (uint32_t) client_data);
#endif                          // TRACE_ETHERSRV_MODE

    errval_t r;
    struct client_closure *closure = (struct client_closure *) cc->st;

    assert(closure != NULL);

    assert(closure->buffer_ptr->buffer_id == buffer_id);
    assert(nr_pbufs <= MAX_NR_TRANSMIT_PBUFS);


    if (closure->nr_transmit_pbufs == 0) {
        closure->nr_transmit_pbufs = nr_pbufs;
        closure->len = 0;
    }
    closure->pbuf[closure->rtpbuf].buffer_id = buffer_id;
    closure->pbuf[closure->rtpbuf].sr = cc;
    closure->pbuf[closure->rtpbuf].cc = closure;
    closure->pbuf[closure->rtpbuf].len = len;
    closure->pbuf[closure->rtpbuf].offset = offset;
    closure->pbuf[closure->rtpbuf].client_data = client_data;
    /* WARN: Most of this code is on assumption that app will send one
     * packet at one time, and there will be no packet pipelining. */
    closure->len = closure->len + len;  /* total lengh of packet */

    /* making the buffer memory cache coherent. */
    bulk_arch_prepare_recv((void *) closure->buffer_ptr->pa + offset, len);

    closure->rtpbuf++;
    if (closure->rtpbuf < closure->nr_transmit_pbufs) {
        /* all pbufs are not arrived yet, waiting for more pbufs associated
         * with this packet. */
        return;
    }


    /*we are done receiving all the pbufs from the application network
       stack and can transmit them finally */

    /* FIXME: ideally, one should check if this sender is allowed to send
     * this packet or not. */

    /* FIXME: this design expects more than one msg when packet does not
     * fit into one pbuf, I feel that it is bad design */

//        do {
    r = ether_transmit_pbuf_list_ptr(closure);
//        } while (r == ETHERSRV_ERR_CANT_TRANSMIT);

    //in case we cannot transmit, discard the _whole_ packet (just don't
    //enqueue transmit descriptors in the network card's ring)
    if (err_is_fail(r)) {
        ++closure->dropped_pkt_count;
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
    } else {
        // successfull transfer!
        ++closure->pkt_count;
        closure->pbuf_count = closure->pbuf_count + closure->nr_transmit_pbufs;
//            printf("Counter incremented %"PRIu64"\n", closure->pbuf_count);

    }

    //reset to indicate that a new packet will start
    closure->nr_transmit_pbufs = 0;
    closure->rtpbuf = 0;
    closure->len = 0;
    // Now check if there are any free TX slot from the packets
    // which are sent.
    while (handle_free_tx_slot_fn_ptr());
}
#endif // 0


static bool send_single_pkt_to_driver(struct ether_binding *cc)
{
//    uint64_t ts = rdtsc();
    struct client_closure *closure = (struct client_closure *)cc->st;
    assert(closure != NULL);
//    struct buffer_descriptor *buffer = closure->buffer_ptr;
//    assert(buffer != NULL);
    struct shared_pool_private *spp = closure->spp_ptr;
    assert(spp != NULL);
    assert(spp->sp != NULL);

    //reset to indicate that a new packet will start
    closure->nr_transmit_pbufs = 0;
    closure->rtpbuf = 0;

    errval_t r;
    // FIXME: keep the copy of ghost_read_id so that it can be restored
    // if something goes wrong
    uint64_t ghost_read_id_copy = spp->ghost_read_id;
    uint64_t current_spp_slot_id = spp->ghost_read_id;
    struct slot_data s;
    if (!sp_ghost_read_slot(spp, &s)) {
        return false;
    }

    // Some sanity checks
/*
    if (buffer->buffer_id != s.buffer_id) {
        printf("PROB: %"PRIu64" != %"PRIu64", no%"PRIu64", %"PRIu64" \n",
            buffer->buffer_id,  s.buffer_id, s.no_pbufs, s.len);
        sp_print_metadata(spp);
        assert(!"pbuf from wrong buffer");
    }
    assert(buffer->buffer_id == s.buffer_id);
*/

    assert(closure->nr_transmit_pbufs == 0);
    assert(s.no_pbufs <= MAX_NR_TRANSMIT_PBUFS);

    // FIXME: Make sure that there are s.no_pbuf slots available

    closure->nr_transmit_pbufs = s.no_pbufs;
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

    closure->hw_queue = closure->hw_queue + closure->rtpbuf;
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
    }
    else {
        // successfull traInsfer!
        if (closure->debug_state_tx == 4) {
            ++closure->pkt_count;
            closure->pbuf_count = closure->pbuf_count + closure->nr_transmit_pbufs;
//                bm_record_event_simple(RE_TX_SP_S, ts);
            if (closure->pkt_count == closure->out_trigger_counter) {
                benchmark_control_request(cc, BMS_STOP_REQUEST, 0, 0);
            }
        } else {
            if (closure->debug_state_tx == 3) {
                ++closure->pkt_count;
                if (closure->pkt_count == 1) {
                    // This is the first packet, so lets restart the timer!!
                    closure->start_ts_tx = rdtsc();
                    closure->debug_state_tx = 4;
                } else {
                    assert(!"Not possible!");
                }
            }
        }

//        printf("successful transer, counter incremented %"PRIu64"\n",
//                closure->pbuf_count);

    }
    closure->len = 0;
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

    if (closure->debug_state_tx == 4) {
        bm_record_event_simple(RE_TX_T, pkts);
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

static void export_ether_control_cb(void *st, errval_t err, iref_t iref)
{
    char service_name[100];

    snprintf(service_name, sizeof(service_name), "%s%s", my_service_name,
             FILTER_SERVICE_SUFFIX);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "service[%s] export failed", service_name);
        abort();
    }

    ETHERSRV_DEBUG("service [%s] exported at iref %u\n", service_name, iref);

    // register this iref with the name service
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for [%s]", service_name);
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
#if 0
/*
	err = frame_alloc(&frame, LAST_ACCESSED_BYTE_TRANSPORT * TRANSMIT_BUFFERS,
			NULL);
	if (err_is_fail(err)) {
		ETHERSRV_DEBUG("connection_service_logic: frame_alloc failed\n");
		free(buffer);
		free(cc);
		// FIXME: free cc->q also
		return err;
	}

	err = invoke_frame_identify(frame, &pa);
	assert(err_is_ok(err));
	cc->tx_private_mem_p = (pa.base);

	err = vspace_map_one_frame_attr((void*) (&(cc->tx_private_mem_v)),
			LAST_ACCESSED_BYTE_TRANSPORT * TRANSMIT_BUFFERS, frame,
			VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
	if (err_is_fail(err)) {
		assert(!"vspace_map_one_frame failed");
	}

*/
#endif                          // 0


    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}                               /* end function: connect_ether_cb */

static errval_t connect_ether_control_cb(void *st,
                                         struct ether_control_binding *b)
{
    ETHERSRV_DEBUG("ether_netd service got a connection!55\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_ether_control_vtbl;
    //b->error_handler = error_handler;

    struct client_closure_FM *ccfm =
      (struct client_closure_FM *) malloc(sizeof(struct client_closure_FM));

    b->st = ccfm;
    ccfm->q = create_cont_q("FILTER-MANAGER");
    ccfm->app_connection = b;

    /* FIXME: should I refuse more than one connections for FM services?
       Currently, I am accepting them */

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}                               /* end function: connect_ether_control_cb */


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
    filter_id_counter = 0;
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

    /* FIXME: do I need separate my_service_name for ether_netd services */
    /* exporting ether_netd interface */
    err = ether_control_export(NULL, export_ether_control_cb,
                               connect_ether_control_cb, get_default_waitset(),
                               IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "ethersrv_netd export failed");
        abort();
    }
}

/*****************************************************************
 *   filter registration
 *****************************************************************/

/* FIXME: provide proper handler here */
static errval_t send_resiger_filter_memory_response(struct q_entry entry)
{
    //    ETHERSRV_DEBUG("send_resigered_netd_memory  -----\n");
    struct ether_control_binding *b =
      (struct ether_control_binding *) entry.binding_ptr;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.register_filter_memory_response(b,
                                                          MKCONT
                                                          (cont_queue_callback,
                                                           ccfm->q),
                                                          entry.plist[0]);
        /* entry.error */
    } else {
        ETHERSRV_DEBUG("send_resigered_netd_memory Flounder bsy will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static struct bulk_transfer_slave bt_filter_rx;

static void register_filter_memory_request(struct ether_control_binding *cc,
                                           struct capref mem_cap)
{

    errval_t err = SYS_ERR_OK;

    struct frame_identity pa;

    err = invoke_frame_identify(mem_cap, &pa);
    assert(err_is_ok(err));

    ETHERSRV_DEBUG("register_netd_memory: attempt to register memory\n");
    // 2 is rx + tx
    if ((1L << pa.bits) < BASE_PAGE_SIZE * 2) {
        ETHERSRV_DEBUG("netd did not provided enough for filter transfer\n");
        err = FILTER_ERR_NOT_ENOUGH_MEMORY;     /* ps: FIXME: enable this error */

    } /* end if: not enough memory */
    else {                      /* enough memory, try to map it */
        void *pool;

        err = vspace_map_one_frame_attr((void *) (&pool), BASE_PAGE_SIZE * 2,
                                        mem_cap,
                                        VREGION_FLAGS_READ_WRITE_NOCACHE, NULL,
                                        NULL);

        if (err_is_fail(err)) {
            DEBUG_ERR(err, "vspace_map_one_frame failed");
            //            abort();
        } /* end if: mapping failed */
        else {
            // Init receiver
            err = bulk_slave_init(pool, BASE_PAGE_SIZE * 2, &bt_filter_rx);
            //            assert(err_is_ok(err));

        }                       /* end else: mapping sucessful */

    }                           /* end else : */

    /* call registered_netd_memory with new IDC */
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_resiger_filter_memory_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

    entry.plist[0] = err;
    /* entry.plist[0]
       entry.error */

    enqueue_cont_q(ccfm->q, &entry);
    ETHERSRV_DEBUG("register_netd_memory: sent IDC\n");

}                               /* end function : register_netd_memory */

/* Handler for sending response to register_filter */
static errval_t send_register_filter_response(struct q_entry e)
{
    //    ETHERSRV_DEBUG("send_resigered_filter for ID %lu  --\n", e.plist[0]);
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.register_filter_response(b,
                                                   MKCONT(cont_queue_callback,
                                                          ccfm->q), e.plist[0],
                                                   e.plist[1], e.plist[2],
                                                   e.plist[3], e.plist[4],
                                                   e.plist[5]);
        /* e.id,       e.error,    e.filter_id, e.buffer_id_rx,
         * e.buffer_id_tx, e.filter_type */

    } else {
        ETHERSRV_DEBUG("send_resigered_filter: ID %" PRIu64
                       ": Flounder bsy will retry\n", e.plist[0]);
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void wrapper_send_filter_registered_msg(struct ether_control_binding *cc,
                                               uint64_t id, errval_t err,
                                               uint64_t filter_id,
                                               uint64_t buffer_id_rx,
                                               uint64_t buffer_id_tx,
                                               uint64_t ftype)
{

    /* call registered_netd_memory with new IDC */

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_register_filter_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

    entry.plist[0] = id;
    entry.plist[1] = err;
    entry.plist[2] = filter_id;
    entry.plist[3] = buffer_id_rx;
    entry.plist[4] = buffer_id_tx;
    entry.plist[5] = ftype;
    // e.plist[0], e.plist[1], e.plist[2],  e.plist[3], e.plist[4], e.plist[5])
    // id, error, filter_id, buffer_id_rx, buffer_id_tx, filter_type

    enqueue_cont_q(ccfm->q, &entry);

}

/**
 * \brief: Registers the filter with network driver
 */
static void register_filter(struct ether_control_binding *cc, uint64_t id,
                            uint64_t len_rx, uint64_t len_tx,
                            uint64_t buffer_id_rx, uint64_t buffer_id_tx,
                            uint64_t ftype, uint64_t paused)
{
    errval_t err = SYS_ERR_OK;

    ETHERSRV_DEBUG("Register_filter: ID:%" PRIu64 " of type[%" PRIu64
                   "] buffers RX[%" PRIu64 "] and TX[%" PRIu64 "]\n", id, ftype,
                   buffer_id_rx, buffer_id_tx);

    struct buffer_descriptor *buffer_rx = NULL;
    struct buffer_descriptor *buffer_tx = NULL;
    struct buffer_descriptor *tmp = buffers_list;

    while (tmp) {

        if (tmp->buffer_id == buffer_id_tx) {
            buffer_tx = tmp;
        }

        if (tmp->buffer_id == buffer_id_rx) {
            buffer_rx = tmp;
        }

        if (buffer_rx != NULL && buffer_tx != NULL) {
            break;
        }

        tmp = tmp->next;
    }                           /* end while : */

    if (buffer_rx == NULL || buffer_tx == NULL) {
        ETHERSRV_DEBUG("no buffer found for the provided buffer id\n");
        err = FILTER_ERR_BUFF_NOT_FOUND;

        wrapper_send_filter_registered_msg(cc, id, err, 0, buffer_id_rx,
                                           buffer_id_tx, ftype);
        return;
    }

    if (len_rx > BASE_PAGE_SIZE) {
        len_rx = BASE_PAGE_SIZE;
    }

    if (len_tx > BASE_PAGE_SIZE) {
        len_tx = BASE_PAGE_SIZE;
    }

    /* using id to find the location of memory */
    void *buf = bulk_slave_buf_get_mem(&bt_filter_rx, id, NULL);

    if (buf == NULL) {
        ETHERSRV_DEBUG("no memory available for filter transfer\n");
        err = FILTER_ERR_NO_NETD_MEM;
        wrapper_send_filter_registered_msg(cc, id, err, 0, buffer_id_rx,
                                           buffer_id_tx, ftype);
        return;
    }

    /* Create the filter data-structures */
    struct filter *new_filter_rx =
      (struct filter *) malloc(sizeof(struct filter));
    struct filter *new_filter_tx =
      (struct filter *) malloc(sizeof(struct filter));

    /* FIXME: use goto to deal with failure conditions and reduce the code */
    if (new_filter_rx == NULL || new_filter_tx == NULL) {
        ETHERSRV_DEBUG("out of memory for filter registration\n");
        err = ETHERSRV_ERR_NOT_ENOUGH_MEM;
        wrapper_send_filter_registered_msg(cc, id, err, 0, buffer_id_rx,
                                           buffer_id_tx, ftype);

        if (new_filter_rx) {
            free(new_filter_rx);
        }

        if (new_filter_tx) {
            free(new_filter_tx);
        }
        return;
    }

    /* Zero out the filters */
    memset(new_filter_rx, 0, sizeof(struct filter));
    memset(new_filter_tx, 0, sizeof(struct filter));

    /* Allocate memory for holding the filter-data */
    new_filter_rx->data = (uint8_t *) malloc(len_rx);
    new_filter_tx->data = (uint8_t *) malloc(len_tx);

    if (new_filter_rx->data == NULL || new_filter_tx->data == NULL) {
        ETHERSRV_DEBUG("out of memory for filter data registration\n");
        err = ETHERSRV_ERR_NOT_ENOUGH_MEM;
        wrapper_send_filter_registered_msg(cc, id, err, 0, buffer_id_rx,
                                           buffer_id_tx, ftype);

        if (new_filter_rx->data) {
            free(new_filter_rx->data);
        }

        if (new_filter_tx->data) {
            free(new_filter_tx->data);
        }

        free(new_filter_rx);
        free(new_filter_tx);

        return;
    }

    /* Zero-out the area of filter-data */
    memset(new_filter_rx->data, 0, len_rx);
    memset(new_filter_tx->data, 0, len_tx);

    filter_id_counter++;

    // rx filter
    memcpy(new_filter_rx->data, buf, len_rx);
    new_filter_rx->len = len_rx;
    new_filter_rx->filter_id = filter_id_counter;
    new_filter_rx->filter_type = ftype;
    new_filter_rx->buffer = buffer_rx;
    new_filter_rx->next = rx_filters;
    new_filter_rx->paused = paused ? true : false;
    rx_filters = new_filter_rx;
    ETHERSRV_DEBUG("filter registered with id %" PRIu64 " and len %d\n",
                   new_filter_rx->filter_id, new_filter_rx->len);

    // tx filter
    void *bbuf_tx = buf + BASE_PAGE_SIZE;

    memcpy(new_filter_tx->data, bbuf_tx, len_tx);
    new_filter_tx->len = len_tx;
    new_filter_tx->filter_id = filter_id_counter;
    new_filter_tx->filter_type = ftype;
    new_filter_tx->buffer = buffer_tx;  // we do not really need to set this
    /* FIXME: following linked list implementation looks buggy */
    new_filter_tx->next = buffer_tx->tx_filters;
    buffer_tx->tx_filters = new_filter_tx;
    /* FIXME: following looks buggy!!! */
    buffer_rx->tx_filters = new_filter_tx;      // sometimes rx buffers transmit

    /* reporting back the success/failure */
    wrapper_send_filter_registered_msg(cc, id, err, filter_id_counter,
                                       buffer_id_rx, buffer_id_tx, ftype);

    ETHERSRV_DEBUG("Register_filter: ID %" PRIu64 ": type[%" PRIu64
                   "] successful [%" PRIu64 "]\n", id, ftype,
                   filter_id_counter);

}                               /* end function: register filter */


/* Handler for sending response to deregister_filter */
static errval_t send_deregister_filter_response(struct q_entry e)
{
    //    ETHERSRV_DEBUG("send_deresigered_filter_response for ID %lu  -----\n", e.plist[0]);
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.deregister_filter_response(b,
                                                     MKCONT(cont_queue_callback,
                                                            ccfm->q),
                                                     e.plist[0], e.plist[1]);
        /* e.error,    e.filter_id,  */

    } else {
        ETHERSRV_DEBUG("send_deresiger_filter_response: Filter_ID %" PRIu64
                       ": Flounder bsy will retry\n", e.plist[1]);
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void wrapper_send_filter_deregister_msg(struct ether_control_binding *cc,
                                               errval_t err, uint64_t filter_id)
{

    /* call registered_netd_memory with new IDC */

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_deregister_filter_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

    entry.plist[0] = err;
    entry.plist[1] = filter_id;
    /* e.plist[0], e.plist[1] );
       e.error,    e.filter_id */
    enqueue_cont_q(ccfm->q, &entry);
}

static struct filter *delete_from_filter_list(struct filter *head,
                                              uint64_t filter_id)
{
    struct filter *prev = NULL;

    while (head != NULL) {
        if (head->filter_id == filter_id) {
            if (prev == NULL) {
                rx_filters = head->next;
            } else {
                prev->next = head->next;
            }
            return head;
        }                       /* end if: filter_id found */
    }                           /* end while: for each element in list */
    return NULL;                /* could not not find the id. */
}


/**
 * \brief: Deregisters the filter with network driver
 */
static void deregister_filter(struct ether_control_binding *cc,
                              uint64_t filter_id)
{
    errval_t err = SYS_ERR_OK;

    ETHERSRV_DEBUG("DeRegister_filter: ID:%" PRIu64 "\n", filter_id);

    /* Create the filter data-structures */
    struct filter *rx_filter = NULL;
    struct filter *tx_filter = NULL;

    rx_filter = delete_from_filter_list(rx_filters, filter_id);
    /* FIXME: delete the tx_filter from the filter list "buffer_rx->tx_filters" */
//    tx_filter = delete_from_filter_list(tx_filters, filter_id);

    if (rx_filter == NULL /*|| tx_filter == NULL */ ) {
        ETHERSRV_DEBUG("Deregister_filter:requested filter_ID [%" PRIu64
                       "] not found\n", filter_id);
        err = FILTER_ERR_FILTER_NOT_FOUND;
    }

    if (rx_filter) {
        free(rx_filter);
    }

    if (tx_filter) {
        free(tx_filter);
    }

    /* reporting back the success/failure */
    wrapper_send_filter_deregister_msg(cc, err, filter_id);

    ETHERSRV_DEBUG("Deregister_filter: ID %" PRIu64 ": Done\n", filter_id);

}                               /* end function: deregister_filter */



/* Handler for sending response to re register_filter */
static errval_t send_re_register_filter_response(struct q_entry e)
{
    //    ETHERSRV_DEBUG("send_re_register_filter_response for ID %lu  -----\n", e.plist[0]);
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.re_register_filter_response(b,
                                                      MKCONT
                                                      (cont_queue_callback,
                                                       ccfm->q), e.plist[0],
                                                      e.plist[1], e.plist[2],
                                                      e.plist[2]);
        /* e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_rx */

    } else {
        ETHERSRV_DEBUG("send_re_register_filter_response: Filter_ID %" PRIu64
                       ": Flounder bsy will retry\n", e.plist[1]);
        return FLOUNDER_ERR_TX_BUSY;
    }
}                               /* end function: send_re_register_filter_response */

static errval_t send_pause_filter_response(struct q_entry e)
{
    //    ETHERSRV_DEBUG("send_re_register_filter_response for ID %lu  -----\n", e.plist[0]);
    struct ether_control_binding *b =
      (struct ether_control_binding *) e.binding_ptr;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.pause_response(b,
                                         MKCONT(cont_queue_callback, ccfm->q),
                                         e.plist[0], e.plist[1]);
        /* e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_rx */

    } else {
        ETHERSRV_DEBUG("send_re_register_filter_response: Filter_ID %" PRIu64
                       ": Flounder bsy will retry\n", e.plist[1]);
        return FLOUNDER_ERR_TX_BUSY;
    }
}                               /* end function: send_re_register_filter_response */

static void wrapper_send_filter_re_register_msg(struct ether_control_binding
                                                *cc, errval_t err,
                                                uint64_t filter_id,
                                                uint64_t buffer_id_rx,
                                                uint64_t buffer_id_tx)
{

    /* call registered_netd_memory with new IDC */

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_re_register_filter_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

    entry.plist[0] = err;
    entry.plist[1] = filter_id;
    entry.plist[2] = buffer_id_rx;
    entry.plist[3] = buffer_id_tx;
/*    e.plist[0], e.plist[1],  e.plist[2],     e.plist[2]
      e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_rx */
    enqueue_cont_q(ccfm->q, &entry);
}                               /* end function: wrapper_send_filter_re_register_msg */

static void wrapper_send_filter_pause_msg(struct ether_control_binding *cc,
                                          errval_t err, uint64_t filter_id)
{

    /* call registered_netd_memory with new IDC */

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_pause_filter_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

    entry.plist[0] = filter_id;
    entry.plist[1] = err;
/*    e.plist[0], e.plist[1],  e.plist[2],     e.plist[2]
      e.error,    e.filter_id, e.buffer_id_rx, e.buffer_id_rx */
    enqueue_cont_q(ccfm->q, &entry);
}                               /* end function: wrapper_send_filter_re_register_msg */


static struct filter *find_from_filter_list(struct filter *head,
                                            uint64_t filter_id)
{
    while (head != NULL) {
        if (head->filter_id == filter_id) {
            return head;
        }                       /* end if: filter_id found */
        head = head->next;
    }                           /* end while: for each element in list */
    return NULL;                /* could not not find the id. */
}

/**
 * \brief: re-registers the filter with network driver
 */
static void re_register_filter(struct ether_control_binding *cc,
                               uint64_t filter_id, uint64_t buffer_id_rx,
                               uint64_t buffer_id_tx)
{
    errval_t err = SYS_ERR_OK;

    ETHERSRV_DEBUG("re_register_filter: ID:%" PRIu64 "\n", filter_id);

    /* Create the filter data-structures */
    struct filter *rx_filter = NULL;

//    struct filter *tx_filter = NULL;

    rx_filter = find_from_filter_list(rx_filters, filter_id);
    /* FIXME: delete the tx_filter from the filter list "buffer_rx->tx_filters" */
//    tx_filter = delete_from_filter_list(tx_filters, filter_id);

    if (rx_filter == NULL /*|| tx_filter == NULL */ ) {
        ETHERSRV_DEBUG("re_register_filter: requested filter_ID [%" PRIu64
                       "] not found\n", filter_id);
        err = FILTER_ERR_FILTER_NOT_FOUND;
        wrapper_send_filter_re_register_msg(cc, err, filter_id,
                                            buffer_id_rx, buffer_id_tx);
        return;
    }
    /* Find the buffer with given buffer_id */
    struct buffer_descriptor *buffer_rx = find_buffer(buffer_id_rx);
    struct buffer_descriptor *buffer_tx = NULL;

    buffer_tx = find_buffer(buffer_id_tx);
    if (buffer_rx == NULL || buffer_rx == NULL) {
        ETHERSRV_DEBUG("re_register_filter: provided buffer id's not found\n");
        ETHERSRV_DEBUG("re_register_filter: rx=[[%" PRIu64 "] = %p], tx=[[%"
                       PRIu64 "] = %p]\n", buffer_id_rx, buffer_rx,
                       buffer_id_tx, buffer_tx);
        err = FILTER_ERR_BUFFER_NOT_FOUND;      /* set error value */
        wrapper_send_filter_re_register_msg(cc, err, filter_id, buffer_id_rx,
                                            buffer_id_tx);
    }
    rx_filter->buffer = buffer_rx;
    /* FIXME: Also, set the new buffer for tx_filters */
    /* reporting back the success/failure */
    wrapper_send_filter_re_register_msg(cc, err, filter_id, buffer_id_rx,
                                        buffer_id_tx);

    ETHERSRV_DEBUG("re_register_filter: ID %" PRIu64 ": Done\n", filter_id);

}                               /* end function: re_register_filter */

/**
 * \brief: pause the filter with network driver
 */
static void pause_filter(struct ether_control_binding *cc, uint64_t filter_id,
                         uint64_t buffer_id_rx, uint64_t buffer_id_tx)
{
    errval_t err = SYS_ERR_OK;

    ETHERSRV_DEBUG("(un)pause_filter: ID:%" PRIu64 "\n", filter_id);

    /* Create the filter data-structures */
    struct filter *rx_filter = NULL;

//    struct filter *tx_filter = NULL;

    rx_filter = find_from_filter_list(rx_filters, filter_id);
    /* FIXME: delete the tx_filter from the filter list "buffer_rx->tx_filters" */
//    tx_filter = delete_from_filter_list(tx_filters, filter_id);

    if (rx_filter == NULL /*|| tx_filter == NULL */ ) {
        ETHERSRV_DEBUG("pause_filter: requested filter_ID [%" PRIu64
                       "] not found\n", filter_id);
        err = FILTER_ERR_FILTER_NOT_FOUND;
        assert(!"NYI");
        /* wrapper_send_filter_re_register_msg(cc, err, filter_id, */
        /*         buffer_id_rx, buffer_id_tx); */
        return;
    }

    /* Find the buffer with given buffer_id */
    struct buffer_descriptor *buffer_rx = find_buffer(buffer_id_rx);
    struct buffer_descriptor *buffer_tx = NULL;

    buffer_tx = find_buffer(buffer_id_tx);
    if (buffer_rx == NULL || buffer_rx == NULL) {
        ETHERSRV_DEBUG("re_register_filter: provided buffer id's not found\n");
        ETHERSRV_DEBUG("re_register_filter: rx=[[%" PRIu64 "] = %p], tx=[[%"
                       PRIu64 "] = %p]\n", buffer_id_rx, buffer_rx,
                       buffer_id_tx, buffer_tx);
        assert(!"NYI");
        /* err =  FILTER_ERR_BUFFER_NOT_FOUND; /\* set error value *\/ */
        /* wrapper_send_filter_re_register_msg(cc, err, filter_id, buffer_id_rx, */
        /*         buffer_id_tx); */
    }
    rx_filter->buffer = buffer_rx;
    /* FIXME: Also, set the new buffer for tx_filters */
    /* reporting back the success/failure */
    wrapper_send_filter_pause_msg(cc, err, filter_id);

    rx_filter->paused = false;
    if (rx_filter->pause_bufpos > 0) {
        for (int i = 0; i < rx_filter->pause_bufpos; i++) {
            struct bufdesc *bd = &rx_filter->pause_buffer[i];

            struct ether_binding *b = rx_filter->buffer->con;
            assert(b != NULL);
            struct client_closure *cl = (struct client_closure *)b->st;
            assert(cl != NULL);
            if (cl->debug_state == 4) {
                ++cl->in_paused_pkts;
            }
            copy_packet_to_user(rx_filter->buffer, bd->pkt_data, bd->pkt_len);
        }
    }
    rx_filter->pause_bufpos = 0;

    ETHERSRV_DEBUG("(un)pause_filter: ID %" PRIu64 ": Done\n", filter_id);

}                               /* end function: re_register_filter */


/* Handler for sending response to register_filter */
static errval_t send_register_arp_filter_response(struct q_entry entry)
{
    //    ETHERSRV_DEBUG("send_resigered_arp_filter  -----\n");
    struct ether_control_binding *b =
      (struct ether_control_binding *) entry.binding_ptr;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.register_arp_filter_response(b,
                                                       MKCONT
                                                       (cont_queue_callback,
                                                        ccfm->q),
                                                       entry.plist[0],
                                                       entry.plist[1]);
        /* e.id,        e.error */

    } else {
        ETHERSRV_DEBUG("send_resigered_arp_filter Flounder bsy will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void wrapper_send_arp_filter_registered_msg(struct ether_control_binding
                                                   *cc, uint64_t id,
                                                   errval_t err)
{

    /* call registered_netd_memory with new IDC */

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_register_arp_filter_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure_FM *ccfm = (struct client_closure_FM *) cc->st;

    entry.plist[0] = id;
    entry.plist[1] = err;
    /* entry.plist[0], entry.plist[1]
       id,             e.error */

    enqueue_cont_q(ccfm->q, &entry);
}

static void register_arp_filter(struct ether_control_binding *cc, uint64_t id,
                                uint64_t len_rx, uint64_t len_tx)
{

    errval_t err = SYS_ERR_OK;

    if (len_rx > BASE_PAGE_SIZE) {
        len_rx = BASE_PAGE_SIZE;
    }
    if (len_tx > BASE_PAGE_SIZE) {
        len_tx = BASE_PAGE_SIZE;
    }

    /* using id to find the location of memory */
    void *buf = bulk_slave_buf_get_mem(&bt_filter_rx, id, NULL);

    if (buf == NULL) {
        ETHERSRV_DEBUG("no memory available for arp_filter transfer\n");
        err = FILTER_ERR_NO_NETD_MEM;
        wrapper_send_arp_filter_registered_msg(cc, id, err);
        return;
    }

    arp_filter_rx.data = (uint8_t *) malloc(len_rx);
    assert(arp_filter_rx.data);
    memcpy(arp_filter_rx.data, buf, len_rx);
    arp_filter_rx.len = len_rx;
    ETHERSRV_DEBUG("#### The received arp RX filter is\n");
    //    show_binary_blob(arp_filter_rx.data, arp_filter_rx.len);

    void *bbuf_tx = buf + BASE_PAGE_SIZE;

    arp_filter_tx.data = (uint8_t *) malloc(len_tx);
    assert(arp_filter_tx.data);
    memcpy(arp_filter_tx.data, bbuf_tx, len_tx);
    arp_filter_tx.len = len_tx;
    ETHERSRV_DEBUG("#### The received arp RX filter is\n");
    //    show_binary_blob(arp_filter_tx.data, arp_filter_tx.len);

    wrapper_send_arp_filter_registered_msg(cc, id, err);
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
    if(!sp_set_read_index(spp, ((spp_index + 1) % spp->c_size))) {
        // FIXME:  This is dengarous!  I should increase read index,
        // only when all the packets till that read index are sent!
//        printf("failed for %"PRIu64"\n",spp_index);
//        sp_print_metadata(spp);
//        assert(!"sp_set_read_index failed");
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

static errval_t send_received_packet_handler(struct q_entry entry)
{
	ETHERSRV_DEBUG("send_received_packet_handler id %lu pbuf %p\n",
			entry.plist[0], (void *)entry.plist[1]);

    struct ether_binding *b = (struct ether_binding *) entry.binding_ptr;
    struct client_closure *cl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        errval_t err;

        if (entry.plist[2] == 0 || entry.plist[1] == 0) {
            /* FIXME: handle this error in better way. */
            ETHERSRV_DEBUG("##ERROR: trying to send pbuf_id %" PRIx64 " at %"
                           PRIx64 " " "of size %" PRIx64 " and len %" PRIx64
                           "\n", entry.plist[0], entry.plist[1], entry.plist[2],
                           entry.plist[3]);
            assert(entry.plist[2] != 0);
        }

#if TRACE_ETHERSRV_MODE
        trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_S,
                    (uint32_t) entry.plist[0]);
#endif                          // TRACE_ETHERSRV_MODE

       uint8_t canary_val = 6;
        queue_set_canary(cl->q, canary_val);
        uint64_t ts = rdtsc();
        err = b->tx_vtbl.packet_received(b,
                                         MKCONT(cont_queue_callback, cl->q),
                                         entry.plist[0], entry.plist[1],
                                         entry.plist[2], entry.plist[3],
                                         ts);
        /* entry.pbuf_id,  entry.paddr,    entry.len,      entry.length,
         * timestamp */

        if ((cl->debug_state == 4) ) {
            bm_record_event_simple(RE_PKT_RECV_MSG, ts);
            bm_record_event_simple(RE_PKT_RECV_Q, entry.plist[5]);
        }
        uint64_t delta = rdtsc() - ts;
        uint8_t ans_canary = queue_get_canary(cl->q);
        if (canary_val != ans_canary) {
            printf("Something else happened inbetween! %"PRIu8"\n", ans_canary);
        }
        if (err_is_fail(err)) {
            if (cl->debug_state == 4) {
                ++cl->in_dropped_notification_prob2;
            }
        } else {
            // Following condition will filter out ARP packets
            // and non-debug state
            if ((cl->debug_state == 4) ) {
//                if (cl->filter_matched == 1) {
                if (entry.plist[4] == 1) {
                    ++cl->in_success;
                    // Updating stats
                    ++cl->in_app_time_n;
                    cl->in_app_time_sum += delta;
                    if(cl->in_app_time_n == 1) {
                        cl->in_app_time_max = delta;
                        cl->in_app_time_min = delta;
                    } else {
                        if(delta > cl->in_app_time_max) {
                            cl->in_app_time_max = delta;
                        }
                        if(delta < cl->in_app_time_min) {
                            cl->in_app_time_min = delta;
                        }
                    }
                    if (cl->in_trigger_counter == 1) {
                        benchmark_control_request(b, BMS_STOP_REQUEST, 0, 0);
                    }
                    --cl->in_trigger_counter;

/*                    if (cl->in_success == 1) {
                        assert(cl->debug_state == 3);
                        // This is the first packet, so record the start time!
                        cl->start_ts = rdtsc();
                        cl->debug_state = 4; // now setting the debugging
                    }
*/
                } else {
                    ++cl->in_other_pkts;
                }
            } // end if : debug_state
        } // end if : notification sent!


        /* FIXME: what is this? why is it here? */
        /* FIXME: I am assuming here that packet has been properly uploaded */
        cl->buffer_ptr->pbuf_head_msg = (cl->buffer_ptr->pbuf_head_msg + 1)
          % APP_QUEUE_SIZE;

        cl->filter_matched = 0;
        /* FIXME: As I have sent the msg here, shouldn't I treat this pbuf
         * as un-initialzed ?*/
        return err;
    } else {
        ETHERSRV_DEBUG
          ("send_received_packet_handler: Flounder busy,rtry+++++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/* enqueues the pbuf from the top of the queue (buffer->head) */
static bool send_packet_received_notification(struct buffer_descriptor *buffer,
                                              struct pbuf_desc *rx_pbuf)
{

    struct client_closure *ccl = (struct client_closure *) buffer->con->st;

    if (buffer->pbuf_head == buffer->pbuf_head_msg) {
        return false;
    }

    assert(rx_pbuf);

#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_P,
                (uint32_t) rx_pbuf->pbuf_id);
#endif                          // TRACE_ETHERSRV_MODE

    bulk_arch_prepare_send((void *) (uintptr_t) rx_pbuf->vaddr, rx_pbuf->len);

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_received_packet_handler;
    entry.binding_ptr = (void *) buffer->con;
    entry.plist[0] = rx_pbuf->pbuf_id;
    entry.plist[1] = rx_pbuf->paddr;    /* the physical address of pbuf payload */
    entry.plist[2] = rx_pbuf->len;
    entry.plist[3] = rx_pbuf->packet_size;
    entry.plist[4] = ccl->filter_matched;
    entry.plist[5] = rdtsc();
    if (entry.plist[2] == 0 || entry.plist[1] == 0) {
        ETHERSRV_DEBUG("## trying to enqueue pbuf_id %" PRIx64 " at %" PRIx64
                       " of size %" PRIx64 " and len %" PRIx64 "\n",
                       entry.plist[0], entry.plist[1], entry.plist[2],
                       entry.plist[3]);
        assert(entry.plist[2] != 0);
    }

    enqueue_cont_q(ccl->q, &entry);
    //    ETHERSRV_DEBUG("send_packet_received done\n");
    /* entry.plist[0], entry.plist[1], entry.plist[2], entry.plist[3] */
    /* entry.pbuf_id,  entry.paddr,    entry.len,      entry.length */

    return true;
}


bool copy_packet_to_user(struct buffer_descriptor * buffer,
                         void *data, uint64_t len)
{

    uint32_t phead, ptail;

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

    phead = buffer->pbuf_head;
    ptail = buffer->pbuf_tail;

    ETHERSRV_DEBUG("Copy_packet_2_usr_buf [%" PRIu64 "]: phead[%u] ptail[%u]\n",
                   buffer->buffer_id, buffer->pbuf_head, buffer->pbuf_tail);

    struct pbuf_desc *upbuf = &pbuf_list[phead];

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

    if (((phead + 1) % APP_QUEUE_SIZE) == ptail) {

        ETHERSRV_DEBUG("[%d]no space in userspace 2cp pkt buf [%" PRIu64
                       "]: phead[%u] ptail[%u]\n", disp_get_domain_id(),
                       buffer->buffer_id, buffer->pbuf_head, buffer->pbuf_tail);
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

    // update the spp indicating the new packet
    // need to increment write pointer
    struct shared_pool_private *spp_ptr = cl->spp_ptr;
    assert(validate_and_empty_produce_slot(spp_ptr, upbuf->spp_index));
    phead = (phead + 1) % APP_QUEUE_SIZE;
    buffer->pbuf_head = phead;

    // add newly available pbuf slots into app-queue and hardware queue
    uint64_t count = 0;
    count = add_new_pbufs_2_app_ring(buffer->con, spp_ptr,
            buffer->buffer_id);
    ETHERSRV_DEBUG("cp_pkt_2_usr: added %"PRIu64" pbufs into app ring\n",
            count);

    bool success = send_packet_received_notification(buffer, upbuf);
    if (!success) {
         if (cl->debug_state == 4) {
            ++cl->in_dropped_notification_prob;
        }
    }
    return success;
}


static void send_arp_to_all(void *data, uint64_t len)
{
    struct filter *head = rx_filters;

    ETHERSRV_DEBUG("### Sending the ARP packet to all, %"PRIx64" \n", len);
    /* sending ARP packets to only those who have registered atleast one
     * filter with e1000n
     * */

    /* FIXME: this code will send two copies or ARP if there are two filters
     * registered, which is incorrect.  Fix it. */
    struct ether_binding *b = NULL;
    struct client_closure *cl = NULL;
    while (head) {
        b = head->buffer->con;
        assert(b != NULL);
        cl = (struct client_closure *) b->st;
        assert(cl != NULL);
        cl->filter_matched = 0;
        if (cl->debug_state == 4) {
            ++cl->in_arp_pkts;
        }

        copy_packet_to_user(head->buffer, data, len);
        head = head->next;
    }

    // Forwarding it to netd as well.
    struct buffer_descriptor *buffer = ((struct client_closure *)
                                        (netd[RECEIVE_CONNECTION]->st))->
      buffer_ptr;


#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_ARP,
                (uint32_t) (uintptr_t) data);
#endif // TRACE_ETHERSRV_MODE

    copy_packet_to_user(buffer, data, len);
}


struct filter *execute_filters(void *data, size_t len)
{
    struct filter *head = rx_filters;
    int res, error;

//      ETHERSRV_DEBUG("Starting the filter matching....\n");
    // TODO: gracefully handle the error cases, although I think
    // it is not really necessary. since it could only mean we have
    // received a corrupted packet.
    while (head) {
        res = execute_filter(head->data, head->len, (uint8_t *) data,
                             len, &error);
        if (res) {
            // FIXME IK: we need some way of testing how precise a match is
            // and take the most precise match (ie with the least wildcards)
            // Currently we just take the most recently added filter as
            // reflected by the order in the list.
            ETHERSRV_DEBUG("##### Filter_id [%" PRIu64 "] type[%" PRIu64
                           "] matched giving buff [%" PRIu64 "]..\n",
                           head->filter_id, head->filter_type,
                           head->buffer->buffer_id);
            return head;
        }
        head = head->next;
    }
    return NULL;
}

#if 0
static bool only_one_user_app(void)
{
    if (buffer_id_counter == 3 || buffer_id_counter == 4) {
        if (first_app_b != NULL) {
            return true;
        }
    }
    return false;
}
#endif // 0

void process_received_packet(void *pkt_data, size_t pkt_len)
{
    struct buffer_descriptor *buffer = NULL;

#if TRACE_ETHERSRV_MODE
    uint32_t pkt_location = (uint32_t) ((uintptr_t) pkt_data);

    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_A, pkt_location);
#endif                          // TRACE_ETHERSRV_MODE

    /* check if there is only one application,
     * then directly transfer the packet. */

#if 0
    if (only_one_user_app()) {
/*
		printf("Taking single app path with buff id %lu\n",
				first_app_b->buffer_id);

		if(copy_packet_to_user(first_app_b, pkt_data, pkt_len) == false) {
			printf("SA: Copy packet to userspace failed\n");
		}
//		printf("Application packet arrived for buff %lu\n", buffer->buffer_id);
		return;
*/
    }
#endif // 0

    if (handle_fragmented_packet(pkt_data, pkt_len)) {
        ETHERSRV_DEBUG("fragmented packet..\n");
//        printf("fragmented packet..\n");
        return;
    }

#if TRACE_ETHERSRV_MODE
    pkt_location = (uint32_t) ((uintptr_t) pkt_data);
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_FILTER_FRAG, pkt_location);
#endif                          // TRACE_ETHERSRV_MODE

    //      printf("normal non-SA mode packet\n");
    /* buffer = execute_filters(pkt_data, pkt_len); */

    // executing filters to find the relevant buffer
    struct filter *filter;
    uint64_t ts = rdtsc();
    filter = execute_filters(pkt_data, pkt_len);
    bm_record_event_simple(RE_FILTER, ts);
    if (filter != NULL) {
/*             printf("multiple app path with buff id %lu\n",
                               buffer->buffer_id);
*/

        buffer = filter->buffer;
        struct ether_binding *b = buffer->con;
        assert(b != NULL);
        struct client_closure *cl = (struct client_closure *) b->st;
        assert(cl != NULL);

        if (cl->debug_state == 3) {
            // Trigger to start the recording the stats
            assert(cl->in_success == 0);
            printf("Actually starting the tracking!!\n");
            cl->start_ts = rdtsc();
            cl->debug_state = 4;
            interrupt_counter = 0;
            interrupt_loop_counter = 0;
        }
        cl->filter_matched = 1;

        if (filter->paused) {
            assert(filter->pause_bufpos < MAX_PAUSE_BUFFER);
            struct bufdesc *bd = &filter->pause_buffer[filter->pause_bufpos++];

            memcpy_fast(bd->pkt_data, pkt_data, pkt_len);
            bd->pkt_len = pkt_len;
        } else {
            if (cl->debug_state == 4) {
                ++cl->in_filter_matched;
            }
            bool ret = copy_packet_to_user(buffer, pkt_data, pkt_len);
            if (ret == false) {
                if (cl->debug_state == 4) {
                    ++cl->in_filter_matched_f;
                    bm_record_event_simple(RE_DROPPED, ts);
                }
//                      printf("A: Copy packet to userspace failed\n");
            } else {
                if (cl->debug_state == 4) {
                    ++cl->in_filter_matched_p;
                    bm_record_event_simple(RE_USEFUL, ts);
                }
            }

        }
        //debug_printf("Application packet arrived for buff %lu\n",
        //buffer->buffer_id);
        return;
    }
// add trace filter_execution_end_1
#if TRACE_ETHERSRV_MODE
    pkt_location = (uint32_t) ((uintptr_t) pkt_data);
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_FILTER_EX_1, pkt_location);
#endif                          // TRACE_ETHERSRV_MODE


    /* no filter is succeeded. So, two case could happen:
       1: arp packets. in this case we copy the arp packet to
       every lwip instance which has an active filter, including netd
       2: any other packet which includes dhcp responses. we copy
       these to the netd. */

    int32_t res = 0;
    int32_t error;

    if (arp_filter_rx.data != NULL) {
/*      ETHERSRV_DEBUG("ARP compare data[%p], len %d\n",
				data, (int)len);
		show_binary_blob(arp_filter_rx.data, arp_filter_rx.len);
*/
        res = execute_filter(arp_filter_rx.data, arp_filter_rx.len,
                             (uint8_t *) pkt_data, pkt_len, &error);

    }

    if (res) {                  // we have an arp packet
//      ETHERSRV_DEBUG("ARP packet...\n");
        send_arp_to_all(pkt_data, pkt_len);
        return;
    }
    // add trace filter_execution_end_2
#if TRACE_ETHERSRV_MODE
    pkt_location = (uint32_t) ((uintptr_t) pkt_data);
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_FILTER_EX_2, pkt_location);
#endif                          // TRACE_ETHERSRV_MODE

    /* assuming that it is netd */
//  ETHERSRV_DEBUG("No client wants, giving it to netd\n");
    buffer = ((struct client_closure *)
              (netd[RECEIVE_CONNECTION]->st))->buffer_ptr;

//    ETHERSRV_DEBUG("sending packet up.\n");
    /* copy the packet to userspace */
    if(buffer == NULL) {
        printf("netd buffer not present\n");
        return;
    }

    struct ether_binding *b = buffer->con;
    if(b == NULL) {
        printf("netd buffer->con not present\n");
        return;
    }

    struct client_closure *cl = (struct client_closure *)b->st;
    assert(cl != NULL);
    if (cl->debug_state == 4) {
        ++cl->in_netd_pkts;
    }
    if (copy_packet_to_user(buffer, pkt_data, pkt_len) == false) {
        ETHERSRV_DEBUG("Copy packet to userspace failed\n");
        /* AB: commented out printf here. If we're on a busy network
         * and packets keep arriving, the rate of debug printing is
         * slower than the packet arrival rate, and we get stuck in a
         * loop printing out these messages that we are discarding packets.
         */
        // printf("O:Copy packet to userspace failed\n");
    }
} // end function: process_received_packet


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
    uint8_t bm_type = 0; // 0 = RX benchmark
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
            printf("TX Explicit msg needed [%"PRIu64"], sent[%"PRIu64"]\n",
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
static uint64_t stats[EVENT_LIST_SIZE][RDT_LIST_SIZE];
void bm_reset_stats(void)
{
    for (int i = 0; i < EVENT_LIST_SIZE; ++i) {
        for (int j = 0; j < RDT_LIST_SIZE; ++j) {
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
            bm_print_event_stat(RE_TX_NOTI_CS,   "D: TX REG NOTI CS");
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

