/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <net_queue_manager/net_queue_manager.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/debug.h>
#include <trace/trace.h>

#include <if/e10k_defs.h>
#include <dev/e10k_dev.h>
#include <dev/e10k_q_dev.h>

#include "helper.h"
#include "e10k_queue.h"



/******************************************************************************/
/* Compile time parameters */

/* Size of the TX and RX rings */
#define NTXDESCS 512
#define NRXDESCS 2048

/* Size of RX buffers */
#define RXBUFSZ 2048


// Enable Debugging for packet transfers
#define DEBUG_ENABLE 0

// Enable Debugging for intialization code
#define INITDEBUG_ENABLE 1





/******************************************************************************/
/* Prototypes */

static void idc_register_queue_memory(uint8_t queue,
                                      struct capref tx_frame,
                                      struct capref rx_frame,
                                      uint32_t rxbufsz);






/******************************************************************************/
/* Global state */

/** Service name */
static char* service_name = "e10k";

/** Binding to the internal e10k management service */
static struct e10k_binding *binding = NULL;

/** Queue index for this manager instance */
static int qi = -1;

/** Mackerel handle for device */
static e10k_t *d = NULL;

/** Queue handle for queue management library */
static e10k_queue_t *q;

/** MAC address to be used */
static uint64_t mac_address = 0;

/** Indicates if the initialization is done */
static int initialized = 0;





/******************************************************************************/
/* Debugging code, etc. */

#if DEBUG_ENABLE
#  define DEBUG queue_debug
#else
#  define DEBUG(x...) do { } while (0)
#endif
#if INITDEBUG_ENABLE
#  define INITDEBUG queue_debug
#else
#  define INITDEBUG(x...) do { } while (0)
#endif


#if DEBUG_ENABLE || INITDEBUG_ENABLE
static void queue_debug(const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    printf("e10k.q%d: ", qi);
    vprintf(fmt, va);
    va_end(va);
}
#endif

/* Helper code for debugging that dumps the statistics registers that are != 0.
 * Most registers are cleared uppon a read. */
#if 0
#define prnonz(x) uint32_t x = e10k_##x##_rd(d); if (x) printf(#x "=%x ", x)
static void stats_dump(void)
{
    queue_debug("");
    prnonz(tpt);
    prnonz(gptc);
    prnonz(txdgpc);

    prnonz(tpr);
    prnonz(gprc);
    prnonz(rxdgpc);

    prnonz(crcerrs);
    prnonz(illerrc);
    prnonz(errbc);
    prnonz(rlec);
    prnonz(ruc);
    prnonz(rfc);
    prnonz(roc);
    prnonz(rjc);
    prnonz(rxnfgpc);
    prnonz(mngprc);
    prnonz(mngpdc);

    prnonz(mlfc);
    prnonz(mrfc);

    uint32_t qprc = e10k_qprc_rd(d, 0);
    if (qprc) printf("qprc.0=%x ", qprc);
    uint32_t qprdc = e10k_qprdc_rd(d, 0);
    if (qprdc) printf("qprdc.0=%x ", qprdc);

    printf("\n");
}

#endif





/******************************************************************************/
/* Transmit path */

static errval_t transmit_pbuf_list_fn(struct driver_buffer *buffers,
                                      size_t                count,
                                      void                 *opaque)
{
    size_t i;
    DEBUG("Add buffer callback %d:\n", count);

    // TODO: Make sure there is room in TX queue
    for (i = 0; i < count; i++) {
        e10k_queue_add_txbuf(q, buffers[i].pa, buffers[i].len, opaque,
            (i == count - 1));
    }

    e10k_queue_bump_txtail(q);

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXDRVADD, 0);
#endif // TRACE_ONLY_SUB_NNET

    return SYS_ERR_OK;
}


static uint64_t find_tx_free_slot_count_fn(void)
{
    return e10k_queue_free_txslots(q);
}

static bool handle_free_tx_slot_fn(void)
{
    void *op;
    int last;

    if (e10k_queue_get_txbuf(q, &op, &last) != 0) {
        return false;
    }

    DEBUG("handle_free_tx_slot_fn: Packet done\n");

    //stats_dump();

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXDRVSEE,
                /*(uint32_t) buf->data*/0);
#endif // TRACE_ONLY_SUB_NNET

    handle_tx_done(op);

    return true;
}





/******************************************************************************/
/* Receive path */

static void check_for_free_txbufs(void)
{
    if (!initialized) return;

    // TODO: This loop can cause very heavily bursty behaviour, if the packets
    // arrive faster than they can be processed.
    while (handle_free_tx_slot_fn()) { }
}

static errval_t register_rx_buffer_fn(uint64_t paddr, void *vaddr, void *opaque)
{

    DEBUG("register_rx_buffer_fn: called\n");
    e10k_queue_add_rxbuf(q, paddr, opaque);
    e10k_queue_bump_rxtail(q);

    DEBUG("register_rx_buffer_fn: terminated\n");
    return SYS_ERR_OK;
}

static uint64_t find_rx_free_slot_count_fn(void)
{
    return e10k_queue_free_rxslots(q);
}

static void check_for_new_packets(void)
{
    size_t len;
    void *op;
    int last;
    size_t count;

    if (!initialized) return;

    //stats_dump();

    // TODO: This loop can cause very heavily bursty behaviour, if the packets
    // arrive faster than they can be processed.
    count = 0;
    while (e10k_queue_get_rxbuf(q, &op, &len, &last) == 0) {
#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXDRVSEE,
                    (uint32_t) len);
#endif // TRACE_ONLY_SUB_NNET

        DEBUG("New packet (q=%d)\n", qi);

        process_received_packet(op, len, !!last);
        count++;
    }

    if (count > 0) e10k_queue_bump_rxtail(q);
}




/******************************************************************************/
/* Misc */


/* Helper call-backs for queue manager */
static errval_t update_txtail(void *opaque, size_t tail)
{
    assert(d != NULL);

    e10k_tdt_wr(d, qi, tail);
    return SYS_ERR_OK;
}

static errval_t update_rxtail(void *opaque, size_t tail)
{
    assert(d != NULL);

    e10k_rdt_1_wr(d, qi, tail);
    return SYS_ERR_OK;
}

/**
 * Callback to pass MAC address to queue manager library.
 */
static void get_mac_addr_fn(uint8_t *mac)
{
    memcpy(mac, &mac_address, 6);
}




/******************************************************************************/
/* Device/queue initialization */

/** Allocate queue n and return handle for queue manager */
static void setup_queue(void)
{
    struct capref tx_frame;
    struct capref rx_frame;
    struct e10k_queue_ops ops = {
        .update_txtail = update_txtail,
        .update_rxtail = update_rxtail };

    size_t tx_size, rx_size;
    void *tx_virt, *rx_virt;



    INITDEBUG("setup_queue\n");

    // Allocate memory for descriptor rings
    tx_size = e10k_q_tdesc_legacy_size * NTXDESCS;
    tx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, tx_size,
        &tx_frame);
    assert(tx_virt != NULL);

    rx_size = e10k_q_rdesc_legacy_size * NRXDESCS;
    rx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, rx_size,
        &rx_frame);
    assert(rx_virt != NULL);


    // Initialize queue manager
    q = e10k_queue_init(tx_virt, NTXDESCS, rx_virt, NRXDESCS, &ops, NULL);

    // Register memory with device manager
    idc_register_queue_memory(qi, tx_frame, rx_frame, RXBUFSZ);
}

/** Terminate this queue driver */
static void terminate_queue_fn(void)
{
    assert(!"NYI");
}





/******************************************************************************/
/* Management interface implemetation */

/** Request device register cap from card driver */
static void idc_request_device_info(void)
{

    errval_t r;
    INITDEBUG("idc_request_device_info()\n");
    r = e10k_request_device_info__tx(binding, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Send memory caps to card driver */
static void idc_register_queue_memory(uint8_t queue,
                                      struct capref tx_frame,
                                      struct capref rx_frame,
                                      uint32_t rxbufsz)
{

    errval_t r;
    INITDEBUG("idc_register_queue_memory()\n");
    r = e10k_register_queue_memory__tx(binding, NOP_CONT, queue,
                                       tx_frame, rx_frame, rxbufsz);
    // TODO: handle busy
    assert(err_is_ok(r));
}



// Callback from device manager
static void idc_queue_init_data(struct e10k_binding *b, struct capref registers,
        uint64_t macaddr)
{
    struct frame_identity frameid = { .base = 0, .bits = 0 };
    errval_t err;
    void *virt;

    INITDEBUG("idc_queue_init_data\n");

    mac_address = macaddr;

    // Map device registers
    invoke_frame_identify(registers, &frameid);
    err = vspace_map_one_frame_attr(&virt, 1 << frameid.bits, registers,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    assert(err_is_ok(err));

    // Initialize mackerel device
    d = malloc(sizeof(*d));
    e10k_initialize(d, virt);

    // Initialize queue
    setup_queue();
}

// Callback from device manager
static void idc_queue_memory_registered(struct e10k_binding *b)
{
    initialized = 1;

    // Register queue with queue_mgr library
    ethersrv_init(service_name, qi, get_mac_addr_fn, terminate_queue_fn,
        transmit_pbuf_list_fn, find_tx_free_slot_count_fn,
        handle_free_tx_slot_fn, RXBUFSZ, register_rx_buffer_fn,
        find_rx_free_slot_count_fn);
}

// Callback from device manager
static void idc_write_queue_tails(struct e10k_binding *b)
{
    INITDEBUG("idc_write_queue_tails()\n");

    e10k_queue_bump_rxtail(q);
    e10k_queue_bump_txtail(q);
}


static struct e10k_rx_vtbl rx_vtbl = {
    .queue_init_data = idc_queue_init_data,
    .queue_memory_registered = idc_queue_memory_registered,
    .write_queue_tails = idc_write_queue_tails,
};

static void bind_cb(void *st, errval_t err, struct e10k_binding *b)
{
    assert(err_is_ok(err));

    INITDEBUG("Sucessfully connected to management interface\n");

    b->rx_vtbl = rx_vtbl;
    binding = b;

    idc_request_device_info();
}

/** Connect to the management interface */
static void connect_to_mngif(void)
{
    errval_t r;
    iref_t iref;
    const char *suffix = "_e10kmng";
    char name[strlen(service_name) + strlen(suffix) + 1];

    // Build label for interal management service
    sprintf(name, "%s%s", service_name, suffix);

    // Connect to service
    INITDEBUG("Looking up management interface (%s)\n", name);
    r = nameservice_blocking_lookup(name, &iref);
    assert(err_is_ok(r));

    INITDEBUG("Binding to management interface\n");
    r = e10k_bind(iref, bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}



static void parse_cmdline(int argc, char **argv)
{
    int i;
    bool has_queue = false;

    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "cardname=", strlen("cardname=") - 1) == 0) {
            service_name = argv[i] + strlen("cardname=");
        } else if (strncmp(argv[i], "queue=", strlen("queue=") - 1) == 0) {
            qi = atol(argv[i] + strlen("queue="));
            has_queue = true;
        } else {
            ethersrv_argument(argv[i]);
        }
    }

    if (!has_queue) {
        USER_PANIC("For queue driver the queue= parameter has to be specified "
                   "on the command line!");
    }
}


static void eventloop(void)
{
    struct waitset *ws;
    errval_t err;

    INITDEBUG("eventloop()\n");

    ws = get_default_waitset();
    while (1) {
        err = event_dispatch_non_block(ws);
        do_pending_work_for_all();
        check_for_new_packets();
        check_for_free_txbufs();
    }
}

int main(int argc, char **argv)
{
    DEBUG("Started\n");
    parse_cmdline(argc, argv);
    connect_to_mngif();
    eventloop();
}

