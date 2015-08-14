/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stddef.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/threads.h>
#include <barrelfish/waitset_chan.h>
#include <ipv4/lwip/inet.h>
#include <bulk_transfer/bulk_transfer.h>
#include <pci/pci.h>

#include <if/net_ports_defs.h>
#include <if/net_ports_rpcclient_defs.h>
#include <if/net_ARP_defs.h>
#include <if/net_ARP_rpcclient_defs.h>
#include <if/e10k_defs.h>

#include <dev/e10k_dev.h>
#include <dev/e10k_q_dev.h>

#include "bulk_net_backend.h"

#define E10K_MNG_SUF "_e10kmng"

#define ETHHDR_LEN 14
#define IPHDR_LEN 20


#if BULK_NET_ENABLE_DEBUG && BULK_NET_ENABLE_DEBUG_E10K
#define DEBUG(x...) debug_printf("e10k: " x)
#if BULK_NET_ENABLE_TRACE
#define BULK_NET_ 1
#else
#endif
#else
#define BULK_NET_ENABLE_E10K_TRACE
#define DEBUG(x...)
#endif

#define USE_INTERRUPTS 0
#define USE_WSPOLL 1

struct e10k_rx_event {
    struct bulk_e10k       *bu;
    struct bulk_net_msgdesc msg;
    struct event_queue_node eqn;
};

struct e10k_tx_event {
    struct bulk_e10k       *bu;
    void                   *op;
    struct event_queue_node eqn;
};


static struct net_ports_rpc_client net_ports_rpc;
static bool net_ports_connected = false;

static struct net_ARP_rpc_client net_arp_rpc;
static bool net_arp_connected = false;

static errval_t update_rxtail(void *opaque, size_t tail);
static errval_t update_txtail(void *opaque, size_t tail);
#if USE_INTERRUPTS
static void interrupt_handler(void *arg);
#endif
#if USE_WSPOLL
void bulk_e10k_poll(struct waitset_chanstate *chan);
#endif

/* Declarations for e10k flounder interface */
static void idc_request_device_info(struct bulk_e10k *bu);
static void idc_register_queue_memory(struct bulk_e10k *bu);
static void idc_queue_init_data(struct e10k_binding *b, struct capref registers,
        uint64_t macaddr);
static void idc_queue_memory_registered(struct e10k_binding *b);
static void idc_write_queue_tails(struct e10k_binding *b);

static struct e10k_rx_vtbl rx_vtbl = {
    .queue_init_data = idc_queue_init_data,
    .queue_memory_registered = idc_queue_memory_registered,
    .write_queue_tails = idc_write_queue_tails,
};


/*****************************************************************************/
/* Port manager client */

/** Bind specific port to queue */
static errval_t port_bind(uint64_t b_rx, uint64_t b_tx, uint64_t q,
        uint16_t port)
{
    errval_t err, msgerr;

    err = net_ports_rpc.vtbl.bind_port(&net_ports_rpc, net_ports_PORT_UDP, port,
            b_rx, b_tx, 0, q, &msgerr);
    if (err_is_fail(err)) {
        return err;
    }

    return msgerr;
}

/** Get any free port and bind it to the queue */
static errval_t port_get(uint64_t b_rx, uint64_t b_tx, uint64_t q,
        uint16_t *port)
{
    errval_t err, msgerr;

    err = net_ports_rpc.vtbl.get_port(&net_ports_rpc, net_ports_PORT_UDP,
            b_rx, b_tx, 0, q, &msgerr, port);
    if (err_is_fail(err)) {
        return err;
    }

    return msgerr;
}

static void p_bind_cb(void *st, errval_t err, struct net_ports_binding *b)
{
    assert(err_is_ok(err));
    err = net_ports_rpc_client_init(&net_ports_rpc, b);
    assert(err_is_ok(err));
    net_ports_connected = true;
}

/** Bind to ports service (currently blocking) */
static void bind_ports(struct waitset *ws)
{
    errval_t err;
    iref_t iref;

    DEBUG("bind_ports()\n");
    err = nameservice_blocking_lookup("e10k_PORTS_MNG", &iref);
    assert(err_is_ok(err));
    DEBUG("resolved\n");

    err = net_ports_bind(iref, p_bind_cb, NULL, ws, IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));
    DEBUG("binding initiated\n");

    while (!net_ports_connected) {
        event_dispatch_non_block(ws);
        event_dispatch_non_block(get_default_waitset());
    }
    DEBUG("bound_ports\n");
}


/*****************************************************************************/
/* ARP service client */

/** Get information about the local TCP/IP configuration*/
static errval_t arp_ip_info(uint32_t *ip, uint32_t *gw, uint32_t *mask)
{
    errval_t err, msgerr;

    err = net_arp_rpc.vtbl.ip_info(&net_arp_rpc, 0, &msgerr, ip, gw, mask);
    if (err_is_fail(err)) {
        return err;
    }
    return msgerr;
}

/** Do an ARP lookup of an ip address */
static errval_t arp_lookup(uint32_t ip, uint64_t *mac)
{
    errval_t err, msgerr;

    err = net_arp_rpc.vtbl.ARP_lookup(&net_arp_rpc, ip, 0, true, &msgerr, mac);
    if (err_is_fail(err)) {
        return err;
    }
    return msgerr;
}

static void a_bind_cb(void *st, errval_t err, struct net_ARP_binding *b)
{
    assert(err_is_ok(err));
    err = net_ARP_rpc_client_init(&net_arp_rpc, b);
    assert(err_is_ok(err));
    net_arp_connected = true;
}

/** Bind to ARP service (currently blocking) */
static void bind_arp(struct waitset *ws)
{
    errval_t err;
    iref_t iref;

    DEBUG("bind_arp()\n");
    err = nameservice_blocking_lookup("e10k_ARP", &iref);
    assert(err_is_ok(err));
    DEBUG("resolved\n");

    err = net_ARP_bind(iref, a_bind_cb, NULL, ws, IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));
    DEBUG("binding initiated\n");

    while (!net_arp_connected) {
        event_dispatch_non_block(ws);
        event_dispatch_non_block(get_default_waitset());
    }
    DEBUG("bound_arp\n");
}


/******************************************************************************/
/* e10k card driver interface */

/** e10k interface: callback for a successful binding */
static void bind_cb(void *st, errval_t err, struct e10k_binding *b)
{
    DEBUG("bind_cb()\n");
    struct bulk_e10k *bu = st;

    assert(err_is_ok(err));

    b->rx_vtbl = rx_vtbl;
    b->st = bu;
    bu->binding = b;

    idc_request_device_info(bu);
}


/** e10k interface: Send request for device information */
static void idc_request_device_info(struct bulk_e10k *bu)
{
    errval_t err;

    DEBUG("idc_request_device_info()\n");

    err = e10k_request_device_info__tx(bu->binding, NOP_CONT);
    assert(err_is_ok(err));

}

/** e10k interface: Register memory for descriptor rings */
static void idc_register_queue_memory(struct bulk_e10k *bu)
{
    errval_t r = SYS_ERR_OK;
    DEBUG("idc_register_queue_memory()\n");

    /* r = e10k_register_queue_memory__tx(bu->binding, NOP_CONT, bu->qi, */
    /*     bu->txframe, bu->txhwbframe, bu->rxframe, bu->buffer_size, E10K_HDRSZ, */
    /*     bu->int_vector, bu->int_core, USE_INTERRUPTS, false); */
    assert(err_is_ok(r));
}

/** e10k interface: Callback for request device info */
static void idc_queue_init_data(struct e10k_binding *b, struct capref registers,
        uint64_t macaddr)
{
    DEBUG("idc_queue_init_data()\n");

    errval_t err;
    struct bulk_e10k *bu = b->st;
    struct frame_identity fid = { .base = 0, .bits = 0 };
    void *virt, *rx, *tx, *txhwb;
    uint8_t core;
    struct e10k_queue_ops ops = {
        .update_txtail = update_txtail,
        .update_rxtail = update_rxtail
    };

    bu->mac = macaddr;

    // Map registers
    invoke_frame_identify(registers, &fid);
    err = vspace_map_one_frame_attr(&virt, 1 << fid.bits, registers,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    assert(err_is_ok(err));

    // Initialize mackerel device (must only be used for queue index register)
    e10k_initialize(&bu->d, virt);

    // Allocate and initialize memory for queues
    err = allocmap_frame(bu->ring_size * E10K_DESCSZ, &rx, NULL, &bu->rxframe);
    assert(err_is_ok(err));
    err = allocmap_frame(bu->ring_size * E10K_DESCSZ, &tx, NULL, &bu->txframe);
    assert(err_is_ok(err));
    err = allocmap_frame(0x1000, &txhwb, NULL, &bu->txhwbframe);
    assert(err_is_ok(err));

    bu->q = e10k_queue_init(tx, bu->ring_size, txhwb, rx, bu->ring_size,
                            &ops, bu);

    // Setup interrupt
#if USE_INTERRUPTS
    err = pci_setup_inthandler(interrupt_handler, bu, &bu->int_vector);
    assert(err_is_ok(err));
    bu->int_core = disp_get_core_id();

#endif

    DEBUG("idc_queue_init_data: done\n");

    // Register ring memory with driver
    core = disp_get_core_id();
    idc_register_queue_memory(bu);
}

/** e10k interface: Callback for register queue memory */
static void idc_queue_memory_registered(struct e10k_binding *b)
{
    struct bulk_e10k *bu = b->st;
    DEBUG("idc_queue_memory_registered()\n");

    bu->ready = true;
}

/**
 * e10k interface: Callback for writing out queue tails (needed in case of card
 * hangs)
 */
static void idc_write_queue_tails(struct e10k_binding *b)
{
    struct bulk_e10k *bu = b->st;
    DEBUG("idc_write_queue_tails()\n");
    e10k_queue_bump_rxtail(bu->q);
    e10k_queue_bump_txtail(bu->q);
}


/*****************************************************************************/
/* e10k queue management */

static void recv_event(void *arg)
{
    DEBUG("recv_event\n");
    struct e10k_rx_event *rxe = arg;
    rxe->bu->received(rxe->bu, &rxe->msg);
    stack_alloc_free(&rxe->bu->rx_event_alloc, rxe);
}

/** Try to process one packet in the receive queue */
static bool recv_one(struct bulk_e10k *bu)
{
    void *op;
    size_t len, hdrlen, i;
    int last = 0, res;
    uint64_t flags = 0;
    struct e10k_rx_event *rxe = NULL; // Fix compile bug -- jb


    i = 0;
    do {
        res = e10k_queue_get_rxbuf(bu->q, &op, &hdrlen, &len, &last, &flags);
        if (res == 0) {
            if (i == 0) {
                rxe = stack_alloc_alloc(&bu->rx_event_alloc);
                 assert(rxe != NULL); // should never happen
            }
            DEBUG("    Received part[%"PRId64"] of packet op=%p hl=%"PRIx64" l=%"
                    PRIx64" f=%"PRIx64"\n", i, op, hdrlen, len, flags);
        }
        if (i == 0 && res != 0) {
            return false;
        } else if (res != 0) {
            continue;
        } else if ((i + !!hdrlen) >= BULK_NET_DESCLEN) {
            USER_PANIC("Buffer chain longer than supported");
        }

        if (hdrlen > 0) {
            rxe->msg.parts[i].size = hdrlen;
            rxe->msg.parts[i].opaque = op;
            i++;
        }

        rxe->msg.parts[i].size = len;
        rxe->msg.parts[i].opaque = op;

        i++;
    } while (last != 1);

    if (i < BULK_NET_DESCLEN) {
        memset(&rxe->msg.parts[i], 0, sizeof(rxe->msg.parts[i]));
    }

#if !USE_INTERRUPTS && USE_WSPOLL
    recv_event(rxe);
#else
    event_queue_add(&bu->event_queue, &rxe->eqn,
        MKCLOSURE(recv_event, rxe));
#endif

    return true;
}

static void tx_event(void *arg)
{
    DEBUG("tx_event\n");
    struct e10k_tx_event *txe = arg;
    txe->bu->transmitted(txe->bu, txe->op);
    stack_alloc_free(&txe->bu->tx_event_alloc, txe);

}

/** Check thee tx queues for transmits that have finshed */
static bool check_tx(struct bulk_e10k *bu)
{
    void *op = NULL;
    bool had = false;
    struct e10k_tx_event *txe;

#if 0
    if (e10k_tdt_rd(&bu->d, bu->qi) != e10k_tdh_rd(&bu->d, bu->qi)) {
        DEBUG("Nonempty: %"PRIx32" %"PRIx32"\n", e10k_tdt_rd(&bu->d,
                    bu->qi), e10k_tdh_rd(&bu->d, bu->qi));
    }
#endif
    if (e10k_queue_get_txbuf(bu->q, &op) == 0) {
        DEBUG("e10k packet sent\n");
        txe = stack_alloc_alloc(&bu->tx_event_alloc);
        assert(txe != NULL); // should never happen
        txe->op = op;
#if !USE_INTERRUPTS && USE_WSPOLL
        tx_event(txe);
#else
        event_queue_add(&bu->event_queue, &txe->eqn,
            MKCLOSURE(tx_event, txe));
#endif
        had = true;
    }
    return had;
}

#if USE_INTERRUPTS
/** Interrupt handler for RX and TX events */
static void interrupt_handler(void *arg)
{
    struct bulk_e10k *bu = arg;
    DEBUG("Interrupt!\n");
    while (recv_one(bu));
    while (check_tx(bu));
}
#else
#if USE_WSPOLL

static inline struct bulk_e10k *wscs_to_e10k(struct waitset_chanstate *chan)
{
    return (struct bulk_e10k *)
       ((uintptr_t) chan - offsetof(struct bulk_e10k, wscs));
}

static void ws_event(void *arg)
{
    struct bulk_e10k *bu = arg;
    bool found, cur;
    do {
        found = false;
        do {
            cur = recv_one(bu);
            found = found || cur;
        } while (cur);
        do {
            cur = check_tx(bu);
            found = found || cur;
        } while (cur);
    } while (found);

    waitset_chan_register_polled(bu->waitset, &bu->wscs,
                    MKCLOSURE(ws_event, bu));

}

void bulk_e10k_poll(struct waitset_chanstate *chan)
{
    struct bulk_e10k *bu = wscs_to_e10k(chan);
    // Check TX queue first, since it is cheaper
    if (e10k_queue_get_txpoll(bu->q) != 0 &&
        e10k_queue_rxpoll(bu->q) != 0)
    {
        return;
    }

    waitset_chan_trigger(chan);
}

#else

/** Thread polling rx and tx queues */
static int recv_thread(void *arg)
{
    struct bulk_e10k *bu = arg;
    DEBUG("Start receiving thread...\n");
    bool found;
    while (1) {
        found = check_tx(bu);
        found = recv_one(bu) || found;
        if (!found) {
            thread_yield();
        }
    }
    return 0;
}
#endif
#endif


/** Callback for queue manager (writes tx tail index) */
static errval_t update_txtail(void *opaque, size_t tail)
{
    struct bulk_e10k *bu = opaque;
    e10k_tdt_wr(&bu->d, bu->qi, tail);
    return SYS_ERR_OK;
}

/** Callback for queue manager (writes rx tail index) */
static errval_t update_rxtail(void *opaque, size_t tail)
{
    struct bulk_e10k *bu = opaque;
    e10k_rdt_1_wr(&bu->d, bu->qi, tail);
    return SYS_ERR_OK;
}


/*****************************************************************************/
/* Public interface */

/**
 * Initialize directly mapped RX/TX queue pair with e10k NIC.
 *
 * @param bu          Channel struct
 * @param ws          Waitset
 * @param card        Card name
 * @param queue       Queue ID to use
 * @param buffer_size Size of receive buffers in bytes
 * @param ring_size   Number of descriptors in the RX/TX rings
 * @param received    Callback for a received packet
 * @param transmitted Callback for a transmitted packet
 */
errval_t bulk_e10k_init(struct bulk_e10k *bu,
                        struct waitset *ws,
                        const char *card,
                        uint8_t queue,
                        size_t buffer_size,
                        size_t ring_size,
                        void (*received)(struct bulk_e10k *,
                                         struct bulk_net_msgdesc *),
                        void (*transmitted)(struct bulk_e10k *, void *))
{
    errval_t err;
    char name[strlen(card) + strlen(E10K_MNG_SUF) + 1];
    iref_t iref;
    struct e10k_rx_event *rxe;
    struct e10k_tx_event *txe;
    size_t i;


    bu->qi = queue;
    bu->ready = false;
    bu->received = received;
    bu->transmitted = transmitted;
    bu->buffer_size = buffer_size;
    bu->ring_size = ring_size;
    bu->waitset = ws;

    // Allocate events
    stack_alloc_init(&bu->rx_event_alloc, ring_size);
    stack_alloc_init(&bu->tx_event_alloc, ring_size);
    rxe = calloc(ring_size, sizeof(*rxe));
    txe = calloc(ring_size, sizeof(*txe));
    for (i = 0; i < ring_size; i++) {
        rxe[i].bu = bu;
        txe[i].bu = bu;
        stack_alloc_free(&bu->rx_event_alloc, rxe + i);
        stack_alloc_free(&bu->tx_event_alloc, txe + i);
    }

    // Connect to port management service
    bind_ports(ws);
    bind_arp(ws);

    // Bind to e10k card driver
    strcpy(name, card);
    strcat(name, E10K_MNG_SUF);
    err = nameservice_blocking_lookup(name, &iref);
    assert(err_is_ok(err));

    DEBUG("Start binding\n");
    err = e10k_bind(iref, bind_cb, bu, ws, IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    while (!bu->ready) {
        event_dispatch_non_block(ws);
        event_dispatch_non_block(get_default_waitset());
    }

#if USE_INTERRUPTS || !USE_WSPOLL
    event_queue_init(&bu->event_queue, ws, EVENT_QUEUE_CONTINUOUS);
#endif
#if !USE_INTERRUPTS
#if USE_WSPOLL
    waitset_chanstate_init(&bu->wscs, CHANTYPE_BULK_E10K);
    waitset_chan_register_polled(ws, &bu->wscs,
            MKCLOSURE(ws_event, bu));
#else
    thread_create(recv_thread, bu);
#endif
#endif

    return SYS_ERR_OK;
}

/**
 * Add a buffer to the receive queue.
 *
 * @param bu     Channel struct
 * @param phys   Physical address of buffer
 * @param header Physical address of header buffer (needs E10K_HDRSZ bytes)
 * @param opaque User-Data for this buffer, will be returned when it is used in
 *               a received packet.
 */
errval_t bulk_e10k_rx_add(struct bulk_e10k *bu, uint64_t phys, uint64_t header,
                          void *opaque)
{
    DEBUG("bulk_e10k_rx_add(transfer=%p, phy=%"PRIx64",header=%"PRIx64",opaque=%p)\n",
            bu, phys, header, opaque);
    int r = e10k_queue_add_rxbuf(bu->q, phys, header, opaque);
    assert(r == 0);
    e10k_queue_bump_rxtail(bu->q);
    return SYS_ERR_OK;
}

/**
 * Send out a packet.
 *
 * @param bu   Channel struct
 * @param decs Descriptor for buffer chain to transmit
 */
errval_t bulk_e10k_send(struct bulk_e10k *bu, struct bulk_net_msgdesc *desc)
{
    size_t totallen = 0;
    size_t cnt = 0;
    size_t i;
    for (i = 0; i < BULK_NET_DESCLEN; i++) {
        if (desc->parts[i].size == 0) {
            break;
        }
        cnt++;
        totallen += desc->parts[i].size;
    }
    DEBUG("bulk_e10k_send(len=%"PRIx64")\n", totallen);

    e10k_queue_add_txcontext(bu->q, 0, ETHHDR_LEN, IPHDR_LEN, 0, 0);
    e10k_queue_add_txbuf_ctx(bu->q, desc->parts[0].phys,
        desc->parts[0].size, desc->parts[0].opaque, 1, cnt == 1,
        totallen, 0, true, false);

    for (i = 1; i < cnt; i++) {
        e10k_queue_add_txbuf(bu->q, desc->parts[i].phys,
                desc->parts[i].size, desc->parts[i].opaque, 0, i == cnt - 1,
                totallen);
    }
    e10k_queue_bump_txtail(bu->q);
    DEBUG("bulk_e10k_send_done\n");
    return SYS_ERR_OK;
}

/**
 * Steer a specific UDP port to this queue.
 *
 * @param bu   Channel struct
 * @param port Port to allocate (in host byte order)
 */
errval_t bulk_e10k_port_add(struct bulk_e10k *bu, uint16_t port)
{
    errval_t err;

    // Register port
    err = port_bind(0, 0, bu->qi, port);
    assert(err_is_ok(err));
    DEBUG("Port registered\n");

    return SYS_ERR_OK;
}

/**
 * Allocate an unused UDP port and steer it to this queue.
 *
 * @param bu   Channel struct
 * @param port Pointer to variable where port number will be stored (host byte
 *             order)
 */
errval_t bulk_e10k_port_alloc(struct bulk_e10k *bu, uint16_t *port)
{
    return port_get(0, 0, bu->qi, port);
}

/**
 * Get IP address configured for this interface.
 *
 * @param bu Channel struct
 * @param ip Pointer to variable where IP will be stored (host byte order)
 */
errval_t bulk_e10k_ip_info(struct bulk_e10k *bu, uint32_t *ip)
{
    errval_t err;
    uint32_t gw, mask;
    err = arp_ip_info(ip, &gw, &mask);
    *ip = ntohl(*ip);
    return err;
}

/**
 * Do an ARP lookup on this interface
 *
 * @param bu  Channnel struct
 * @param ip  IP address to resolve (in host byte order)
 * @param mac Pointer to variable where MAC address will be stored
 */
errval_t bulk_e10k_arp_lookup(struct bulk_e10k *bu, uint32_t ip, uint64_t *mac)
{
    return arp_lookup(htonl(ip), mac);
}

