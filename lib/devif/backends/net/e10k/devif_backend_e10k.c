/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <devif/queue_interface.h>
#include <dev/e10k_q_dev.h>
#include <dev/e10k_dev.h>
#include <dev/e10k_vf_dev.h>
#include <skb/skb.h>

#include <if/e10k_vf_defs.h>
#include <if/e10k_vf_rpcclient_defs.h>

#include "e10k_devif_vf.h"
#include "helper.h"
#include "e10k_queue.h"
#include "debug.h"

#define NUM_TX_DESC 2048
#define NUM_RX_DESC 2048

#define ETHHDR_LEN 14
#define IPHDR_LEN 20
#define UDPHDR_LEN 8

// for debugging
static e10k_t* d;

// TODO only required for legacy interrupts
struct e10k_queue* queues[128];

/******************************************************************************/
/* Misc functions */


static inline bool buf_use_tcpxsm(uint64_t flags)
{
    return (flags & NETIF_TXFLAG_TCPCHECKSUM);
}

static inline bool buf_use_udpxsm(uint64_t flags)
{
    return (flags & NETIF_TXFLAG_UDPCHECKSUM);
}

static inline bool buf_use_ipxsm(uint64_t flags)
{
    return (flags & NETIF_TXFLAG_IPCHECKSUM) ||
        buf_use_tcpxsm(flags) || buf_use_udpxsm(flags);
}

static inline uint8_t buf_tcphdrlen(uint64_t flags)
{
    return ((flags & NETIF_TXFLAG_TCPHDRLEN_MASK) >>
        NETIF_TXFLAG_TCPHDRLEN_SHIFT) * 4;
}

static errval_t update_txtail(struct e10k_queue* q, size_t tail)
{
    assert(q->d != NULL);

    if (q->use_vf) {
        e10k_vf_vftdt_wr(q->d, q->id, tail);
    } else {
        e10k_tdt_wr(q->d, q->id, tail);
    }
    return SYS_ERR_OK;
}

static errval_t update_rxtail(struct e10k_queue* q, size_t tail)
{
    assert(q->d != NULL);

    if (q->use_vf) {
        e10k_vf_vfrdt_wr(q->d, q->id, tail);
    } else {
        e10k_rdt_1_wr(q->d, q->id, tail);
    }
    return SYS_ERR_OK;
}


static struct region_entry* get_region(struct e10k_queue* q, regionid_t rid)
{
    struct region_entry* entry = q->regions;
    while (entry != NULL) {
        if (entry->rid == rid) {
            return entry;
        }
        entry = entry->next;
    }
    return NULL;
}

static errval_t enqueue_tx_buf(struct e10k_queue* q, regionid_t rid,
                               genoffset_t offset,
                               genoffset_t length,
                               genoffset_t valid_data,
                               genoffset_t valid_length,
                               uint64_t flags)
{
    DEBUG_QUEUE("Enqueueing TX buf \n");

    if (e10k_queue_free_txslots(q) == 0) {
        DEBUG_QUEUE("e10k_%d: Not enough space in TX ring, not adding buffer\n",
                q->id);
        // TODO better error
        return NIC_ERR_ENQUEUE;
    }

    // Prepare checksum offload
    //
    struct region_entry* entry = get_region(q, rid);
    assert(entry != NULL);

    e10k_q_l4_type_t l4t = 0;
    uint8_t l4len = 0;

    if (buf_use_ipxsm(flags)) {

        if (buf_use_tcpxsm(flags)) {
            l4t = e10k_q_tcp;
            l4len = buf_tcphdrlen(flags);
        } else if (buf_use_udpxsm(flags)) {
            l4t = e10k_q_udp;
            l4len = UDPHDR_LEN;
        }

        e10k_queue_add_txcontext(q, 0, ETHHDR_LEN, IPHDR_LEN, l4len, l4t);

	    if (q->use_vtd) {
            lpaddr_t addr = 0;
            addr = (lpaddr_t) entry->virt + offset + valid_data;
            e10k_queue_add_txbuf_ctx(q, addr, rid, offset, length,
                                     valid_data, valid_length, flags,
                                     valid_length, 0, true, l4len !=0);
	    } else {
            lpaddr_t addr = 0;
            addr = (lpaddr_t) entry->phys + offset + valid_data;
            e10k_queue_add_txbuf_ctx(q, addr, rid, offset, length,
                                     valid_data, valid_length, flags,
                                     valid_length, 0, true, l4len != 0);
        }
    } else {
        if (q->use_vtd) {
            lvaddr_t addr = 0;
            addr = (lvaddr_t) entry->virt + offset + valid_data;
            e10k_queue_add_txbuf(q, addr, rid, offset, length, valid_data,
                                 valid_length, flags,
                                 valid_length);
        } else {
            lpaddr_t addr;
            addr = (lpaddr_t) entry->phys + offset + valid_data;

            e10k_queue_add_txbuf(q, addr, rid, offset, length, valid_data,
                                 valid_length, flags,
                                 valid_length);
        }
    }
    e10k_queue_bump_txtail(q);
    return SYS_ERR_OK;
}


static errval_t enqueue_rx_buf(struct e10k_queue* q, regionid_t rid,
                               genoffset_t offset,
                               genoffset_t length,
                               genoffset_t valid_data,
                               genoffset_t valid_length,
                               uint64_t flags)
{
    //DEBUG_QUEUE("Enqueueing RX buf \n");
    // check if there is space
    if (e10k_queue_free_rxslots(q) == 0) {
        DEBUG_QUEUE("e10k_%d: Not enough space in RX ring, not adding buffer\n",
                q->id);
        // TODO better error
        return NIC_ERR_ENQUEUE;
    }

    if (q->use_vtd) {
        // get virtual address of buffer
        struct region_entry* entry = get_region(q, rid);
        assert(entry != NULL);

        lpaddr_t addr = 0;
        addr = (lpaddr_t) entry->virt + offset;
        e10k_queue_add_rxbuf(q, addr, rid, offset, length, valid_data,
                             valid_length, flags);
    } else {
        // get virtual address of buffer
        struct region_entry* entry = get_region(q, rid);
        assert(entry != NULL);

        lpaddr_t addr = 0;
        addr = (lpaddr_t) entry->phys + offset;
        e10k_queue_add_rxbuf(q, addr, rid, offset, length, valid_data,
                             valid_length, flags);
    }

    e10k_queue_bump_rxtail(q);
    return SYS_ERR_OK;
}

/******************************************************************************/
/* Queue functions */

static errval_t e10k_enqueue(struct devq* q, regionid_t rid, genoffset_t offset,
                             genoffset_t length, genoffset_t valid_data,
                             genoffset_t valid_length, uint64_t flags)
{
    errval_t err;


    struct e10k_queue* queue = (struct e10k_queue*) q;
    if (flags & NETIF_RXFLAG) {
        /* can not enqueue receive buffer larger than 2048 bytes */
        assert(length <= 2048);

        err = enqueue_rx_buf(queue, rid, offset, length, valid_data,
                             valid_length, flags);
        if (err_is_fail(err)) {
            return err;
        }
    } else if (flags & NETIF_TXFLAG) {

        assert(length <= 2048);
        
        DEBUG_QUEUE("Enqueuing offset=%lu valid_data=%lu txhwb=%d tx_tail=%zu tx_head=%zu \n", 
               offset, valid_data, (queue->tx_hwb == NULL) ? 0 : *((uint32_t*)queue->tx_hwb), queue->tx_tail,
               queue->tx_head);

        err = enqueue_tx_buf(queue, rid, offset, length, valid_data,
                             valid_length, flags);
        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}


static errval_t e10k_dequeue(struct devq* q, regionid_t* rid,
                             genoffset_t* offset, genoffset_t* length,
                             genoffset_t* valid_data,
                             genoffset_t* valid_length, uint64_t* flags)
{
    struct e10k_queue* que = (struct e10k_queue*) q;
    int last;
    errval_t err = SYS_ERR_OK;

    if (!e10k_queue_get_txbuf(que, rid, offset, length, valid_data,
                              valid_length, flags)) {
        err = DEVQ_ERR_QUEUE_EMPTY;
    }  else {
        DEBUG_QUEUE("Queue %d sent offset=%lu valid_length=%lu transmit count %d\n", 
               que->id, *offset, *valid_length, e10k_vf_vfgptc_rd(que->d));
        return SYS_ERR_OK;
    }

    if (!e10k_queue_get_rxbuf(que, rid, offset, length, valid_data,
                             valid_length, flags, &last)) {
        err = DEVQ_ERR_QUEUE_EMPTY;
    } else {
        DEBUG_QUEUE("Queue %d received offset=%lu valid_length=%lu \n", 
               que->id, *offset, *valid_length);
        return SYS_ERR_OK;
    }
     

    return err;
}

static errval_t e10k_register(struct devq* q, struct capref cap, regionid_t rid)
{
    errval_t err;
    struct e10k_queue* queue = (struct e10k_queue*) q;

    struct frame_identity id;
    err = invoke_frame_identify(cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    void* va;
    err = vspace_map_one_frame_attr(&va, id.bytes, cap,
                                    VREGION_FLAGS_READ_WRITE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    // keep track of regions since we need the virtual address ...
    struct region_entry* entry = malloc(sizeof(struct region_entry));
    entry->rid = rid;
    entry->cap = cap;
    entry->phys = id.base;
    entry->virt = (lvaddr_t)va;
    entry->size = id.bytes;
    entry->next = NULL;

    // linked list of regions
    struct region_entry* cur = queue->regions;
    if (cur == NULL) {
        queue->regions = entry;
        return SYS_ERR_OK;
    }

    while (cur->next != NULL) {
        cur = cur->next;
    }
    
    cur->next = entry;

    return SYS_ERR_OK;
}

static errval_t e10k_deregister(struct devq* q, regionid_t rid)
{
    return SYS_ERR_OK;
}

static errval_t e10k_control(struct devq* q, uint64_t cmd, uint64_t value, uint64_t *result)
{
    struct e10k_queue* queue = (struct e10k_queue*) q;
    *result = queue->mac;
    return SYS_ERR_OK;
}


static errval_t e10k_notify(struct devq* q)
{
    return SYS_ERR_OK;
}

static errval_t e10k_destroy(struct devq* queue)
{
    struct e10k_queue* q = (struct e10k_queue*) queue;
    free(q);
    //TODO: rest of the cleanup
    return SYS_ERR_OK;
}

/******************************************************************
 * Management functions
 *
 */

static void interrupt_cb(struct e10k_vf_binding *b, uint16_t qid)
{
    struct e10k_queue* q = queues[qid];

    if (q != b->st) {
        debug_printf("STATE MISMATCH!\n %p %p\n", q, b->st);
        q = b->st;
    }

    q->cb(q);
}

static struct e10k_vf_rx_vtbl rx_vtbl = {
    .interrupt = interrupt_cb,
};

static void bind_cb(void *st, errval_t err, struct e10k_vf_binding *b)
{
    struct e10k_queue* q = (struct e10k_queue*) st;
    assert(err_is_ok(err));

    DEBUG_QUEUE("Sucessfully connected to management interface\n");

    b->st = q;
    q->binding = b;
    b->rx_vtbl = rx_vtbl;
    e10k_vf_rpc_client_init(q->binding);
    q->bound = true;
}

/** Connect to the management interface */
static void connect_to_mngif(struct e10k_queue* q)
{
    errval_t r;
    iref_t iref;

    q->bound = false;
    char name[strlen("e10k_vf") + 2];

    // Build label for interal management service
    sprintf(name, "%s%u", "e10k_vf", q->pci_function);

    // Connect to service
    DEBUG_QUEUE("Looking up management interface (%s)\n", name);
    r = nameservice_blocking_lookup(name, &iref);
    assert(err_is_ok(r));

    DEBUG_QUEUE("Binding to management interface\n");
    r = e10k_vf_bind(iref, bind_cb, q, get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));

    while (!q->bound) {
        event_dispatch(get_default_waitset());
    }
}

/*********************************************************
 * Queue creation 
 */

static errval_t map_device_memory(struct e10k_queue* q,
                                  struct capref regs)
{

    struct frame_identity id = {.base = 0, .bytes = 0};
    errval_t err;

    err = invoke_frame_identify(regs, &id);
    if (err_is_fail(err)) {
        return err;
    }

    void* va;
    err = vspace_map_one_frame_attr(&va, id.bytes, regs,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }
      

    DEBUG_QUEUE("mapped %zu bytes at address %p\n", id.bytes, va);
    q->d = malloc(sizeof(e10k_t));
    assert(q->d != NULL);
    e10k_initialize(q->d, va);
    d = q->d;
    return SYS_ERR_OK;
}

// TODO mostly cleanup when fail
errval_t e10k_queue_create(struct e10k_queue** queue, e10k_event_cb_t cb,
                           uint32_t bus, uint32_t function, uint32_t devid, uint32_t dev,
                           bool use_vf, bool interrupts, bool qzero)
{

    errval_t err;
    struct e10k_queue* q;
    // start VF driver
    
    q = malloc(sizeof(struct e10k_queue));
    q->pci_function = 0; // TODO allow also function 1

    // txhwb
    if (use_vf) {
        q->use_txhwb = false;
    } else {
        q->use_txhwb = true;
    }
    q->cb = cb;
    
    if (use_vf) {
        //USER_PANIC("NOT YET WORKING \n");
        // Start VF
        if (!e10k_vf_started()) {
            err = e10k_init_vf_driver(0, 0, bus+1, dev+16, devid, interrupts);
            if (err_is_fail(err)) {
                return err;
            }
        }

        // If i can not create any more queues -> start new VF
        if (!e10k_vf_can_create_queue()) {
            err = e10k_init_vf_driver(0, 0, bus+1, dev+16, devid, interrupts);
            if (err_is_fail(err)) {
                return err;
            }
        }

        err = skb_client_connect();
        assert(err_is_ok(err));
        err = skb_execute_query("vtd_enabled(0,_).");
        if (err_is_fail(err)) {
            DEBUG_QUEUE("Assume disabled VT-d \n");
            q->use_vtd = false;
        } else {
            DEBUG_QUEUE("Assume enabled VT-d \n");
            q->use_vtd = true;
        }

    } else {
        q->use_vtd = false;
        // need to set up communicaton to PF
        connect_to_mngif(q);
    }

    // allocate memory for RX/TX rings
    struct capref tx_frame;
    size_t tx_size = e10k_q_tdesc_adv_wb_size*NUM_TX_DESC;
    void* tx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, tx_size, &tx_frame);
    if (tx_virt == NULL) {
        return DEVQ_ERR_INIT_QUEUE;
    }

    struct capref rx_frame;
    size_t rx_size = e10k_q_rdesc_adv_wb_size*NUM_RX_DESC;
    void* rx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, rx_size, &rx_frame);
    if (rx_virt == NULL) {
        return DEVQ_ERR_INIT_QUEUE;
    }

    struct e10k_queue_ops ops = {
        .update_txtail = update_txtail,
        .update_rxtail = update_rxtail,
    };

    struct capref txhwb_frame;
    void* txhwb_virt = NULL;

    if (q->use_txhwb) {
        txhwb_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, BASE_PAGE_SIZE,
        &txhwb_frame);
        if (txhwb_virt == NULL) {   
            return DEVQ_ERR_INIT_QUEUE;
        }
        memset(txhwb_virt, 0, sizeof(uint32_t));
    }

    e10k_queue_init(q, tx_virt, NUM_TX_DESC, txhwb_virt,
                    rx_virt, NUM_RX_DESC, &ops);

    DEBUG_QUEUE("Local queue init done\n");

    q->use_vf = use_vf;
    q->rx_frame = rx_frame;
    q->tx_frame = tx_frame;
    q->txhwb_frame = txhwb_frame;
    q->use_irq = interrupts;

    // XXX:disable by default for now
    q->use_rsc = false;

    if (q->use_vf) {
        err = e10k_vf_init_queue_hw(q);
        if (err_is_fail(err)) {
            return err;
        }
    } else {

        int qid;
        errval_t err2;
        struct capref regs;

        if (q->use_irq) {
            /*
            err = pci_setup_inthandler(interrupt_handler, NULL, &vector);
            assert(err_is_ok(err));
            core = disp_get_core_id();
            */
            // TODO setup MSI-X interrupts
        }

        // Inform card driver about new queue and get the registers/queue id
        err = slot_alloc(&regs);
        if (err_is_fail(err)) {
            return err;
        }

        err = q->binding->rpc_tx_vtbl.create_queue(q->binding, tx_frame, txhwb_frame,
                                            rx_frame, 2048, q->msix_intvec,
                                            q->msix_intdest, q->use_irq, false, qzero,
                                            &q->mac, &qid,
                                            &regs, &err2);
        if (err_is_fail(err) || err_is_fail(err2)) {
            DEBUG_QUEUE("e10k rpc error\n");
            return err_is_fail(err)? err: err2;
        }

        assert(qid >= 0);
        q->id = (uint16_t)qid;

        err = map_device_memory(q, regs);
        if (err_is_fail(err)) {
            DEBUG_QUEUE("e10k map device error\n");
            return err;
        }
            
        update_txtail(q, 0);
        update_rxtail(q, 0);
        
    }

    err = devq_init(&q->q, false);
    if (err_is_fail(err)) {
        DEBUG_QUEUE("e10k devq_init error\n");
        return err;
    }
    
    q->q.f.enq = e10k_enqueue;
    q->q.f.deq = e10k_dequeue;
    q->q.f.reg = e10k_register;
    q->q.f.dereg = e10k_deregister;
    q->q.f.ctrl = e10k_control;
    q->q.f.notify = e10k_notify;
    q->q.f.destroy = e10k_destroy;


    *queue = q;
    queues[q->id] = q;

    DEBUG_QUEUE("e10k queue init done\n");
    return SYS_ERR_OK;
}

uint64_t e10k_queue_get_id(struct e10k_queue* q)
{
    return q->id;
}
