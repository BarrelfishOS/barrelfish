/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/inthandler.h>
#include <devif/queue_interface_backend.h>
#include <devif/backends/net/e1000_devif.h>
#include <net_interfaces/flags.h>
#include <debug_log/debug_log.h>
#include <if/e1000_devif_defs.h>
#include <int_route/int_route_client.h>

#include "e1000n.h"
#include "e1000n_devq.h"



/*****************************************************************
 * allocate a single frame, mapping it into our vspace with given
 * attributes
 ****************************************************************/
static void *alloc_map_frame(vregion_flags_t attr, size_t size, struct capref *retcap)
{
    struct capref frame;
    errval_t err;
    void *va;

    err = frame_alloc(&frame, size, NULL);
    if (err_is_fail(err)) {
        E1000_DEBUG("ERROR: frame_alloc failed.\n");
        return NULL;
    }

    err = vspace_map_one_frame_attr(&va, size, frame, attr, NULL, NULL);

    if (err_is_fail(err)) {
        E1000_DEBUG("Error: vspace_map_one_frame failed\n");
        return NULL;
    }

    if (retcap != NULL) {
        *retcap = frame;
    }

    return va;
}

static errval_t e1000_register(struct devq* q, struct capref cap,
                                  regionid_t rid)
{
    e1000_queue_t *device = (e1000_queue_t *)q;
    struct frame_identity id;
    errval_t err;
    
    err = invoke_frame_identify(cap, &id);
    assert(err_is_ok(err));
    assert(!device->region_id);
    device->region_id = rid;
    device->region_base = id.base;
    device->region_size = id.bytes;
    debug_printf("%s:%s:  rid:%d:%lx:%lx\n", device->name, __func__, rid, device->region_base, device->region_size);
    return SYS_ERR_OK;
}

static errval_t e1000_deregister(struct devq* q, regionid_t rid)
{
    return SYS_ERR_OK;
}


static errval_t e1000_control(struct devq* q, uint64_t cmd, uint64_t value,
                                 uint64_t *result)
{
    e1000_queue_t *device = (e1000_queue_t *)q;
    *result = device->mac_address;
    return SYS_ERR_OK;
}

static errval_t e1000_enqueue_rx(e1000_queue_t *device, regionid_t rid,
                               genoffset_t offset, genoffset_t length,
                               genoffset_t valid_data, genoffset_t valid_length,
                               uint64_t flags)
{

    if (e1000_queue_free_rxslots(device) == 0) {
        E1000_DEBUG("Not enough space in RX ring, not transmitting buffer \n" );
        return DEVQ_ERR_QUEUE_FULL;
    }

    union rx_desc desc;

    desc.raw[0] = desc.raw[1] = 0;
    desc.rx_read_format.buffer_address = device->region_base + offset;

    device->receive_ring[device->receive_tail] = desc;
    device->receive_tail = (device->receive_tail + 1) % DRIVER_RECEIVE_BUFFERS;
    

    e1000_dqval_t dqval = 0;
    dqval = e1000_dqval_val_insert(dqval, device->receive_tail);
    e1000_rdt_wr(&device->hw_device, 0, dqval);

    return SYS_ERR_OK;
}

static errval_t e1000_dequeue_rx(e1000_queue_t *device, regionid_t* rid, 
                                 genoffset_t* offset,
                                 genoffset_t* length, genoffset_t* valid_data,
                                 genoffset_t* valid_length, uint64_t* flags)
{
    if (device->receive_head == device->receive_tail) {
        return DEVQ_ERR_QUEUE_EMPTY;
    }

    volatile union rx_desc *rxd;
    rxd = &device->receive_ring[device->receive_head];
    if (!rxd->rx_read_format.info.status.dd ||
            !rxd->rx_read_format.info.status.eop) {
        return DEVQ_ERR_QUEUE_EMPTY;
    }
    
    device->receive_head = (device->receive_head + 1) % DRIVER_TRANSMIT_BUFFERS;
    
    *rid = device->region_id;
    *offset = rxd->rx_read_format.buffer_address - device->region_base;
    *length = 2048;
    *valid_data = 0;
    *valid_length = rxd->rx_read_format.info.length;
    *flags = NETIF_RXFLAG;
    
    //debug_printf("%s:%s: %lx:%ld:%ld:%ld:%lx\n", device->name, __func__, *offset, *length, *valid_data, *valid_length, *flags);

    return SYS_ERR_OK;
}

static errval_t e1000_enqueue_tx(e1000_queue_t *device, regionid_t rid,
                               genoffset_t offset, genoffset_t length,
                               genoffset_t valid_data, genoffset_t valid_length,
                               uint64_t flags)
{
    struct tx_desc tdesc;

    if (e1000_queue_free_txslots(device) == 0) {
        E1000_DEBUG("Not enough space in TX ring, not transmitting buffer \n" );
        return DEVQ_ERR_QUEUE_FULL;
    }

    if (device->advanced_descriptors == 3) {
        tdesc.buffer_address = device->region_base + offset + valid_data;
        tdesc.ctrl.raw = 0;
        tdesc.ctrl.advanced_data.dtalen = valid_length;
        tdesc.ctrl.advanced_data.dtyp.d.dtyp = 3; // advanced data descriptor
        tdesc.ctrl.advanced_data.dcmd.d.eop = 1;
        tdesc.ctrl.advanced_data.dcmd.d.ifcs = 1;
        tdesc.ctrl.advanced_data.dcmd.d.rs = 1;
        tdesc.ctrl.advanced_data.dcmd.d.dext = 1;
        tdesc.ctrl.advanced_data.popts_paylen.d.paylen = valid_length;
    } else if (device->advanced_descriptors == 1) {
        tdesc.buffer_address = device->region_base + offset + valid_data;
        tdesc.ctrl.raw = 0;
        tdesc.ctrl.extended_data.data_len = valid_length;
        tdesc.ctrl.extended_data.dtyp = 1; // extended data descriptor
        tdesc.ctrl.extended_data.dcmd.d.rs = 1;
        tdesc.ctrl.extended_data.dcmd.d.ifcs = 1;
        tdesc.ctrl.extended_data.dcmd.d.eop = 1;
        tdesc.ctrl.extended_data.dcmd.d.dext = 1;
    } else {
        tdesc.buffer_address = device->region_base + offset + valid_data;
        tdesc.ctrl.raw = 0;
        tdesc.ctrl.legacy.data_len = valid_length;
        tdesc.ctrl.legacy.cmd.d.eop = 1;
        tdesc.ctrl.legacy.cmd.d.ifcs = 1;
        tdesc.ctrl.legacy.cmd.d.rs = 1;
    }

    device->transmit_ring[device->transmit_tail] = tdesc;
    device->transmit_tail = (device->transmit_tail + 1) % DRIVER_TRANSMIT_BUFFERS;

    e1000_dqval_t dqval = 0;
    dqval = e1000_dqval_val_insert(dqval, device->transmit_tail);
    e1000_tdt_wr(&device->hw_device, 0, dqval);

    return SYS_ERR_OK;
}

static errval_t e1000_dequeue_tx(e1000_queue_t *device, regionid_t* rid, genoffset_t* offset,
                                 genoffset_t* length, genoffset_t* valid_data,
                                 genoffset_t* valid_length, uint64_t* flags)
{
    if (device->transmit_head == device->transmit_tail) {
        return DEVQ_ERR_QUEUE_EMPTY;
    }

    volatile struct tx_desc *txd;
    txd = &device->transmit_ring[device->transmit_head];
    if (txd->ctrl.legacy.stat_rsv.d.dd != 1) {
        return DEVQ_ERR_QUEUE_EMPTY;
    }
    device->transmit_head = (device->transmit_head + 1) % DRIVER_TRANSMIT_BUFFERS;
    
    *rid = device->region_id;
    *offset = txd->buffer_address - device->region_base;
    *length = 2048;
    *valid_data = *offset & 2047;
    *offset &= ~2047;
    *valid_length = txd->ctrl.legacy.data_len;
    *flags = NETIF_TXFLAG | NETIF_TXFLAG_LAST;
    
    // debug_print_to_log("DEQTX %d", *valid_length);
    //debug_printf("%s:%s: %lx:%ld:%ld:%ld:%lx\n", device->name, __func__, *offset, *length, *valid_data, *valid_length, *flags);

    return SYS_ERR_OK;
}


static errval_t e1000_enqueue(struct devq* q, regionid_t rid,
                                 genoffset_t offset, genoffset_t length,
                                 genoffset_t valid_data, genoffset_t valid_length,
                                 uint64_t flags)
{
    e1000_queue_t *device = (e1000_queue_t *)q;
    errval_t err;

    if (flags & NETIF_RXFLAG) {
        /* can not enqueue receive buffer larger than 2048 bytes */
        assert(length <= 2048);

        err = e1000_enqueue_rx(device, rid, offset, length, valid_data, valid_length,
                             flags);
        if (err_is_fail(err)) {
            return err;
        }
    } else if (flags & NETIF_TXFLAG) {
        assert(length <= BASE_PAGE_SIZE);

        err = e1000_enqueue_tx(device, rid, offset, length, valid_data, valid_length,
                             flags);
        if (err_is_fail(err)) {
            return err;
        }
    } else {
        printf("Unknown buffer flags \n");
        return NIC_ERR_ENQUEUE;
    }

    return SYS_ERR_OK;
}

static errval_t e1000_dequeue(struct devq* q, regionid_t* rid, genoffset_t* offset,
                                 genoffset_t* length, genoffset_t* valid_data,
                                 genoffset_t* valid_length, uint64_t* flags)
{
    e1000_queue_t *device = (e1000_queue_t *)q;
    
    if (e1000_dequeue_tx(device, rid, offset, length, valid_data, valid_length,  flags) == SYS_ERR_OK) 
        return SYS_ERR_OK;
    if (e1000_dequeue_rx(device, rid, offset, length, valid_data, valid_length, flags) == SYS_ERR_OK) {
        return SYS_ERR_OK;
    }
    // debug_printf("%s:%s:\n", device->name, __func__);
    return DEVQ_ERR_QUEUE_EMPTY;
}

static errval_t e1000_notify(struct devq* q)
{
    assert(0);
    return SYS_ERR_OK;
}

static errval_t e1000_destroy(struct devq * queue)
{
    e1000_queue_t* q = (e1000_queue_t *) queue;
    free(q);
    // TODO rest of the cleanup
    return SYS_ERR_OK;
}

static void bind_cb(void *st, errval_t err, struct e1000_devif_binding *b)
{
    e1000_queue_t* queue = (e1000_queue_t*) st;
    b->st = st;
    
    queue->b = b;
    e1000_devif_rpc_client_init(queue->b);
    queue->bound = true;
}

static void interrupt_handler(void* arg) {
    e1000_queue_t *device = (e1000_queue_t*) arg;
    e1000_intreg_t icr = e1000_icr_rd(&device->hw_device);

    icr = 0;
    /*
    if (e1000_intreg_lsc_extract(icr) != 0) {
        if (e1000_check_link_up(&device->hw_device)) {
            e1000_auto_negotiate_link(&device->hw_device, device->mac_type);
        } else {
            E1000_DEBUG("Link status change to down.\n");
        }
    }
    */
    device->isr(arg);
}


errval_t e1000_queue_create(e1000_queue_t ** q, uint32_t vendor, uint32_t deviceid,
    uint32_t bus, uint32_t pci_device, uint32_t function, unsigned interrupt_mode,
    void (*isr)(void *))
{
    errval_t err;
    e1000_queue_t *device;
    struct frame_identity id;

    debug_printf("%s: %x:%x:%x:%x:%x  %d\n", __func__, vendor, deviceid, bus,
        pci_device, function, interrupt_mode);

    device = malloc(sizeof(e1000_queue_t));
    assert(device);

    device->pci_vendor = vendor;
    device->pci_deviceid = deviceid;
    device->pci_bus = bus;
    device->pci_device = pci_device;
    device->pci_function = function;
    device->interrupt_mode = interrupt_mode;
    device->name = malloc(128);
    snprintf(device->name, 128, "e1000:%x:%x:%x:%x:%x", vendor, deviceid, bus,
        pci_device, function);

    device->receive_head = 0;
    device->receive_tail = 0;
    device->transmit_head = 0;
    device->transmit_tail = 0;
    device->region_id = 0;
    device->isr = isr;
    device->bound = false;

    device->receive_buffers = DRIVER_RECEIVE_BUFFERS;
    device->transmit_buffers = DRIVER_TRANSMIT_BUFFERS;

    device->mac_type = e1000_get_mac_type(device->pci_vendor, device->pci_deviceid);

    switch (device->mac_type) {
        case e1000_82571:
        case e1000_82572:
        case e1000_82574: {
            device->extended_interrupts = 0;
            device->advanced_descriptors = 1;
        } break;
        case e1000_82576:
        case e1000_I210:
        case e1000_I219:
        case e1000_I350: {
            device->extended_interrupts = 1;
            device->advanced_descriptors = 3;
        } break;
    default:
        device->extended_interrupts = 0;
        device->advanced_descriptors = 0;
    }

    //e1000_init(device, interrupt_mode);

    char service[128];
    snprintf(service, 128, "e1000_%x_%x_%x_%s", bus, pci_device, function, "devif");
    // Connect to e1000 card driver
    iref_t iref;
    err = nameservice_blocking_lookup(service, &iref);
    if (err_is_fail(err)) {
        return err;
    }

    err = e1000_devif_bind(iref, bind_cb, device, get_default_waitset(), 1);
    if (err_is_fail(err)) {
        return err;
    }
    
    // wait until bound
    while(!device->bound) {
        event_dispatch(get_default_waitset());
    }

    // and create queue. set the transmit receive ring

    device->transmit_ring = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, 
                                            DRIVER_TRANSMIT_BUFFERS*sizeof(struct tx_desc), 
                                            &device->tx);
    if (device->transmit_ring == NULL) {
        return DEVQ_ERR_INIT_QUEUE;
    }

    device->receive_ring = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, 
                                           DRIVER_RECEIVE_BUFFERS*sizeof(union rx_desc), 
                                           &device->rx);
    if (device->receive_ring == NULL) {
        return DEVQ_ERR_INIT_QUEUE;
    }

    err = slot_alloc(&device->regs);
    if (err_is_fail(err)) {
        return err;
    }

    errval_t err2;
    err = device->b->rpc_tx_vtbl.create_queue(device->b, device->rx, device->tx, 
                                             (bool) interrupt_mode,
                                              &device->mac_address, &device->regs,
                                              &device->irq,
                                              &err2);
    if (err_is_fail(err) || err_is_fail(err2)) {
        err = err_is_fail(err) ? err: err2;
        return err;
    }

    err = invoke_frame_identify(device->regs, &id);
    if (err_is_fail(err)) {
        return err;
    }

    void* va;
    err = vspace_map_one_frame_attr(&va, id.bytes, device->regs,
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        return err;
    }
      
    e1000_initialize(&(device->hw_device), va);

    err = devq_init(&device->q, false);
    assert(err_is_ok(err));

    E1000_DEBUG("interrupt_mode=%s\n", interrupt_mode ? "on" : "off");
    if(interrupt_mode){
        err = int_route_client_connect(); //Multiple calls are OK
        if(err_is_fail(err)){
            DEBUG_ERR(err, "int_route_client_connect");
            return err;
        }

        err = int_route_client_route_and_connect(device->irq, 0,
                get_default_waitset(), interrupt_handler, device);
        if(err_is_fail(err)){
            DEBUG_ERR(err, "int_route_client_route_and_connect");
        }
    }
    
    device->q.f.enq = e1000_enqueue;
    device->q.f.deq = e1000_dequeue;
    device->q.f.reg = e1000_register;
    device->q.f.dereg = e1000_deregister;
    device->q.f.ctrl = e1000_control;
    device->q.f.notify = e1000_notify;
    device->q.f.destroy = e1000_destroy;
    
    *q = device;

    return SYS_ERR_OK;
}
