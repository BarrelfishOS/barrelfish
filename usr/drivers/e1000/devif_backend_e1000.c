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
#include <driverkit/iommu.h>

#include "e1000n.h"
#include "e1000n_devq.h"

static errval_t e1000_register(struct devq* q, struct capref cap,
                                  regionid_t rid)
{
    e1000_queue_t *device = (e1000_queue_t *)q;
    errval_t err;
    struct dmem mem;

    err = driverkit_iommu_vspace_map_cl(device->iommu, cap,
                                        VREGION_FLAGS_READ_WRITE_NOCACHE,
                                        &mem);
    if (err_is_fail(err)) {
        return err;
    }

    device->region_id = rid;
    device->region_base = mem.devaddr;
    device->region_size = mem.size;

    E1000_DEBUG("%s:%s:  rid:%d:%lx:%lx\n", device->name, __func__, rid, device->region_base, device->region_size);
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
    
    E1000_DEBUG("%s:%s: %lx:%ld:%ld:%ld:%lx\n", device->name, __func__, *offset, *length, *valid_data, *valid_length, *flags);

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

    E1000_DEBUG("%s:%s: %lx:%ld:%ld:%ld:%lx:%lx\n", device->name, __func__, offset, length, valid_data, valid_length,                  flags, tdesc.buffer_address);

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
    E1000_DEBUG("%s:%s: %lx:%ld:%ld:%ld:%lx\n", device->name, __func__, *offset, *length, *valid_data, *valid_length, *flags);

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
    // E1000_DEBUG("%s:%s:\n", device->name, __func__);
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
    device->isr(arg);
}

/*****************************************************************
 * Queue setup local.
 *
 ****************************************************************/
static void e1000_set_rxbsize(e1000_queue_t*eds, e1000_rx_bsize_t rx_bsize)
{
    uint8_t bsize;
    uint8_t bsex;
    e1000_rctl_t rctl;

    switch (rx_bsize) {
    case bsize_16384:
        bsize = 0x1;
        bsex = 1;
        break;
    case bsize_8192:
        bsize = 0x2;
        bsex = 1;
        break;
    case bsize_4096:
        bsize = 0x3;
        bsex = 1;
        break;
    case bsize_2048:
        bsize = 0x0;
        bsex = 0;
        break;
    case bsize_1024:
        bsize = 0x1;
        bsex = 0;
        break;
    case bsize_512:
        bsize = 0x2;
        bsex = 0;
        break;
    case bsize_256:
    default:
        bsize = 0x3;
        bsex = 0;
        break;
    }

    rctl = e1000_rctl_rd(&eds->hw_device);
    rctl = e1000_rctl_bsize_insert(rctl, bsize);
    rctl = e1000_rctl_bsex_insert(rctl, bsex);
    rctl = e1000_rctl_bam_insert(rctl, 1);
    e1000_rctl_wr(&eds->hw_device, rctl);

    e1000_rctl_en_wrf(&eds->hw_device, 1);
}

static void e1000_set_tipg(e1000_queue_t *eds)
{
    e1000_tipg_t tipg = 0;

    if ((eds->mac_type <= e1000_82547_rev_2)
            && (eds->media_type == e1000_media_type_fiber
                || eds->media_type == e1000_media_type_serdes)) {
        tipg = e1000_tipg_ipgt_insert(tipg, DEFAULT_825XX_TIPG_IPGT_FIBER);
    } else {
        tipg = e1000_tipg_ipgt_insert(tipg, DEFAULT_825XX_TIPG_IPGT_COPPER);
    }

    switch (eds->mac_type) {
    case e1000_82542:
        tipg = e1000_tipg_ipgt_insert(tipg, DEFAULT_825XX_TIPG_IPGT);
        tipg = e1000_tipg_ipgr1_insert(tipg, DEFAULT_82542_TIPG_IPGR1);
        tipg = e1000_tipg_ipgr2_insert(tipg, DEFAULT_82542_TIPG_IPGR2);
        break;
    case e1000_82575:
    case e1000_82576:
    case e1000_I210:
    case e1000_I219:
    case e1000_I350:
        tipg = e1000_tipg_ipgr1_insert(tipg, DEFAULT_82575_TIPG_IPGR1);
        tipg = e1000_tipg_ipgr2_insert(tipg, DEFAULT_82575_TIPG_IPGR2);
        break;
    default:
        tipg = e1000_tipg_ipgr1_insert(tipg, DEFAULT_82543_TIPG_IPGR1);
        tipg = e1000_tipg_ipgr2_insert(tipg, DEFAULT_82543_TIPG_IPGR2);
        break;
    }

    e1000_tipg_wr(&eds->hw_device, tipg);
}

static void e1000_setup_rx(e1000_queue_t *device, dmem_daddr_t base)
{
    e1000_t *hw_device = &device->hw_device;

    /* clear MTA table */
    for (int i = 0; i < e1000_mta_length; i++) {
        e1000_mta_wr(hw_device, i, 0);
    }

    E1000_DEBUG("%s:%s: RX base address %lx\n", device->name, __func__, base);
    switch (device->mac_type) {
        case e1000_82576:
        case e1000_I210:
        case e1000_I219:
        case e1000_I350: {
            /*  Software should program RDLEN[n] register only when queue is disabled */
            e1000_rdbal_I350_wr(hw_device, 0, base & 0xffffffff);
            e1000_rdbah_I350_wr(hw_device, 0, (base >> 32) & 0xffffffff);
            e1000_rdlen_I350_len_wrf(hw_device, 0, (device->receive_buffers / 8));

            /* Initialize receive head and tail pointers */
            e1000_rdh_I350_wr(hw_device, 0, 0);
            e1000_rdt_I350_wr(hw_device, 0, 0);
        } break;
        default: {
            /* tell card where receive ring is */
            e1000_rdbal_wr(hw_device, 0, base & 0xffffffff);
            e1000_rdbah_wr(hw_device, 0, (base >> 32) & 0xffffffff);
            e1000_rdlen_len_wrf(hw_device, 0, (device->receive_buffers / 8));

            /* Initialize receive head and tail pointers */
            e1000_rdh_wr(hw_device, 0, 0);
            e1000_rdt_wr(hw_device, 0, 0);
        } break;
    }

    /* set buffer size and enable receive unit */
    e1000_set_rxbsize(device, bsize_2048);

    /* receive descriptor control */
    switch (device->mac_type) {
        case e1000_82575:
        {
            e1000_rxdctl_82575_t rxdctl = 0;

            rxdctl = e1000_rxdctl_82575_enable_insert(rxdctl, 1);
            rxdctl = e1000_rxdctl_82575_wthresh_insert(rxdctl, 1);
            e1000_rxdctl_82575_wr(hw_device, 0, rxdctl);
            
            E1000_DEBUG("%s: rxdctl %x\n", __func__, e1000_rxdctl_82575_rd(hw_device, 0));
        } break;
        case e1000_82576:
        case e1000_I210:
        case e1000_I219:
        case e1000_I350: {
            /* If VLANs are not used, software should clear VFE. */
            e1000_rctl_vfe_wrf(hw_device, 0);

             /* Set up the MTA (Multicast Table Array) by software. This means
              * zeroing all entries initially and adding in entries as requested. */
            for (int i = 0; i < 128; ++i) {
                e1000_mta_wr(hw_device, i, 0);
            }

            /* Program SRRCTL of the queue according to the size of the buffers,
             * the required header handling and the drop policy. */
            e1000_srrctl_t srrctl = 0;
            srrctl = e1000_srrctl_bsizeheader_insert(srrctl, 0);
            e1000_srrctl_wr(hw_device, 0, srrctl);

            /* Enable the queue by setting RXDCTL.ENABLE. In the case of queue zero,
             * the enable bit is set by default - so the ring parameters should be
             * set before RCTL.RXEN is set. */
            e1000_rxdctl_I350_t rxdctl = 0;
            rxdctl = e1000_rxdctl_I350_enable_insert(rxdctl, 1);
            rxdctl = e1000_rxdctl_I350_wthresh_insert(rxdctl, 1);
            e1000_rxdctl_I350_wr(hw_device, 0, rxdctl);

            /* Poll the RXDCTL register until the ENABLE bit is set. The tail should
             * not be bumped before this bit was read as one. */
            int timeout = 1000;
            while (!e1000_rxdctl_I350_enable_rdf(hw_device, 0) && timeout--) {
                // usec_delay(10);
            }
            E1000_DEBUG("%s.%d: timeout=%d\n", __func__, __LINE__, timeout);
            // if (timeout <= 0) {
            //     E1000_DEBUG("ERROR: failed to enable the RX queue\n");
            // }
        } break;
        default: {
            e1000_rxdctl_t rxdctl = 0;

            rxdctl = e1000_rxdctl_gran_insert(rxdctl, 1);
            rxdctl = e1000_rxdctl_wthresh_insert(rxdctl, 1);
            e1000_rxdctl_wr(hw_device, 0, rxdctl);

            e1000_rfctl_exsten_wrf(hw_device, 0);
        } break;
    }
    
    E1000_DEBUG("%s: rctl:%x  rxdctl:%x\n", __func__, e1000_rctl_rd(hw_device), e1000_rxdctl_rd(hw_device, 0));
}


static void e1000_setup_tx(e1000_queue_t *device,  dmem_daddr_t base)
{
    e1000_t *hw_device = &device->hw_device;

    E1000_DEBUG("%s:%s: TX base address %lx\n", device->name, __func__, base);
    switch (device->mac_type) {
        case e1000_82576:
        case e1000_I210:
        case e1000_I219:
        case e1000_I350: {
            /* Software should program TDLEN[n] register only when queue is disabled */
            e1000_tdbal_I350_wr(hw_device, 0, base & 0xffffffff);
            e1000_tdbah_I350_wr(hw_device, 0, base >> 32);
            e1000_tdlen_I350_len_wrf(hw_device, 0, (device->transmit_buffers / 8));
            e1000_tdh_I350_wr(hw_device, 0, 0);
            e1000_tdt_I350_wr(hw_device, 0, 0);
        } break;
        default: {
            /* tell card about our transmit ring */
            e1000_tdbal_wr(hw_device, 0, base & 0xffffffff);
            e1000_tdbah_wr(hw_device, 0, base >> 32);
            e1000_tdlen_len_wrf(hw_device, 0, (device->transmit_buffers/ 8));
            e1000_tdh_wr(hw_device, 0, 0);
            e1000_tdt_wr(hw_device, 0, 0);
        } break;
    }
    
    /* --------------------- transmit setup --------------------- */
    switch (device->mac_type) {
        case e1000_82575:
        {
            e1000_txdctl_82575_t txdctl = 0;
            txdctl = e1000_txdctl_82575_enable_insert(txdctl, 1);
            txdctl = e1000_txdctl_82575_priority_insert(txdctl, 1);
            e1000_txdctl_82575_wr(hw_device, 0, txdctl);
        } break;
        case e1000_82576:
        case e1000_I210:
        case e1000_I219:
        case e1000_I350: {
            /* Program the TXDCTL register with the desired TX descriptor write
             * back policy. Suggested values are:
                    — WTHRESH = 1b
                    — All other fields 0b.
             */
            e1000_txdctl_I350_t txdctl = 0;
            // txdctl = e1000_txdctl_I350_priority_insert(txdctl, 1);
            txdctl = e1000_txdctl_I350_wthresh_insert(txdctl, 1);
            e1000_txdctl_I350_wr(hw_device, 0, txdctl);

            /* If needed, set the TDWBAL/TWDBAH to enable head write back */
            e1000_tdwbal_wr(hw_device, 0, 0);
            e1000_tdwbah_wr(hw_device, 0, 0);

            /* Enable the queue using TXDCTL.ENABLE (queue zero is enabled by default). */
            e1000_txdctl_I350_enable_wrf(hw_device, 0, 1);
        } break;
        default: {
            e1000_txdctl_t txdctl = 0;
            txdctl = e1000_txdctl_gran_insert(txdctl, 1);
            txdctl = e1000_txdctl_wthresh_insert(txdctl, 1);
            e1000_txdctl_wr(hw_device, 0, txdctl);
        } break;
    }
    /* enable transmit */

    e1000_tctl_t tctl = 0;
    tctl = e1000_tctl_ct_insert(tctl, 0xf);
    tctl = e1000_tctl_en_insert(tctl, 1);
    tctl = e1000_tctl_psp_insert(tctl, 1);
    e1000_tctl_wr(hw_device, tctl);

    if (device->mac_type == e1000_82576 || device->mac_type == e1000_I210 || 
        device->mac_type == e1000_I350  || device->mac_type == e1000_I219 ) {
            /* Poll the TXDCTL register until the ENABLE bit is set. */
            int timeout = 1000;
            while(!e1000_txdctl_I350_enable_rdf(hw_device, 0) && timeout--) {
                // usec_delay(10);
            }
            E1000_DEBUG("%s.%d: timeout=%d\n", __func__, __LINE__, timeout);
            // if (timeout <= 0) {
            //     E1000_DEBUG("ERROR: failed to enable the TX queue\n");
            // }
    }
    e1000_set_tipg(device);
    E1000_DEBUG("%s: tctl:%x  txdctl:%x\n", __func__, e1000_tctl_rd(hw_device), e1000_txdctl_rd(hw_device, 0));
}

errval_t e1000_queue_create(e1000_queue_t ** q, struct capref* ep, uint32_t vendor, 
    uint32_t deviceid, uint32_t bus, uint32_t pci_device, uint32_t function, 
    unsigned interrupt_mode, void (*isr)(void *))
{
    errval_t err;
    e1000_queue_t *device;
    struct frame_identity id;

    E1000_DEBUG("%s: %x:%x:%x:%x:%x  %d\n", __func__, vendor, deviceid, bus,
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

    if (ep == NULL) {
        char service[128];
        snprintf(service, 128, "e1000_%x_%x_%x_%s", bus, pci_device, function, "devif");
        // Connect to e1000 card driver
        iref_t iref;
        err = nameservice_blocking_lookup(service, &iref);
        if (err_is_fail(err)) {
            goto error;
        }

        err = e1000_devif_bind(iref, bind_cb, device, get_default_waitset(), 1);
        if (err_is_fail(err)) {
            goto error;
        }
    } else {

        err = e1000_devif_bind_to_endpoint(*ep, bind_cb, device, get_default_waitset(), 1);
        if (err_is_fail(err)) {
            goto error;
        }
    }
    // wait until bound
    while(!device->bound) {
        event_dispatch(get_default_waitset());
    }


    err = slot_alloc(&device->regs);
    if (err_is_fail(err)) {
        goto error;
    }

    err = slot_alloc(&device->irq);
    if (err_is_fail(err)) {
        slot_free(device->regs);
        goto error;
    }

    err = slot_alloc(&device->iommu_ep);
    if (err_is_fail(err)) {
        slot_free(device->irq);
        slot_free(device->regs);
        goto error;
    }

    errval_t err2;
    err = device->b->rpc_tx_vtbl.create_queue(device->b, (bool) interrupt_mode,
                                              &device->mac_address, &device->media_type, &device->regs,
                                              &device->irq, &device->iommu_ep,
                                              &err2);
    if (err_is_fail(err) || err_is_fail(err2)) {
        E1000_DEBUG("%s:%d: failed create queue \n", __func__, __LINE__);
        err = err_is_fail(err) ? err: err2;

        goto error;
    }

    err = invoke_frame_identify(device->regs, &id);
    if (err_is_fail(err)) {
        goto error;
    }

    E1000_DEBUG("%s:%d: Mapping frame \n", __func__, __LINE__);
    void* va;
    err = vspace_map_one_frame_attr(&va, id.bytes, device->regs,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        goto error;
    }
      
    E1000_DEBUG("%s:%d: Init regs \n", __func__, __LINE__);
    e1000_initialize(&(device->hw_device), va);

    E1000_DEBUG("%s:%d: Init iommu client \n", __func__, __LINE__);

    // Set buffer ring sizes
    device->rx_ring_size = device->transmit_buffers*sizeof(union rx_desc);
    device->tx_ring_size = device->receive_buffers*sizeof(struct tx_desc);

    // and create queue. set the transmit receive ring
    err = driverkit_iommu_client_init_cl(device->iommu_ep, &device->iommu);
    if (err_is_fail(err)) {
        debug_printf("########## e1000: Initializing IOMMU client failed, Continue withouth IOMMU ####### \n");
        device->iommu = NULL;
        // Continue !   
    }

    E1000_DEBUG("%s:%d: transmit ring \n", __func__, __LINE__);
    err = driverkit_iommu_mmap_cl(device->iommu, device->tx_ring_size, VREGION_FLAGS_READ_WRITE_NOCACHE, 
                                  &device->tx);
    device->transmit_ring = (void*) device->tx.vbase;
    if (err_is_fail(err)) {
        // TODO propery cleanup
        goto error;
    }

    E1000_DEBUG("%s:%d: receive ring \n", __func__, __LINE__);
    err = driverkit_iommu_mmap_cl(device->iommu, device->rx_ring_size, VREGION_FLAGS_READ_WRITE_NOCACHE, 
                                  &device->rx);
    device->receive_ring = (void*) device->rx.vbase;
    if (device->receive_ring == NULL) {
        // TODO propery cleanup
        goto error;
    }

    e1000_setup_tx(device, device->tx.devaddr);
    e1000_setup_rx(device, device->rx.devaddr);

    err = devq_init(&device->q, false);
    assert(err_is_ok(err));

    E1000_DEBUG("interrupt_mode=%s\n", interrupt_mode ? "on" : "off");
    if(interrupt_mode){
        /*
        err = int_route_client_route_and_connect(device->irq, 0,
                get_default_waitset(), interrupt_handler, device);
        if(err_is_fail(err)){
            DEBUG_ERR(err, "int_route_client_route_and_connect");
            goto error;
        }

        */
        /* interrupts don't work yetr */
        err = periodic_event_create(&device->event, get_default_waitset(),
                                    50, MKCLOSURE(interrupt_handler, device));
    }

    device->q.f.enq = e1000_enqueue;
    device->q.f.deq = e1000_dequeue;
    device->q.f.reg = e1000_register;
    device->q.f.dereg = e1000_deregister;
    device->q.f.ctrl = e1000_control;
    device->q.f.notify = e1000_notify;
    device->q.f.destroy = e1000_destroy;
    
    *q = device;
    device->q.iommu = device->iommu;
    
    return SYS_ERR_OK;

error:
    free(device);
    return err;   
}
