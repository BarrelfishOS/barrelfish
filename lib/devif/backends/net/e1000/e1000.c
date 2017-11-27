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
#include <devif/queue_interface_backend.h>
#include <devif/backends/net/e1000_devif.h>
#include "e1000.h"
#include <net_interfaces/flags.h>
#include <debug_log/debug_log.h>

#define DRIVER_RECEIVE_BUFFERS      (16384)
#define DRIVER_TRANSMIT_BUFFERS     (16384)

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
    e1000_queue_t *device = (e1000_queue_t *)q;
    debug_printf("%s:%s:\n", device->name, __func__);
    return SYS_ERR_OK;
}


static errval_t e1000_control(struct devq* q, uint64_t cmd, uint64_t value,
                                 uint64_t *result)
{
    e1000_queue_t *device = (e1000_queue_t *)q;
    debug_printf("%s:%s:\n", device->name, __func__);
    *result = device->mac_address;
    return SYS_ERR_OK;
}

static errval_t e1000_enqueue_rx(e1000_queue_t *device, regionid_t rid,
                               genoffset_t offset, genoffset_t length,
                               genoffset_t valid_data, genoffset_t valid_length,
                               uint64_t flags)
{
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

static errval_t e1000_dequeue_rx(e1000_queue_t *device, regionid_t* rid, genoffset_t* offset,
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
    
    // debug_print_to_log("DEQRX %d", *valid_length);
    // debug_printf("%s:%s: %lx:%ld:%ld:%ld:%lx\n", device->name, __func__, *offset, *length, *valid_data, *valid_length, *flags);

    return SYS_ERR_OK;
}

static errval_t e1000_enqueue_tx(e1000_queue_t *device, regionid_t rid,
                               genoffset_t offset, genoffset_t length,
                               genoffset_t valid_data, genoffset_t valid_length,
                               uint64_t flags)
{
    struct tx_desc tdesc;

    // debug_printf("%s:%s: %lx:%ld:%ld:%ld:%lx\n", device->name, __func__, offset, length, valid_data, valid_length, flags);
    // debug_print_to_log("ENQTX %d", valid_length);

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
    // debug_printf("%s:%s: %lx:%ld:%ld:%ld:%lx\n", device->name, __func__, *offset, *length, *valid_data, *valid_length, *flags);

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
    
    if (e1000_dequeue_tx(device, rid, offset, length, valid_data, valid_length, flags) == SYS_ERR_OK)
        return SYS_ERR_OK;
    if (e1000_dequeue_rx(device, rid, offset, length, valid_data, valid_length, flags) == SYS_ERR_OK)
        return SYS_ERR_OK;
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

/*****************************************************************
 * Reset the device and disable interrupts.
 *
 ****************************************************************/
static int e1000_reset(e1000_queue_t *device)
{
    // errval_t err = 0;
    int timeout;
    e1000_t *hw_device = &device->hw_device;

    /* disable interrupts */
    if (device->extended_interrupts) {
        e1000_eimc_wr(hw_device, 0xffffffff);
    }
    e1000_imc_rawwr(hw_device, 0xffffffff);

    /* disable receive and transmit */
    e1000_rctl_rawwr(hw_device, 0);
    e1000_tctl_rawwr(hw_device, 0);

    /* Delay to allow outstanding PCI transactions to complete before
     * reseting the device */
    // usec_delay(1000);
    e1000_ctrl_phy_rst_wrf(hw_device, 1);
    e1000_ctrl_rst_wrf(hw_device, 1);
    /* Wait for reset to clear */
    timeout = 1000;
    do {
        // usec_delay(10);
    } while (e1000_ctrl_rst_rdf(hw_device) != 0 && 0 < timeout--);
    assert(timeout >= 0);
    debug_printf("%s.%d: timeout=%d\n", __func__, __LINE__, timeout);

    /* clear any pending interrupts */
    e1000_icr_rd(hw_device);
    if (device->extended_interrupts) {
        e1000_eicr_rd(hw_device);
    }
    
    e1000_ctrl_phy_rst_wrf(hw_device, 0);

    debug_printf("Reset done..\n");

    return 0;
}

/*
 * setup MAC
 */
static void e1000_setup_mac(e1000_queue_t *device, uint64_t *mac_addr)
{
    e1000_t *hw_device = &device->hw_device;
    
    /* is a valid MAC already present? */
    /* This will always return false due to hardware/software reset */
    bool mac_present = e1000_rah_av_rdf(hw_device, 0);
    
    assert(mac_present);
    /* cache MAC for stack to see */
    uint64_t mac_hi = e1000_rah_rah_rdf(hw_device, 0);
    uint64_t mac = e1000_ral_rd(hw_device, 0) + (mac_hi << 32);
    *mac_addr = mac | (mac_hi << 32);

    debug_printf("%s: MAC address: %lx\n", device->name, *mac_addr);
    device->mac_address = *mac_addr;

    /* clear all other filers (clear high-to-low (13.4.3)) */
    for (int i = 1; i < e1000_ral_length; i++) {
        e1000_rah_wr(hw_device, i, 0);
        e1000_ral_wr(hw_device, i, 0);
    }

}

static void e1000_setup_link(e1000_queue_t *device)
{
    e1000_t *hw_device = &device->hw_device;
    e1000_ctrl_slu_wrf(hw_device, 1);
    // e1000_ctrl_asde_wrf(hw_device, 1);
}

/*
 * Set interrupt throttle for all interrupts
 */
static void e1000_set_interrupt_throttle(e1000_queue_t *device, uint16_t usec)
{
    e1000_t *hw_device = &device->hw_device;
    /* Enable interrupt throttling rate.
     *
     * The optimal performance setting for this register is very system and
     * configuration specific. A initial suggested range is 651-5580 (28Bh - 15CCh).
     * The value 0 will disable interrupt throttling
     */
    int16_t rate = usec;
    
    if (device->mac_type == e1000_82575
        || device->mac_type == e1000_82576
        || device->mac_type == e1000_I210
        || device->mac_type == e1000_I350) {
        e1000_eitr_interval_wrf(hw_device, 0, rate);
        e1000_eitr_interval_wrf(hw_device, 1, rate);
        e1000_eitr_interval_wrf(hw_device, 2, rate);
        e1000_eitr_interval_wrf(hw_device, 3, rate);
    } else if (device->mac_type == e1000_82574) {
        e1000_itr_interval_wrf(hw_device, rate * 4);
        e1000_eitr_82574_interval_wrf(hw_device, 0, rate);
        e1000_eitr_82574_interval_wrf(hw_device, 1, rate);
        e1000_eitr_82574_interval_wrf(hw_device, 2, rate);
        e1000_eitr_82574_interval_wrf(hw_device, 3, rate);
    } else {
        e1000_itr_interval_wrf(hw_device, rate * 4);
    }
}

static void e1000_enable_interrupts(e1000_queue_t *device)
{
    e1000_t *hw_device = &device->hw_device;

    e1000_set_interrupt_throttle(device, E1000_DEFAULT_INT_THROTTLE_RATE);

    if (device->extended_interrupts) {
        e1000_intreg_t intreg = 0;
        /* Activate link change interrupt */
        intreg = e1000_intreg_lsc_insert(intreg, 1);
        e1000_ims_wr(hw_device, intreg);
        
        e1000_ivar_int_alloc0_wrf(hw_device, 0, 0);
        e1000_ivar_int_alloc1_wrf(hw_device, 0, 1);
        e1000_ivar_int_alloc_val0_wrf(hw_device, 0, 1);
        e1000_ivar_int_alloc_val1_wrf(hw_device, 0, 1);

        e1000_eintreg_t eintreg = 0;
        eintreg = e1000_eintreg_rxtxq0_insert(eintreg, 1);
        eintreg = e1000_eintreg_rxtxq1_insert(eintreg, 1);

        e1000_iam_wr(hw_device, 0);
        e1000_eims_wr(hw_device, eintreg);
        e1000_eiac_wr(hw_device, 0);
        e1000_eiam_wr(hw_device, eintreg);
        e1000_gpie_nsicr_wrf(hw_device, 1);
    } else {
        e1000_intreg_t intreg = 0;
        /* Activate link change interrupt */
        intreg = e1000_intreg_lsc_insert(intreg, 1);
        /* Activate rx0 interrupt */
        intreg = e1000_intreg_rxt0_insert(intreg, 1);
        intreg = e1000_intreg_txdw_insert(intreg, 1);
        e1000_ims_wr(hw_device, intreg);

    /* In case of the 82574, we explicitly activate int cause auto clear to
     * get the same behaviour as the other cards */
        if (device->mac_type == e1000_82574) {
            e1000_ctrlext_iame_wrf(hw_device, 1);
        }
    }
    debug_printf("%s: Interrupts enabled\n", __func__);
}


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
    assert(err_is_ok(err));
    err = vspace_map_one_frame_attr(&va, size, frame, attr, NULL, NULL);
    assert(err_is_ok(err));

    if (retcap != NULL) {
        *retcap = frame;
    }
    return va;
}

/*****************************************************************
 * Set RX buffer size and enable receive unit.
 *
 ****************************************************************/
static void e1000_set_rxbsize(e1000_queue_t *device, e1000_rx_bsize_t rx_bsize)
{
    e1000_t *hw_device = &device->hw_device;
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

    rctl = e1000_rctl_rd(hw_device);
    rctl = e1000_rctl_bsize_insert(rctl, bsize);
    rctl = e1000_rctl_bsex_insert(rctl, bsex);
    rctl = e1000_rctl_bam_insert(rctl, 1);
    e1000_rctl_wr(hw_device, rctl);

    e1000_rctl_en_wrf(hw_device, 1);
}

/*****************************************************************
 * Configure device receive
 *
 ****************************************************************/
static void e1000_setup_rx(e1000_queue_t *device)
{
    e1000_t *hw_device = &device->hw_device;
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    struct capref frame;
    errval_t err;

    /* Allocate and map frame for receive ring */
    device->receive_ring = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
                                sizeof(union rx_desc) * device->receive_buffers,
                                &frame);
    assert(device->receive_ring);
    err = invoke_frame_identify(frame, &frameid);
    assert(err_is_ok(err));

    /* clear MTA table */
    for (int i = 0; i < e1000_mta_length; i++) {
        e1000_mta_wr(hw_device, i, 0);
    }

    switch (device->mac_type) {
        case e1000_82576:
        case e1000_I210:
        case e1000_I350: {
            /*  Software should program RDLEN[n] register only when queue is disabled */
            e1000_rdbal_I350_wr(hw_device, 0, frameid.base & 0xffffffff);
            e1000_rdbah_I350_wr(hw_device, 0, (frameid.base >> 32) & 0xffffffff);
            e1000_rdlen_I350_len_wrf(hw_device, 0, (device->receive_buffers / 8));

            /* Initialize receive head and tail pointers */
            e1000_rdh_I350_wr(hw_device, 0, 0);
            e1000_rdt_I350_wr(hw_device, 0, 0);
        } break;
        default: {
            /* tell card where receive ring is */
            e1000_rdbal_wr(hw_device, 0, frameid.base & 0xffffffff);
            e1000_rdbah_wr(hw_device, 0, (frameid.base >> 32) & 0xffffffff);
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
            
            debug_printf("%s: rxdctl %x\n", __func__, e1000_rxdctl_82575_rd(hw_device, 0));
        } break;
        case e1000_82576:
        case e1000_I210:
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
            debug_printf("%s.%d: timeout=%d\n", __func__, __LINE__, timeout);
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
    
    debug_printf("%s: rctl:%x  rxdctl:%x\n", __func__, e1000_rctl_rd(hw_device), e1000_rxdctl_rd(hw_device, 0));
}

/*****************************************************************
 * Configure card transmit
 *
 ****************************************************************/
static void e1000_setup_tx(e1000_queue_t *device)
{
    e1000_t *hw_device = &device->hw_device;
    struct frame_identity frameid = { .base = 0, .bytes = 0 };
    struct capref frame;
    errval_t err;

    /* allocate and map frame for transmit ring */
    device->transmit_ring = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
                              sizeof(struct tx_desc) * device->transmit_buffers,
                              &frame);
    assert(device->transmit_ring);
    err = invoke_frame_identify(frame, &frameid);
    assert(err_is_ok(err));

    switch (device->mac_type) {
        case e1000_82576:
        case e1000_I210:
        case e1000_I350: {
            /* Software should program TDLEN[n] register only when queue is disabled */
            e1000_tdbal_I350_wr(hw_device, 0, frameid.base & 0xffffffff);
            e1000_tdbah_I350_wr(hw_device, 0, frameid.base >> 32);
            e1000_tdlen_I350_len_wrf(hw_device, 0, (device->transmit_buffers / 8));
            e1000_tdh_I350_wr(hw_device, 0, 0);
            e1000_tdt_I350_wr(hw_device, 0, 0);
        } break;
        default: {
            /* tell card about our transmit ring */
            e1000_tdbal_wr(hw_device, 0, frameid.base & 0xffffffff);
            e1000_tdbah_wr(hw_device, 0, frameid.base >> 32);
            e1000_tdlen_len_wrf(hw_device, 0, (device->transmit_buffers / 8));
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

    if (device->mac_type == e1000_82576 || device->mac_type == e1000_I210 || device->mac_type == e1000_I350) {
            /* Poll the TXDCTL register until the ENABLE bit is set. */
            int timeout = 1000;
            while(!e1000_txdctl_I350_enable_rdf(hw_device, 0) && timeout--) {
                // usec_delay(10);
            }
            debug_printf("%s.%d: timeout=%d\n", __func__, __LINE__, timeout);
            // if (timeout <= 0) {
            //     E1000_DEBUG("ERROR: failed to enable the TX queue\n");
            // }
    }
    debug_printf("%s: tctl:%x  txdctl:%x\n", __func__, e1000_tctl_rd(hw_device), e1000_txdctl_rd(hw_device, 0));
}

// static void e1000_timer(void *arg)
// {
//     e1000_queue_t *device = arg;
//     e1000_t *hw_device = &device->hw_device;
//
//     debug_printf("%x %x  %x %x\n", e1000_ctrl_rd(hw_device), e1000_status_rd(hw_device), e1000_icr_rd(hw_device), e1000_ims_rd(hw_device));
// }

/*****************************************************************
 * PCI init callback.
 *
 * Setup device, create receive ring and connect to Ethernet server.
 *
 ****************************************************************/
static void e1000_init_fn(void *user_state, struct device_mem *bar_info,
                          int nr_allocated_bars)
{
    e1000_queue_t *device = user_state;
    uint64_t mac_addr;
    errval_t err;

    err = map_device(&bar_info[0]);
    assert(err_is_ok(err));

    e1000_initialize(&device->hw_device, (void *) bar_info[0].vaddr);

    device->mac_type = e1000_get_mac_type(device->pci_vendor, device->pci_deviceid);
    debug_printf("%s: mac_type is: %s\n", __func__, e1000_mac_type_to_str(device->mac_type));
    
    // set features
    switch (device->mac_type) {
        case e1000_82571:
        case e1000_82572:
        case e1000_82574: {
            device->extended_interrupts = 0;
            device->advanced_descriptors = 1;
        } break;
        case e1000_82576:
        case e1000_I210:
        case e1000_I350: {
            device->extended_interrupts = 1;
            device->advanced_descriptors = 3;
        } break;
    default:
        device->extended_interrupts = 0;
        device->advanced_descriptors = 0;
    }

    e1000_reset(device);
    e1000_setup_mac(device, &mac_addr);
    e1000_setup_link(device);
    e1000_setup_rx(device);
    e1000_setup_tx(device);

    /* Enable interrupts */
    if (device->interrupt_mode) {
        e1000_enable_interrupts(device);
    }

    // periodic_event_create(&device->timer, get_default_waitset(),
    //                            500000, MKCLOSURE(e1000_timer, device));
}

/*****************************************************************
 * Check link connection status.
 *
 ****************************************************************/
static bool e1000_check_link_up(e1000_queue_t *device)
{
    e1000_t *hw_device = &device->hw_device;
    e1000_status_t status = e1000_status_rd(hw_device);

    if (e1000_status_lu_extract(status)) {
        return true;
    }
    return false;
}

static void e1000_interrupt_handler(void *arg)
{
    e1000_queue_t *device = arg;
    e1000_t *hw_device = &device->hw_device;
    /* Read interrupt cause, this also acknowledges the interrupt */
    e1000_intreg_t icr = 0;
    e1000_eintreg_t eicr = 0;
    
    if (device->extended_interrupts) { // mask interrupts
        eicr = e1000_eicr_rd(hw_device);
    } else {
        e1000_intreg_t intreg = 0;
        /* Activate link change interrupt */
        intreg = e1000_intreg_lsc_insert(intreg, 1);
        /* Activate rx0 interrupt */
        intreg = e1000_intreg_rxt0_insert(intreg, 1);
        intreg = e1000_intreg_txdw_insert(intreg, 1);
        e1000_imc_wr(hw_device, intreg);
    }
    icr = e1000_icr_rd(hw_device);

    if (e1000_intreg_lsc_extract(icr) != 0) {
        if (e1000_check_link_up(device)) {
            debug_printf("%s: link up\n", device->name);
        } else {
            debug_printf("%s: link down\n", device->name);
        }
    }
    device->isr(device);
    if (device->extended_interrupts) { // unmask interrupts
        e1000_eintreg_t eintreg = 0;
        eintreg = e1000_eintreg_rxtxq0_insert(eintreg, 1);
        eintreg = e1000_eintreg_rxtxq1_insert(eintreg, 1);

        e1000_eims_wr(hw_device, eintreg);
    } else {
        e1000_intreg_t intreg = 0;
        /* Activate link change interrupt */
        intreg = e1000_intreg_lsc_insert(intreg, 1);
        /* Activate rx0 interrupt */
        intreg = e1000_intreg_rxt0_insert(intreg, 1);
        intreg = e1000_intreg_txdw_insert(intreg, 1);
        e1000_ims_wr(hw_device, intreg);
    }
}

static void e1000_reregister_handler(void *arg)
{
    e1000_queue_t *device = arg;
    errval_t err;
    uint32_t class = PCI_CLASS_ETHERNET;
    uint32_t subclass = PCI_DONT_CARE;
    uint32_t program_interface = PCI_DONT_CARE;
    
    printf("%s:%s:%d:\n", __FILE__, __FUNCTION__, __LINE__);
    err = pci_reregister_irq_for_device(
            class, subclass, program_interface,
            device->pci_vendor, device->pci_deviceid, device->pci_bus,
            device->pci_device, device->pci_function,
            e1000_interrupt_handler, device,
            e1000_reregister_handler, device);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pci_reregister_irq_for_device");
    }

    return;
}

static void e1000_init(e1000_queue_t *device, unsigned interrupt_mode)
{
    errval_t err;
    uint32_t class = PCI_CLASS_ETHERNET;
    uint32_t subclass = PCI_DONT_CARE;
    uint32_t program_interface = PCI_DONT_CARE;

    err = pci_client_connect();
    assert(err_is_ok(err));

    if (interrupt_mode) { // 1 or 2
        err = pci_register_driver_movable_irq(e1000_init_fn, device, class,
                                              subclass, program_interface,
                                              device->pci_vendor, device->pci_deviceid,
                                              device->pci_bus, device->pci_device,
                                              device->pci_function,
                                              e1000_interrupt_handler, device,
                                              e1000_reregister_handler, device);
        printf("########### Driver with interrupts ###########\n");
    } else {
        err = pci_register_driver_noirq(e1000_init_fn, device, class, subclass,
                                        program_interface, device->pci_vendor,
                                        device->pci_deviceid, device->pci_bus,
                                        device->pci_device, device->pci_function);
        printf("########### Driver without interrupts ###########\n");
    }
}

errval_t e1000_queue_create(e1000_queue_t ** q, uint32_t vendor, uint32_t deviceid,
    uint32_t bus, uint32_t pci_device, uint32_t function, unsigned interrupt_mode,
    void (*isr)(void *))
{
    errval_t err;
    e1000_queue_t *device;
    
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

    device->receive_buffers = DRIVER_RECEIVE_BUFFERS;
    device->transmit_buffers = DRIVER_TRANSMIT_BUFFERS;

    e1000_init(device, interrupt_mode);

    err = devq_init(&device->q, false);
    assert(err_is_ok(err));
    
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

e1000_mac_type_t e1000_get_mac_type(uint32_t vendor, uint32_t device_id)
{
    if (vendor == PCI_VENDOR_INTEL) {

        switch (device_id) {
        case E1000_DEVICE_82542:
            return e1000_82542;
        case E1000_DEVICE_82543GC_FIBER:
        case E1000_DEVICE_82543GC_COPPER:
            return e1000_82543;
        case E1000_DEVICE_82544EI_COPPER:
        case E1000_DEVICE_82544EI_FIBER:
        case E1000_DEVICE_82544GC_COPPER:
        case E1000_DEVICE_82544GC_LOM:
            return e1000_82544;
        case E1000_DEVICE_82540EM:
        case E1000_DEVICE_82540EM_LOM:
        case E1000_DEVICE_82540EP:
        case E1000_DEVICE_82540EP_LOM:
        case E1000_DEVICE_82540EP_LP:
            return e1000_82540;
        case E1000_DEVICE_82545EM_COPPER:
        case E1000_DEVICE_82545EM_FIBER:
            return e1000_82545;
        case E1000_DEVICE_82545GM_COPPER:
        case E1000_DEVICE_82545GM_FIBER:
        case E1000_DEVICE_82545GM_SERDES:
            return e1000_82545_rev_3;
        case E1000_DEVICE_82546EB_COPPER:
        case E1000_DEVICE_82546EB_FIBER:
        case E1000_DEVICE_82546EB_QUAD_COPPER:
            return e1000_82546;
        case E1000_DEVICE_82546GB_COPPER:
        case E1000_DEVICE_82546GB_FIBER:
        case E1000_DEVICE_82546GB_SERDES:
        case E1000_DEVICE_82546GB_PCIE:
        case E1000_DEVICE_82546GB_QUAD_COPPER:
        case E1000_DEVICE_82546GB_QUAD_COPPER_KSP3:
            return e1000_82546_rev_3;
        case E1000_DEVICE_82541EI:
        case E1000_DEVICE_82541EI_MOBILE:
        case E1000_DEVICE_82541ER_LOM:
            return e1000_82541;
        case E1000_DEVICE_82541ER:
        case E1000_DEVICE_82541GI:
        case E1000_DEVICE_82541GI_LF:
        case E1000_DEVICE_82541GI_MOBILE:
            return e1000_82541_rev_2;
        case E1000_DEVICE_82547EI:
        case E1000_DEVICE_82547EI_MOBILE:
            return e1000_82547;
        case E1000_DEVICE_82547GI:
            return e1000_82547_rev_2;
        case E1000_DEVICE_82563EB:
            return e1000_82563;
        case E1000_DEVICE_82571EB_COPPER:
        case E1000_DEVICE_82571EB_FIBER:
        case E1000_DEVICE_82571EB_SERDES:
        case E1000_DEVICE_82571EB_SERDES_DUAL:
        case E1000_DEVICE_82571EB_SERDES_QUAD:
        case E1000_DEVICE_82571EB_QUAD_COPPER:
        case E1000_DEVICE_82571EB_QUAD_FIBER:
        case E1000_DEVICE_82571EB_QUAD_COPPER_LOWPROFILE:
            return e1000_82571;
        case E1000_DEVICE_82572EI_COPPER:
        case E1000_DEVICE_82572EI_FIBER:
        case E1000_DEVICE_82572EI_SERDES:
        case E1000_DEVICE_82572EI:
            return e1000_82572;
        case E1000_DEVICE_82573E:
        case E1000_DEVICE_82573E_IAMT:
        case E1000_DEVICE_82573L:
            return e1000_82573;
        case E1000_DEVICE_82574L:
            return e1000_82574;
        case E1000_DEVICE_82575EB:
            return e1000_82575;
        case E1000_DEVICE_82576EG:
            return e1000_82576;
        case E1000_DEVICE_I210:
            return e1000_I210;
        case E1000_DEVICE_I350_EEPROM_LESS:
        case E1000_DEVICE_I350_COPPER:
        case E1000_DEVICE_I350_FIBER:
        case E1000_DEVICE_I350_BACKPANE:
        case E1000_DEVICE_I350_SGMII:
        case E1000_DEVICE_I350_DUMMY:
            return e1000_I350;
        default:
            // E1000_DEBUG("Unsupported device: vendor: 0x%x,  device id: 0x%x\n", PCI_VENDOR_INTEL, device_id);
            return e1000_undefined;
        }
    }

    return e1000_undefined;
}

char * e1000_mac_type_to_str(e1000_mac_type_t mt){
    char * names[] = {
        "undefined",
        "82542",
        "82543",
        "82544",
        "82540",
        "82545",
        "82545_rev_3",
        "82546",
        "82546_rev_3",
        "82541",
        "82541_rev_2",
        "82547",
        "82547_rev_2",
        "82563",
        "82571",
        "82572",
        "82573",
        "82574",
        "82575",
        "82576",
        "I210",
        "I350"
    };
    if(mt >= e1000_num_macs) return NULL;
    return names[mt];
};
