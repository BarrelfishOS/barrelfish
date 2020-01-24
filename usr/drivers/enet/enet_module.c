/**
 * \file
 * \brief imx8 NIC driver module
 */
/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <devif/queue_interface_backend.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>
#include <dev/enet_dev.h>

#include "enet.h"


static struct region_entry* get_region(struct enet_queue* q, regionid_t rid)
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

static errval_t enet_register(struct devq* q, struct capref cap, regionid_t rid)
{
    errval_t err;
    struct enet_queue* queue = (struct enet_queue*) q;

    // keep track of regions since we need the virtual address ...
    struct region_entry* entry = calloc(1, sizeof(struct region_entry));
    assert(entry);
    entry->rid = rid;
    entry->next = NULL;
    
    struct frame_identity id;
    err = frame_identify(cap, &id);
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

    entry->mem.devaddr = id.base;
    entry->mem.vbase = (lvaddr_t) va;
    entry->mem.mem = cap;
    entry->mem.size = id.bytes;
    
    ENET_DEBUG("register region id %d base=%lx \n", rid, entry->mem.devaddr);
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

    ENET_DEBUG("registerd region id %d base=%p len=%ld \n", rid, 
                (void*) entry->mem.vbase, entry->mem.size);
    return SYS_ERR_OK;
}

static errval_t enet_rx_dequeue(struct devq* que, regionid_t* rid,
                                genoffset_t* offset,
                                genoffset_t* length,
                                genoffset_t* valid_data,
                                genoffset_t* valid_length,
                                uint64_t* flags)
{
    struct enet_queue* q = (struct enet_queue*) que;      
    enet_bufdesc_t desc = q->ring[q->head];
    struct devq_buf buf = q->ring_bufs[q->head];

    uint16_t status = enet_bufdesc_sc_extract(desc);
    ENET_DEBUG("SC %lx \n", enet_bufdesc_sc_extract(desc));
    if (!(status & ENET_RX_EMPTY)) {
        // TODO error handling!
        *valid_length = enet_bufdesc_len_extract(desc);
        ENET_DEBUG("Received Packet %lu \n", *valid_length);
        *offset = buf.offset;
        *valid_data = 0;
        *rid = buf.rid;
        *flags = buf.flags;
    } else {
        return DEVQ_ERR_QUEUE_EMPTY;
    }
    status &= ~ENET_RX_STATS;
    status |= ENET_RX_EMPTY;
    // TODO this might already had back ownership!! 
    
    enet_bufdesc_sc_insert(desc, status);
    
    q->head = q->head & (q->size -1);

    return  SYS_ERR_OK;
}

static errval_t enet_tx_dequeue(struct devq* que, regionid_t* rid,
                                genoffset_t* offset,
                                genoffset_t* length,
                                genoffset_t* valid_data,
                                genoffset_t* valid_length,
                                uint64_t* flags)
{
    //struct enet_queue* q = (struct enet_queue*) que;      
    //enet_bufdesc_t desc = q->ring[q->head];
    return  SYS_ERR_OK;
}

static errval_t enet_tx_enqueue(struct devq* que, regionid_t rid, genoffset_t offset,
                                genoffset_t length, genoffset_t valid_data,
                                genoffset_t valid_length, uint64_t flags)
{

    return SYS_ERR_OK;
}
static errval_t enet_rx_enqueue(struct devq* que, regionid_t rid, genoffset_t offset,
                                genoffset_t length, genoffset_t valid_data,
                                genoffset_t valid_length, uint64_t flags)
{
    struct enet_queue* q = (struct enet_queue*) que;   
    //enet_bufdesc_addr_insert(desc, );
    struct region_entry *entry = get_region(q, rid);
    assert(entry);    
    
    // TODO SEE IF THERE IS SPACE!
   
    lpaddr_t addr = 0;
    addr = (lpaddr_t) entry->mem.devaddr + offset;
 
    struct devq_buf buf= q->ring_bufs[q->tail];
    buf.offset = offset;
    buf.length = length;
    buf.valid_length = valid_length;
    buf.valid_data = valid_data;
    buf.rid = rid;
    buf.flags = flags;
   
    enet_bufdesc_t desc = q->ring[q->tail];
    enet_bufdesc_addr_insert(desc, addr);

    if (q->tail == (q->size -1)) {
        enet_bufdesc_sc_insert(desc, ENET_SC_WRAP | ENET_RX_EMPTY);
    } else {
        enet_bufdesc_sc_insert(desc, ENET_RX_EMPTY);
    }

    //ENET_DEBUG("enqueue offset=%lx length=%zu\n", offset, length);
    q->tail = (q->tail + 1) & (q->size -1);
    return SYS_ERR_OK;
}


__attribute__((unused)) static errval_t enet_write_mdio(struct enet_driver_state* st, uint8_t phyaddr,
                                uint8_t regaddr, uint16_t data)
{
    
    // Some protocol ...

    // TODO Need + 40 registeR!!!
    enet_mmfr_t reg;
    enet_mmfr_pa_insert(reg, phyaddr);
    enet_mmfr_ra_insert(reg, regaddr);
    enet_mmfr_data_insert(reg, data);   
    enet_mmfr_st_insert(reg, 0x1);   
    enet_mmfr_ta_insert(reg, 0x1);   

    // 1 is write 2 is read
    enet_mmfr_op_insert(reg, 0x1);   

    enet_mmfr_wr(st->d, reg);

    uint16_t tries = 1000;
    while (!(enet_eir_mii_rdf(st->d) & 0x1)) {
        barrelfish_usleep(10);
        tries--;
        if (tries == 0) {
            return ENET_ERR_MDIO_WRITE;
        }
    }
    
    enet_eir_mii_wrf(st->d, 0x1);
    return SYS_ERR_OK;
}

__attribute__((unused)) static errval_t enet_read_mdio(struct enet_driver_state* st, uint8_t phyaddr,
                                uint8_t regaddr, uint16_t *data)
{
    
    // Some protocol ...
    enet_eir_mii_wrf(st->d, 0x1);

    // TODO Need + 40 registeR!!!
    enet_mmfr_t reg;
    enet_mmfr_pa_insert(reg, phyaddr);
    enet_mmfr_ra_insert(reg, regaddr);
    enet_mmfr_st_insert(reg, 0x1);   
    enet_mmfr_ta_insert(reg, 0x1);   

    // 1 is write 2 is read
    enet_mmfr_op_insert(reg, 0x2);   

    enet_mmfr_wr(st->d, reg);

    uint16_t tries = 1000;
    while (!(enet_eir_mii_rdf(st->d) & 0x1)) {
        barrelfish_usleep(10);
        tries--;
        if (tries == 0) {
            return ENET_ERR_MDIO_WRITE;
        }
    }
    
    enet_eir_mii_wrf(st->d, 0x1);
    *data = enet_mmfr_data_rdf(st->d);
    
    return SYS_ERR_OK;
}

// bool promiscous for promiscous mode. 
// This will also set it so that all multicast packets will also be received!
static void enet_init_multicast_filt(struct enet_driver_state* st, bool promisc)
{
    if (promisc) {
        enet_rcr_prom_wrf(st->d, 1);
        return;
    }

    enet_rcr_prom_wrf(st->d, 0);
    
    // TODO Catching all multicast packets for now
    enet_gaur_gaddr_wrf(st->d, 0xFFFFFFFF);
    enet_galr_gaddr_wrf(st->d, 0xFFFFFFFF);
    // TODO if we do not catch all multicast packet then do this:
    // crc32 value of mac address
    /*
    unsigned int crc = 0xffffffff;
    unsigned char hash;
    unsigned int hash_high = 0, hash_low = 0;
    for (int i = 0; i < 6; i++) {
        unsigned char data = ((uint8_t*) &st->mac)[i];

        for (int bit = 0; bit < 8; bit++, data >>= 1) {
            crc = (crc >> 1) ^ (((crc ^ data) & 1) ? ENET_CRC32_POLY : 0);  
        }
        
        hash = (crc >> (32 - ENET_HASH_BITS)) & 0x3f;  
        
        if (hash > 31) {
            hash_high |= 1 << (hash - 32);
        } else {
            hash_low |= 1 << hash;
        }
    } 
  
    enet_gaur_gaddr_wrf(st->d, hash_high);
    enet_galr_gaddr_wrf(st->d, hash_low);
    */
    // TODO if this is M5272 then set the hash table entries to 0 ...
}

static void enet_read_mac(struct enet_driver_state* st)
{
    uint64_t lower = enet_palr_paddr1_rdf(st->d);
    uint64_t upper = enet_paur_paddr2_rdf(st->d);
    // this is weird lower seems to be the upper part of the address ..
    uint64_t mac = (lower << 16) | upper;

    ENET_DEBUG("MAC %lx \n", mac);
    st->mac = mac;  
}

static void enet_write_mac(struct enet_driver_state* st)
{
    
    uint64_t lower = st->mac >> 16;
    uint32_t upper = st->mac & 0xFFFF;

    enet_palr_paddr1_wrf(st->d, lower);
    enet_paur_paddr2_wrf(st->d, upper);
}

static void enet_activate_rx_ring(struct enet_driver_state* st)
{
    // bit is always set to 1 only when ring is empty then it is set to 0
   enet_radr_radr_wrf(st->d, 0); 
}

static errval_t enet_rings_start(struct enet_driver_state* st) 
{   
    // tell the card the start of the RX descriptor ring
    enet_rdsr_wr(st->d, st->rxq->desc_mem.devaddr);
    
    // Normally 2048er buffs but the alignment constraints are worst case 64
    enet_mrbr_wr(st->d, ROUND_DOWN(2048-64, 64));
    
    // RX: For other queues would be possible to enable DMA here (1+2)
    // Not needed for queue 0
    
    // tell the card the start of the TX descriptor ring
    enet_tdsr_wr(st->d, st->txq->desc_mem.devaddr);
        
    // TX: For other queues would be possible to enable DMA here (1+2)
    // Not needed for queue 0

    errval_t err = SYS_ERR_OK;
    return err;
}

static void enet_init_mii(struct enet_driver_state* st)
{
    // TODO check these vlaues: set by using register dump on linux
    // set MII speed
    enet_mscr_mii_speed_wrf(st->d, 1);
    // set hold time 
    enet_mscr_hold_time_wrf(st->d, 0);
}

static errval_t enet_rings_init(struct enet_driver_state* st)
{
    // init receive buffer descriptors
    enet_bufdesc_t desc;
    uint16_t val;

    ENET_DEBUG("RX %p ring init to default values \n", st->rxq->ring);
    for (int i = 0; i < st->rxq->size; i++) {
        desc = st->rxq->ring[i];
        enet_bufdesc_sc_insert(desc, 0);
    }

    // set last one to wrap
    desc = st->rxq->ring[st->rxq->size - 1];
    val = enet_bufdesc_sc_extract(desc);
    val |= 0x2000;
    enet_bufdesc_sc_insert(desc, val);
    st->rxq->head = 0;
    st->rxq->tail = 0;
    
    ENET_DEBUG("TX %p ring init to default values \n", st->txq->ring);
    // init send buffer descriptors
    for (int i = 0; i < st->txq->size; i++) {
        desc = st->txq->ring[i];
        enet_bufdesc_sc_insert(desc, 0);
        enet_bufdesc_addr_insert(desc, 0);
    }

    // set last one to wrap
    desc = st->txq->ring[st->txq->size - 1];
    val = enet_bufdesc_sc_extract(desc);
    val |= 0x2000;
    enet_bufdesc_sc_insert(desc, val);

    st->txq->head = 0;
    st->txq->tail = 0;

    return SYS_ERR_OK;
}

static errval_t enet_restart(struct enet_driver_state* st)
{
    errval_t err;
    // reset device
    ENET_DEBUG("Reset device\n");
    
    uint64_t ecr = enet_ecr_rd(st->d);
    enet_ecr_wr(st->d, ecr | 0x1);
    while (enet_ecr_rd(st->d) & 0x1) {
        barrelfish_usleep(10);
        // TODO timeout
    }

    enet_eimr_wr(st->d, 0);
    // Write back mac again
    ENET_DEBUG("Reset MAC\n");
    // TODO do this later? NOT in dump
    enet_write_mac(st);
    enet_read_mac(st);
    

    // Clear outstanding interrupts
    ENET_DEBUG("Clear outstanding interrupts\n");
    enet_eir_wr(st->d, 0xFFFFFFFF);
        
    //Initalize descriptor rings
    ENET_DEBUG("Reset RX/TX rings\n");
    err = enet_rings_init(st);
    if (err_is_fail(err)) {
        debug_printf("Failed init of rings \n");
        return err;
    }

    ENET_DEBUG("Restart RX/TX rings\n");
    err = enet_rings_start(st);
    if (err_is_fail(err)) {
        debug_printf("Failed starting rings \n");
        return err;
    }

    // set max pkt size
    enet_rcr_max_fl_wrf(st->d, 1522);

    // set full duplex mode
    // TODO check if needed?
    //enet_tcr_fden_wrf(st->d, 1);
    
    // TODO check these vlaues: set by using register dump on linux
    // set MII speed
    //enet_mscr_mii_speed_wrf(st->d, 1);a
    // Set MII speed, do not drop preamble and set hold time to 10ns
    enet_mscr_mii_speed_wrf(st->d, 0x18);
    enet_mscr_hold_time_wrf(st->d, 0x1);
    // set hold time 
    //enet_mscr_hold_time_wrf(st->d, 0); TODO implication of hold time!

    // TODO check if this is verion M5272 then have to fix some quirks here

    // ENET_MAC is special here!
    // flow control and length check
    //enet_rcr_fce_wrf(st->d, 1);
    //enet_rcr_nlc_wrf(st->d, 1);
    // Only have RMII (Fast ethernet)
    //enet_rcr_rmii_mode_wrf(st->d, 1);
    //enet_rcr_rmii_10t_wrf(st->d, 0);
    
    // Assume this is not M5272
    // FIFO threshold parameter to reduce overrun
    enet_rsem_rx_section_empty_wrf(st->d, 0x84);
    enet_rsfl_rx_section_full_wrf(st->d, 16);
    enet_raem_rx_almost_full_wrf(st->d, 8);
    enet_rafl_rx_almost_empty_wrf(st->d, 8);
    enet_opd_pause_dur_wrf(st->d, 0xFFF0);
    
    // TODO setup multicast filter line 1087
#ifdef ENET_PROMISC
    ENET_DEBUG("Setting multicast filter promiscous mode \n");
    enet_init_multicast_filt(st, true);
#else
    ENET_DEBUG("Setting multicast filter NOT in promiscous mode \n");
    enet_init_multicast_filt(st, false);
#endif

    // Enable enet store forward mode (ENET quirk)
    enet_tfwr_strfwd_wrf(st->d, 1);

    // TODO enable advanced descriptors
    //enet_ecr_en1588_wrf(st->d, 1);
    
    enet_ecr_dbswp_wrf(st->d, 1);
    enet_ecr_speed_wrf(st->d, 0);
    enet_ecr_etheren_wrf(st->d, 1);


    ENET_DEBUG("Activate RX ring \n");
    enet_activate_rx_ring(st);

    ENET_DEBUG("Set interrupts we want to handle \n");
    // Set interrupts we want to handle
    enet_eimr_mii_wrf(st->d, 1);
    enet_eimr_txf_wrf(st->d, 1);
    enet_eimr_rxf_wrf(st->d, 1); 
    enet_eimr_txf1_wrf(st->d, 1);
    enet_eimr_rxf1_wrf(st->d, 1);
    enet_eimr_txf2_wrf(st->d, 1);
    enet_eimr_rxf2_wrf(st->d, 1);

    ENET_DEBUG("TODO: Enable interrupts\n");
    // TODO enable interrupt coalesing
    // TODO get/setup interrupt 
    return SYS_ERR_OK;
}

static errval_t enet_open(struct enet_driver_state *st)
{
    errval_t err;
    // TODO fec_enet_alloc_buffers? needed?
    err = enet_restart(st);
    if (err_is_fail(err)) {
        // TODO cleanup
        return err;
    }
    return SYS_ERR_OK;
}

/**
 * Driver initialization function. This function is called by the driver domain
 * (see also 'create_handler' in ddomain_service.c).
 * Typically through a request from the device manager.
 *
 * The init function is supposed to set `dev` to the exported service iref.
 * The init function may use `bfi->dstate` to store additional state about the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \param[in]   flags Additional flags (The exact flags supported is device/driver specific).
 * \param[out]  dev   The service iref over which the device can be contacted.
 *
 * \retval SYS_ERR_OK Device initialized successfully.
 * \retval LIB_ERR_MALLOC_FAIL Unable to allocate memory for the driver.
 */
static errval_t init(struct bfdriver_instance* bfi, uint64_t flags, iref_t* dev) {
    ENET_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);
    errval_t err;

    bfi->dstate = malloc(sizeof(struct enet_driver_state));
    struct enet_driver_state * st = (struct enet_driver_state*) bfi->dstate;    
    st->bfi = bfi;

    if (bfi->dstate == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    assert(bfi->dstate != NULL);
    
    err = driverkit_get_bar_cap(st->bfi, 0, &st->regs);
    if (err_is_fail(err)) {
        USER_PANIC("pcid_get_bar_cap failed \n");
    }

    err = map_device_cap(st->regs, &st->d_vaddr);
    if (err_is_fail(err)) {
        USER_PANIC("map_device_cap failed \n");
    }

    
    /* Initialize Mackerel binding */
    st->d = (enet_t *) malloc(sizeof(enet_t));
    enet_initialize(st->d, (void *) st->d_vaddr);

    assert(st->d != NULL);
    enet_read_mac(st);
    /*
    lower = enet_mac_low_addr_rdf(st->d);
    upper = enet_mac_high_addr_rdf(st->d);
    // this is weird lower seems to be the upper part of the address ..
    mac = (lower << 16) | upper;

    ENET_DEBUG("MAC lower %lx \n", lower);
    ENET_DEBUG("MAC upper %lx \n", upper);
    ENET_DEBUG("MAC %lx \n", mac);
    */
    // TODO rest phy devm_gpio_request_one()? 
    
    // TODO Alloc queue struts 
    ENET_DEBUG("Allocating TX/RX ring data structures \n");
    st->rxq = calloc(1, sizeof(struct enet_queue));
    st->txq = calloc(1, sizeof(struct enet_queue));
    st->rxq->size = RX_RING_SIZE;
    st->txq->size = TX_RING_SIZE;
    assert(st->rxq);
    assert(st->txq);

    st->rxq->align = 0x3f;
    st->txq->align = 0;

    // TODO check for advanced descriptors
    // TODO linking iommu driverkit library does not seem to work ...
    ENET_DEBUG("Allocating RX/TX descriptor ring \n");
    size_t tot_size = (st->rxq->size + st->txq->size)*sizeof(enet_bufdesc_t);
    err = frame_alloc(&(st->rxq->desc_mem.mem), tot_size, (size_t *)&(st->rxq->desc_mem.size));
    if (err_is_fail(err)) {
        return err;
    }

    ENET_DEBUG("Mapping RX/TX descriptor ring\n");
    err = vspace_map_one_frame_attr((void**) &(st->rxq->desc_mem.vbase), tot_size, 
                                    st->rxq->desc_mem.mem, 
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(st->rxq->desc_mem.mem);
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        return err;
    }

    struct frame_identity id;
    err = frame_identify(st->rxq->desc_mem.mem, &id);
    if (err_is_fail(err)) {
        return err;
    }

    st->rxq->desc_mem.devaddr = id.base;

    /*
    err = driverkit_iommu_mmap_cl(NULL, st->rxq->size + st->txq->size*
                                  sizeof(enet_bufdesc_t), VREGION_FLAGS_READ_WRITE, 
                                  &st->rxq->desc_mem);
    if (err_is_fail(err)) {
        // TODO cleanup
        return DRIVERKIT_ERR_DRIVER_INIT;
    }
    */
    st->rxq->ring = (void*) st->rxq->desc_mem.vbase;
    assert(st->rxq->ring);
    memset(st->rxq->ring, 0, tot_size);

    st->txq->desc_mem.vbase = (st->rxq->desc_mem.vbase + (st->rxq->size*sizeof(enet_bufdesc_t)));
    st->txq->desc_mem.devaddr = (st->rxq->desc_mem.devaddr + (st->rxq->size*sizeof(enet_bufdesc_t)));
    st->txq->ring = (void*) st->txq->desc_mem.vbase;
    st->txq->desc_mem.size = st->rxq->desc_mem.size;

    assert(st->txq->ring);

    // Disable RX
    uint32_t reg_val = enet_eimr_rd(st->d);
    enet_eimr_rxf_insert(reg_val, 0);
    enet_eimr_rxf1_insert(reg_val, 0);
    enet_eimr_rxf2_insert(reg_val, 0);
    enet_eimr_wr(st->d, reg_val);
    
    ENET_DEBUG("RX phys=%p virt=%p \n", (void*) st->rxq->desc_mem.devaddr,
                (void*) st->rxq->ring);
    ENET_DEBUG("TX phys=%p virt=%p \n", (void*) st->txq->desc_mem.devaddr,
                (void*) st->txq->ring);


    // Alloc cleanq data structs
    
    st->rxq->ring_bufs = calloc(st->rxq->size, sizeof(struct devq_buf));
    st->txq->ring_bufs = calloc(st->txq->size, sizeof(struct devq_buf));
    assert(st->rxq->ring_bufs);
    assert(st->txq->ring_bufs);
    // restart NIC
    ENET_DEBUG("Restart NIC\n");
    err = enet_restart(st);
    if (err_is_fail(err)) {
        // TODO cleanup
        return err;
    }

    ENET_DEBUG("Init MII bus\n");
    enet_init_mii(st);

    err = devq_init(&st->rxq->q, false);
    if (err_is_fail(err)) {
        debug_printf("enet devq_init error\n");
        return err;
    }

    err = devq_init(&st->txq->q, false);
    if (err_is_fail(err)) {
        debug_printf("enet devq_init error\n");
        return err;
    }

    st->rxq->q.f.reg = enet_register;
    st->rxq->q.f.enq = enet_rx_enqueue;
    st->rxq->q.f.deq = enet_rx_dequeue;


    st->txq->q.f.reg = enet_register;
    st->txq->q.f.enq = enet_tx_enqueue;
    st->txq->q.f.deq = enet_tx_dequeue;

    err = enet_open(st);
    if (err_is_fail(err)) {
        // TODO cleanup
        return err;
    }

    struct capref mem;
    err = frame_alloc(&mem, 512*2048, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    regionid_t rid;
    err = devq_register((struct devq*) st->rxq, mem, &rid);
    if (err_is_fail(err)) {
        return err;
    }

    
    for (int i = 0; i < 512; i++) {
        err = devq_enqueue((struct devq*) st->rxq, rid, i*(2048), 2048,
                            0, 2048, 0);
        if (err_is_fail(err)) {
            return err;
        }
    }

    struct devq_buf buf;
    while(true) {
        err = devq_dequeue((struct devq*) st->rxq, &buf.rid, &buf.offset,
                           &buf.length, &buf.valid_data, &buf.valid_length,
                           &buf.flags);
        if (err_is_ok(err)) {
            
        } else {

        }
        for(volatile int i = 0; i < 100000000; i++) {
            i++;
            i--;
        }
    }
    *dev = 0x00;

    return SYS_ERR_OK;
}

/**
 * Instructs driver to attach to the device.
 * This function is only called if the driver has previously detached
 * from the device (see also detach).
 *
 * \note After detachment the driver can not assume anything about the
 * configuration of the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t attach(struct bfdriver_instance* bfi) {
    ENET_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    return SYS_ERR_OK;
}

/**
 * Instructs driver to detach from the device.
 * The driver must yield any control over to the device after this function returns.
 * The device may be left in any state.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t detach(struct bfdriver_instance* bfi) {
    ENET_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    return SYS_ERR_OK;
}

/**
 * Instructs the driver to go in a particular sleep state.
 * Supported states are platform/device specific.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t set_sleep_level(struct bfdriver_instance* bfi, uint32_t level) {
    ENET_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    return SYS_ERR_OK;
}

/**
 * Destroys this driver instance. The driver will yield any
 * control over the device and free any state allocated.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t destroy(struct bfdriver_instance* bfi) {
    ENET_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);
    struct enet_driver_state* uds = bfi->dstate;
    free(uds);
    bfi->dstate = NULL;

    // XXX: Tear-down the service
    bfi->device = 0x0;

    return SYS_ERR_OK;
}

static errval_t get_ep(struct bfdriver_instance* bfi, bool lmp, struct capref* ret_cap)
{
    ENET_DEBUG("Endpoint was requested \n");
  
    errval_t err = SYS_ERR_OK;  
    return err;
}

/**
 * Registers the driver module with the system.
 *
 * To link this particular module in your driver domain,
 * add it to the addModules list in the Hakefile.
 */
DEFINE_MODULE(enet_module, init, attach, detach, set_sleep_level, destroy, get_ep);
