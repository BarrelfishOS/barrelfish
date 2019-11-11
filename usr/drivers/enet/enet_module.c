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
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>
#include <dev/enet_dev.h>

#define ENET_DEBUG(x...) debug_printf("[enet] " x);

#define TX_RING_SIZE 512
#define ENET_RX_FRSIZE 2048
#define ENET_RX_PAGES 256

#define RX_RING_SIZE (BASE_PAGE_SIZE / ENET_RX_FRSIZE) * ENET_RX_PAGES

#define ENET_SC_WRAP ((ushort)0x2000)

struct enet_rx_queue {
    struct devq q;
    size_t size;
    struct dmem desc_mem;

    // hd + tail
    size_t head;
    size_t tail;

    // Descriptor + Cleanq
    enet_bufdesc_array_t *ring;
    struct devq_buf *ring_bufs;
};

struct enet_tx_queue {    
    struct devq q;
    size_t size;
    // stop and wake threashold
    uint16_t stop_th; 
    uint16_t wake_th;
    char* tso_hdr;
    struct dmem desc_mem;

    // hd + tail
    size_t head;
    size_t tail;

    // Descriptor + Cleanq
    enet_bufdesc_array_t *ring;
    struct devq_buf *ring_bufs;
};

struct enet_driver_state {
    struct bfdriver_instance *bfi;
    struct capref regs;
    lvaddr_t d_vaddr;

    struct enet_rx_queue* rxq;
    struct enet_tx_queue* txq;
    enet_t* d;
    uint64_t mac;
};



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

static errval_t enet_rings_start(struct enet_driver_state* st) 
{   
    // tell the card the start of the RX descriptor ring
    enet_rdsr_wr(st->d, st->rxq->desc_mem.devaddr);
    
    // Normally 2048er buffs but the alignment constraints are worst case 64
    enet_mrbr_wr(st->d, 2048-64);
    
    // RX: For other queues would be possible to enable DMA here (1+2)
    // Not needed for queue 0
    
    // tell the card the start of the TX descriptor ring
    enet_tdsr_wr(st->d, st->txq->desc_mem.devaddr);
        
    // TX: For other queues would be possible to enable DMA here (1+2)
    // Not needed for queue 0

    errval_t err = SYS_ERR_OK;
    return err;
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
    ENET_DEBUG("Reste device\n");
    enet_ecr_wr(st->d, 0);

    // Write back mac again
    ENET_DEBUG("Reset MAC\n");
    enet_write_mac(st);
    enet_read_mac(st);
    

    // Clear outstanding interrupts
    ENET_DEBUG("Clear outstanding interrupts\n");
    enet_eir_wr(st->d, 0xFFFFFFFF);
        
    //Initalize descriptor rings
    ENET_DEBUG("Reset RX/TX rings\n");
    err = enet_rings_init(st);
    if (err_is_fail(err)) {
        ENET_DEBUG("Failed init of rings \n");
        return err;
    }

    ENET_DEBUG("Restart RX/TX rings\n");
    err = enet_rings_start(st);
    if (err_is_fail(err)) {
        ENET_DEBUG("Failed starting rings \n");
        return err;
    }

    // set full duplex mod3
    enet_tcr_fden_wrf(st->d, 1);
    
    // TODO check these vlaues: set by using register dump on linux
    // set MII speed
    enet_mscr_mii_speed_wrf(st->d, 1);
    // set hold time 
    enet_mscr_hold_time_wrf(st->d, 0);
    

    // TODO check if this is verion M5272 then have to fix some quirks here

    // ENET_MAC is special here!
    // flow control and length check
    enet_rcr_fce_wrf(st->d, 1);
    enet_rcr_nlc_wrf(st->d, 1);
    // Only have RMII (Fast ethernet)
    enet_rcr_rmii_mode_wrf(st->d, 1);
    enet_rcr_rmii_10t_wrf(st->d, 0);
    
    // Assume this is not M5272
    // FIFO threshold parameter to reduce overrun
    enet_rsem_rx_section_empty_wrf(st->d, 0x84);
    enet_rsfl_rx_section_full_wrf(st->d, 16);
    enet_raem_rx_almost_full_wrf(st->d, 8);
    enet_rafl_rx_almost_empty_wrf(st->d, 8);
    enet_opd_pause_dur_wrf(st->d, 0xFFF0);
    
    // TODO setup multicast filter line 1087
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
    st->rxq = calloc(1, sizeof(struct enet_tx_queue));
    st->txq = calloc(1, sizeof(struct enet_tx_queue));
    st->rxq->size = RX_RING_SIZE;
    st->txq->size = TX_RING_SIZE;
    assert(st->rxq);
    assert(st->txq);


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
