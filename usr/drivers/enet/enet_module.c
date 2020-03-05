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

#define PHY_ID 0x2
static void some_sleep(uint64_t rep)
{    
    // TODO some sleep, barrelfish_usleep() seems to be broken!
    while(rep) {
        for(volatile int i = 0; i < 100000; i++) {
            i++;
            i--;
        }

        rep--;
    }
}

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

static inline size_t enet_full_slots(struct enet_queue* q)
{
    size_t head = q->head;
    size_t tail = q->tail;
    size_t size = q->size;

    if (tail >= head) {
        return (tail - head);
    } else {
        return (tail + size - head);
    }
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
    struct devq_buf* buf = &q->ring_bufs[q->head];

    __builtin___clear_cache(desc, desc+sizeof(enet_bufdesc_t));

    uint16_t status = enet_bufdesc_sc_extract(desc);

    ENET_DEBUG("Try dequeue %d RADR %d ENABLED %d STATUS %lx \n", q->head, 
               enet_rdar_rdar_rdf(q->d), enet_ecr_etheren_rdf(q->d), status);
    
    if (!(status & ENET_RX_EMPTY)) {
        // TODO error handling!
        *valid_length = enet_bufdesc_len_extract(desc);
        ENET_DEBUG("Received Packet %lu \n", *valid_length);
        *offset = buf->offset;
        *valid_data = 0;
        *rid = buf->rid;
        *flags = buf->flags;
    } else {
        return DEVQ_ERR_QUEUE_EMPTY;
    }
    status &= ~ENET_RX_STATS;
    //status |= ENET_RX_EMPTY;
    // TODO this might already had back ownership!! 
    
    enet_bufdesc_sc_insert(desc, status);
    
    __builtin___clear_cache(q->ring[q->head], q->ring[q->head+1]);

    q->head = (q->head+1) & (q->size -1);

    return  SYS_ERR_OK;
}

static errval_t enet_tx_dequeue(struct devq* que, regionid_t* rid,
                                genoffset_t* offset,
                                genoffset_t* length,
                                genoffset_t* valid_data,
                                genoffset_t* valid_length,
                                uint64_t* flags)
{
    struct enet_queue* q = (struct enet_queue*) que;      

    if (enet_full_slots(q)) {
        enet_bufdesc_t desc = q->ring[q->head];
        __builtin___clear_cache(q->ring[q->head], q->ring[q->head+1]);
        desc = q->ring[q->head];
        struct devq_buf* buf= &q->ring_bufs[q->head];

        if (!(enet_bufdesc_sc_extract(desc) & ENET_TX_READY)) {
            ENET_DEBUG("We sent something!! \n");
            *valid_length = buf->valid_length;
            *offset = buf->offset;
            *length = 2048;
            *valid_data = 0;
            *rid = buf->rid;
            *flags = buf->flags;
        } else {
            return DEVQ_ERR_QUEUE_EMPTY;
        }
    } else {
        return DEVQ_ERR_QUEUE_EMPTY;
    }

    ENET_DEBUG("Deq TX head=%zu \n", q->head);
    q->head = (q->head + 1) & (q->size -1);
    return SYS_ERR_OK;
}


static void enet_activate_tx_ring(enet_t * d)
{
    ENET_DEBUG("Activating TX ring \n");
    // bit is always set to 1 only when ring is empty then it is set to 0
    enet_tdar_tdar_wrf(d, 1); 
}

static errval_t enet_tx_enqueue(struct devq* que, regionid_t rid, genoffset_t offset,
                                genoffset_t length, genoffset_t valid_data,
                                genoffset_t valid_length, uint64_t flags)
{
    
    struct enet_queue* q = (struct enet_queue*) que;   

    assert(valid_length > 0 && valid_length < ENET_MAX_PKT_SIZE);

    lpaddr_t addr = 0;
    lvaddr_t vaddr = 0;
    struct region_entry *entry = get_region(q, rid);
    assert(entry);    
    addr = (lpaddr_t) entry->mem.devaddr + offset + valid_data;
    vaddr = (lvaddr_t) entry->mem.vbase + offset + valid_data;
    
    struct devq_buf* buf= &q->ring_bufs[q->tail];
    buf->offset = offset;
    buf->length = length;
    buf->valid_length = valid_length;
    buf->valid_data = valid_data;
    buf->rid = rid;
    buf->flags = flags;
 
    // TODO alignment
    
    enet_bufdesc_t desc = q->ring[q->tail];
    enet_bufdesc_addr_insert(desc, addr);
    enet_bufdesc_len_insert(desc, valid_length);

    __builtin___clear_cache((void*) vaddr, (void*) vaddr+valid_length);

    if (q->tail == (q->size -1)) {
        enet_bufdesc_sc_insert(desc, ENET_TX_READY | ENET_TX_CRC | 
                               ENET_TX_LAST | ENET_TX_WRAP);
    } else {
        enet_bufdesc_sc_insert(desc, ENET_TX_READY | ENET_TX_CRC | ENET_TX_LAST);
    }

    __builtin___clear_cache(q->ring[q->tail], q->ring[q->tail+1]);
    // activate TX
    enet_activate_tx_ring(q->d);

    // wait until sent
    int timeout = 5000;
    while(timeout--) {
        if (!(enet_tdar_tdar_rdf(q->d))) {
            break;
        }
    }
 
    if (timeout == 0) {
        debug_printf("Failed sending!! \n");
        return NIC_ERR_TX_PKT;
    }

    timeout = 5000;
    // make sure it is really sent!!
    while(timeout--) {
        __builtin___clear_cache(q->ring[q->tail], q->ring[q->tail+1]);
        desc = q->ring[q->tail];
        if (!(enet_bufdesc_sc_extract(desc) & ENET_TX_READY)) {
            break;
        }
    }

    debug_printf("Descriptor %lx \n", desc);
    if (timeout == 0) {
        debug_printf("Failed sending!! \n");
        return NIC_ERR_TX_PKT;
    } else {
        q->tail = (q->tail + 1) & (q->size -1);
    }

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
    enet_bufdesc_len_insert(desc, 0);

    if (q->tail == (q->size -1)) {
        enet_bufdesc_sc_insert(desc, ENET_SC_WRAP | ENET_RX_EMPTY);
    } else {
        enet_bufdesc_sc_insert(desc, ENET_RX_EMPTY);
    }

    __builtin___clear_cache(q->ring[q->tail], q->ring[q->tail+1]);
    /*ENET_DEBUG("enqueue ring_buf[%d]=%p phys=%lx offset=%lx length=%zu\n", q->tail, 
                q->ring[q->tail], addr, offset, length);
    */
    q->tail = (q->tail + 1) & (q->size -1);
    return SYS_ERR_OK;
}

static errval_t enet_write_mdio(struct enet_driver_state* st, int8_t phyaddr,
                                int8_t regaddr, int16_t data)
{
    
    // Some protocol ...

    enet_mmfr_t reg = 0;
    reg = enet_mmfr_pa_insert(reg, phyaddr);
    reg = enet_mmfr_ra_insert(reg, regaddr);
    reg = enet_mmfr_data_insert(reg, data);   
    reg = enet_mmfr_st_insert(reg, 0x1);   
    reg = enet_mmfr_ta_insert(reg, 0x2);   

    // 1 is write 2 is read
    reg = enet_mmfr_op_insert(reg, 0x1);   
 
    ENET_DEBUG("Write MDIO: write cmd %lx \n", reg);

    enet_mmfr_wr(st->d, reg);

    uint16_t tries = 1000;
    while (!(enet_eir_mii_rdf(st->d) & 0x1)) {
        tries--;
        //barrelfish_usleep(10);
        if (tries == 0) {
            return ENET_ERR_MDIO_WRITE;
        }
    }
   
    enet_eir_mii_wrf(st->d, 0x1);
    return SYS_ERR_OK;
}

static errval_t enet_read_mdio(struct enet_driver_state* st, int8_t phyaddr,
                               int8_t regaddr, int16_t *data)
{
    
    // Some protocol ...
    enet_eir_mii_wrf(st->d, 0x1);

    enet_mmfr_t reg = 0;
    reg = enet_mmfr_pa_insert(reg, phyaddr);
    reg = enet_mmfr_ra_insert(reg, regaddr);
    reg = enet_mmfr_st_insert(reg, 0x1);   
    reg = enet_mmfr_ta_insert(reg, 0x2);   
    // 1 is write 2 is read
    reg = enet_mmfr_op_insert(reg, 0x2);   

    enet_mmfr_wr(st->d, reg);
    
    ENET_DEBUG("Read MDIO: read cmd %lx \n", reg);

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

static errval_t enet_get_phy_id(struct enet_driver_state* st)
{
    errval_t err;
    int16_t data; 
    uint32_t phy_id;

    // get phy ID1
    err = enet_read_mdio(st, PHY_ID,  0x2, &data);
    if (err_is_fail(err))  {
        return err;
    }   
    phy_id = data << 16;

    // get phy ID2
    err = enet_read_mdio(st, PHY_ID,  0x3, &data);
    if (err_is_fail(err))  {
        return err;
    }   

    phy_id |= data;
    st->phy_id = phy_id;    
    return err;
}

#define PHY_RESET 0x8000

#define PHY_RESET_CMD 0x0
#define PHY_STATUS_CMD 0x1
#define PHY_AUTONEG_CMD 0x4
#define PHY_LPA_CMD 0x5
#define PHY_CTRL1000_CMD 0x09
#define PHY_STAT1000_CMD 0x0a

static errval_t enet_reset_phy(struct enet_driver_state* st)
{
    debug_printf("PHY ID %d \n", st->phy_id);
    errval_t err;
    err = enet_write_mdio(st, PHY_ID, PHY_RESET_CMD, PHY_RESET);
    if (err_is_fail(err))  {
        return err;
    }   

    int16_t data;
    err = enet_read_mdio(st, PHY_ID, PHY_RESET_CMD, &data);
    if (err_is_fail(err))  {
        return err;
    }   
    
    int timeout = 500;
    while ((data & PHY_RESET) && timeout > 0) {
        err = enet_read_mdio(st, PHY_ID, PHY_RESET_CMD, &data);
        if (err_is_fail(err))  {
            return err;
        }   
    
        barrelfish_usleep(1000);
        timeout--;
    }

    if (data & PHY_RESET) {
        return ENET_ERR_PHY_RESET;
    }

    return SYS_ERR_OK;
}

static errval_t enet_setup_autoneg(struct enet_driver_state* st)
{
    errval_t err;
    int16_t status;
    int16_t autoneg;

    // Read BASIC MODE status register
    err = enet_read_mdio(st, PHY_ID, 0x1, &status);
    if (err_is_fail(err))  {
        return err;
    }   

    // READ autoneg status
    err = enet_read_mdio(st, PHY_ID, PHY_AUTONEG_CMD, &autoneg);
    if (err_is_fail(err))  {
        return err;
    }   
    
    // Read BASIC contorl register
    err = enet_read_mdio(st, PHY_ID, PHY_RESET_CMD, &status);
    if (err_is_fail(err))  {
        return err;
    }   

    // TODO uboot driver seems to only read this stuff ?
    return SYS_ERR_OK;
}

#define AUTONEG_100FULL 0x0100
#define AUTONEG_100HALF 0x0080
#define AUTONEG_10FULL  0x0040
#define AUTONEG_10HALF  0x0020
#define AUTONEG_PSB_802_3 0x0001

#define AUTONEG_ENABLE 0x1000
#define AUTONEG_RESTART 0x0200
static errval_t enet_restart_autoneg(struct enet_driver_state* st)
{
    errval_t err;
    err = enet_write_mdio(st, PHY_ID, PHY_RESET_CMD, PHY_RESET);
    if (err_is_fail(err)) {
        return err;
    }

    some_sleep(1000);
    //barrelfish_usleep(1000);

    err = enet_write_mdio(st, PHY_ID, PHY_AUTONEG_CMD, 
                          AUTONEG_100FULL | AUTONEG_100HALF | AUTONEG_10FULL |
                          AUTONEG_10HALF | AUTONEG_PSB_802_3);
    if (err_is_fail(err)) {
        return err;
    }
 
    err = enet_write_mdio(st, PHY_ID, PHY_RESET_CMD, 
                          AUTONEG_ENABLE | AUTONEG_RESTART);
    if (err_is_fail(err)) {
        return err;
    }
   
    return SYS_ERR_OK;
}


static errval_t enet_init_phy(struct enet_driver_state* st)
{
    errval_t err;
    err = enet_get_phy_id(st);
    if (err_is_fail(err))  {
        return err;
    }   
 
    err = enet_reset_phy(st);
    if (err_is_fail(err))  {
        return err;
    }   
   
    // board_phy_config in uboot driver. Don't know what
    // this actually does ...
    err = enet_write_mdio(st, PHY_ID, 0x1d, 0x1f);
    assert(err_is_ok(err));
    err = enet_write_mdio(st, PHY_ID, 0x1e, 0x8);
    assert(err_is_ok(err));
    err = enet_write_mdio(st, PHY_ID, 0x1d, 0x00);
    assert(err_is_ok(err));
    err = enet_write_mdio(st, PHY_ID, 0x1e, 0x82ee);
    assert(err_is_ok(err));
    err = enet_write_mdio(st, PHY_ID, 0x1d, 0x05);
    assert(err_is_ok(err));
    err = enet_write_mdio(st, PHY_ID, 0x1e, 0x100);
    assert(err_is_ok(err));

    err = enet_setup_autoneg(st);
    if (err_is_fail(err))  {
        return err;
    }   

    return SYS_ERR_OK;
}



#define PHY_STATUS_LSTATUS 0x0004
#define PHY_STATUS_ANEG_COMP 0x0020
#define PHY_STATUS_ESTAT 0x0100
#define PHY_STATUS_ERCAP 0x0001


#define PHY_LPA_100HALF  0x0080
#define PHY_LPA_100FULL 0x0100
#define PHY_LPA_10FULL  0x0040
// TODO check for rest of link capabilities
static void enet_parse_link(struct enet_driver_state* st)
{
    // just a sanity check if values are ok
    errval_t err;
    int16_t status;
    err = enet_read_mdio(st, PHY_ID, PHY_STAT1000_CMD, &status);
    assert(err_is_ok(err));

    int16_t mii_reg;
    err = enet_read_mdio(st, PHY_ID, PHY_STATUS_CMD, &mii_reg);
    assert(err_is_ok(err));

    if (status < 0) {   
        debug_printf("ENET not capable of 1G \n");
        return;
    } else {
        err = enet_read_mdio(st, PHY_ID, PHY_CTRL1000_CMD, &status);
        assert(err_is_ok(err));
        
        if (status == 0) {
            int16_t lpa, lpa2;   
            err = enet_read_mdio(st, PHY_ID, PHY_AUTONEG_CMD, &lpa);
            assert(err_is_ok(err));

            err = enet_read_mdio(st, PHY_ID, PHY_LPA_CMD, &lpa2);
            assert(err_is_ok(err));
        
            lpa &= lpa2;
            if (lpa & (PHY_LPA_100FULL | PHY_LPA_100HALF)) {
                if (lpa & PHY_LPA_100FULL) {
                    debug_printf("LINK 100 Mbit/s FULL duplex \n");
                } else {
                    debug_printf("LINK 100 Mbit/s half\n");
                }
            }
        }
    }

}

static errval_t enet_phy_startup(struct enet_driver_state* st)
{
    errval_t err;
    // board_phy_config in uboot driver. Don't know what
    // this actually does ...
    int16_t mii_reg;
    err = enet_read_mdio(st, PHY_ID, PHY_STATUS_CMD, &mii_reg);
    assert(err_is_ok(err));

    if (mii_reg & PHY_STATUS_LSTATUS) {
        debug_printf("LINK already UP\n");
        return SYS_ERR_OK;
    }
    
    if (!(mii_reg & PHY_STATUS_ANEG_COMP)) {
        ENET_DEBUG("Staring autonegotiation \n");
        while(!(mii_reg & PHY_STATUS_ANEG_COMP))  {
            err = enet_read_mdio(st, PHY_ID, PHY_STATUS_CMD, &mii_reg);
            assert(err_is_ok(err));
            some_sleep(1000);
        }
        
        ENET_DEBUG("Autonegotation done\n");
    }
    
    enet_parse_link(st);
    
    return SYS_ERR_OK;
}

// bool promiscous for promiscous mode. 
// This will also set it so that all multicast packets will also be received!
/*
static void enet_init_multicast_filt(struct enet_driver_state* st, bool promisc)
{
    if (promisc) {
        enet_rcr_prom_wrf(st->d, 1);
        return;
    }

    enet_rcr_prom_wrf(st->d, 0);
    
    // TODO Catching all multicast packets for now
    enet_gaur_wr(st->d, 0xFFFFFFFF);
    enet_galr_wr(st->d, 0xFFFFFFFF);
    // TODO if we do not catch all multicast packet then do this:
    // crc32 value of mac address
    #if 0
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
    #endif
    // TODO if this is M5272 then set the hash table entries to 0 ...
}
*/

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
    ENET_DEBUG("Activating RX ring \n");
    // bit is always set to 1 only when ring is empty then it is set to 0
    enet_rdar_rdar_wrf(st->d, 1); 
    some_sleep(1000);
}

/*
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
*/


static errval_t enet_reset(struct enet_driver_state* st)
{
    // reset device
    ENET_DEBUG("Reset device\n");
    
    uint64_t ecr = enet_ecr_rd(st->d);
    enet_ecr_wr(st->d, ecr | 0x1);
    int timeout = 500;
    while ((enet_ecr_rd(st->d) & 0x1) && timeout > 0) {
        barrelfish_usleep(10);
        // TODO timeout
    }

    if (timeout <= 0) {
        return ENET_ERR_DEV_RESET;
    }
   
    return SYS_ERR_OK;
}

static void enet_reg_setup(struct enet_driver_state* st)
{
    // Set interrupt mask register
    ENET_DEBUG("Set interrupt mask register\n");
    enet_eimr_wr(st->d, 0x0);
    // Clear outstanding interrupts
    ENET_DEBUG("Clear outstanding interrupts\n");
    enet_eir_wr(st->d, 0xFFFFFFFF);
    
    uint64_t reg; 
    // TODO see if other fields are required, not in dump
    reg = enet_rcr_rd(st->d);
    reg = enet_rcr_loop_insert(reg, 0x0);
    reg = enet_rcr_rmii_mode_insert(reg, 0x1);
    reg = enet_rcr_mii_mode_insert(reg, 0x1);
    reg = enet_rcr_fce_insert(reg, 0x1);
    reg = enet_rcr_max_fl_insert(reg, 1522);
    enet_rcr_wr(st->d, reg);   
}

static errval_t enet_rings_init(struct enet_driver_state* st)
{
    // init receive buffer descriptors
    enet_bufdesc_t desc;
    uint16_t val;

    memset(st->txq->ring, 0, st->txq->size*sizeof(enet_bufdesc_t));
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
    val |= ENET_SC_WRAP;
    enet_bufdesc_sc_insert(desc, val);

    st->txq->head = 0;
    st->txq->tail = 0;

    // TODO dchace flush

    memset(st->rxq->ring, 0, st->rxq->size*sizeof(enet_bufdesc_t));
    ENET_DEBUG("RX %p ring init to default values \n", st->rxq->ring);
    for (int i = 0; i < st->rxq->size; i++) {
        desc = st->rxq->ring[i];
        enet_bufdesc_sc_insert(desc, 0);
    }

    // set last one to wrap
    desc = st->rxq->ring[st->rxq->size - 1];
    val = enet_bufdesc_sc_extract(desc);
    val |= ENET_SC_WRAP;
    enet_bufdesc_sc_insert(desc, val);
    st->rxq->head = 0;
    st->rxq->tail = 0;
    
    // TODO dchace flush
    return SYS_ERR_OK;
}

static errval_t enet_open(struct enet_driver_state *st)
{
    errval_t err = SYS_ERR_OK;
    // Enable full duplex, disable heartbeet
    enet_tcr_fden_wrf(st->d, 0x1);

    // Invalidate rx descriptors
    st->rxq->tail = 0;
    err = frame_alloc(&st->rx_mem, 512*2048, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    regionid_t rid;
    err = devq_register((struct devq*) st->rxq, st->rx_mem, &rid);
    if (err_is_fail(err)) {
        return err;
    }

    for (int i = 0; i < st->rxq->size-1; i++) {
        err = devq_enqueue((struct devq*) st->rxq, rid, i*(2048), 2048,
                            0, 2048, 0);
        if (err_is_fail(err)) {
            return err;
        }
    }

    // Enable HW endian swap
    enet_ecr_dbswp_wrf(st->d, 0x1);
    enet_ecr_en1588_wrf(st->d, 0x0);
    // Enable store and forward mode
    enet_tfwr_strfwd_wrf(st->d, 0x1);
    // Enable controler
    enet_ecr_etheren_wrf(st->d, 0x1);

    // TODO don't think this is MX25/MX53 or MX6SL
    // Startup PHY
    err = enet_phy_startup(st);
    if (err_is_fail(err))  {
        return err;
    } 

    uint8_t speed = enet_ecr_speed_rdf(st->d);
    
    if (!speed) {
        enet_rcr_rmii_10t_wrf(st->d, 0x0);
    }

    enet_activate_rx_ring(st);
    ENET_DEBUG("Init done! \n");
    return err;
}

static errval_t enet_init(struct enet_driver_state* st)
{
    errval_t err = SYS_ERR_OK;
    // set HW addreses
    enet_iaur_wr(st->d, 0);
    enet_ialr_wr(st->d, 0);
    enet_gaur_wr(st->d, 0);
    enet_galr_wr(st->d, 0);
    enet_write_mac(st);

    ENET_DEBUG("Setting RX/TX rings to default values \n");
    // Init rings
    err = enet_rings_init(st);
    if (err_is_fail(err)){
        return err;
    }

    enet_reg_setup(st);

    uint64_t reg; 
    // Set MII speed, do not drop preamble and set hold time to 10ns
    reg = enet_mscr_rd(st->d);
    reg = enet_mscr_mii_speed_insert(reg, 0x18);
    reg = enet_mscr_hold_time_insert(reg, 0x1);
    enet_mscr_wr(st->d, reg);

    // Set Opcode and Pause duration
    enet_opd_wr(st->d, 0x00010020);
    enet_tfwr_tfwr_wrf(st->d, 0x2);

    // Set multicast addr filter
    enet_gaur_wr(st->d, 0);
    enet_galr_wr(st->d, 0);

    // Max pkt size rewrite ...
    reg = enet_rcr_rd(st->d);
    reg = enet_rcr_max_fl_insert(reg, ENET_MAX_PKT_SIZE);
    reg = enet_rcr_prom_insert(reg, 1);
    enet_rcr_wr(st->d, reg);   

    // Tell card beginning of rx/tx rings
    enet_rdsr_wr(st->d, st->rxq->desc_mem.devaddr);
    enet_tdsr_wr(st->d, st->txq->desc_mem.devaddr);

    err = enet_restart_autoneg(st);
    if (err_is_fail(err)) {
        return err;
    }

    err = enet_open(st);
    if (err_is_fail(err)) {
        // TODO cleanup
        return err;
    }

    return err;
}

static errval_t enet_probe(struct enet_driver_state* st)
{
    errval_t err;
    err = enet_reset(st);
    if (err_is_fail(err)) {
        return err;
    }
 
    enet_reg_setup(st);
   
    uint64_t reg; 
    // Set MII speed, do not drop preamble and set hold time to 10ns
    reg = enet_mscr_rd(st->d);
    reg = enet_mscr_mii_speed_insert(reg, 0x18);
    reg = enet_mscr_hold_time_insert(reg, 0x1);
    enet_mscr_wr(st->d, reg);

    err = enet_init_phy(st);
    if (err_is_fail(err))  {
        debug_printf("Failed PHY reset\n");
        return err;
    }   

    // Write back mac again
    ENET_DEBUG("Reset MAC\n");
    // TODO do this later? NOT in dump
    enet_write_mac(st);
    enet_read_mac(st);

    // TODO checked dump until here! 
    return SYS_ERR_OK;
}

static errval_t enet_alloc_queues(struct enet_driver_state* st)
{   
    errval_t err;
    ENET_DEBUG("Allocating TX/RX ring data structures \n");
    st->rxq = calloc(1, sizeof(struct enet_queue));
    st->txq = calloc(1, sizeof(struct enet_queue));
    st->rxq->size = RX_RING_SIZE;
    st->txq->size = TX_RING_SIZE;
    st->txq->d = st->d;
    st->rxq->d = st->d;

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
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
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
    assert((st->rxq->desc_mem.devaddr & st->rxq->align) == 0);

    memset(st->rxq->ring, 0, st->rxq->size*sizeof(enet_bufdesc_t));

    st->txq->desc_mem.vbase = (st->rxq->desc_mem.vbase + (st->rxq->size*sizeof(enet_bufdesc_t)));
    st->txq->desc_mem.devaddr = (st->rxq->desc_mem.devaddr + (st->rxq->size*sizeof(enet_bufdesc_t)));
    st->txq->ring = (void*) st->txq->desc_mem.vbase;
    st->txq->desc_mem.size = st->rxq->desc_mem.size;

    assert(st->txq->ring);
    assert((st->txq->desc_mem.devaddr & st->txq->align) == 0);

    memset(st->txq->ring, 0, st->txq->size*sizeof(enet_bufdesc_t));
    /*
    // Disable RX
    uint32_t reg_val = enet_eimr_rd(st->d);
    enet_eimr_rxf_insert(reg_val, 0);
    enet_eimr_rxf1_insert(reg_val, 0);
    enet_eimr_rxf2_insert(reg_val, 0);
    enet_eimr_wr(st->d, reg_val);
    */
    ENET_DEBUG("RX phys=%p virt=%p \n", (void*) st->rxq->desc_mem.devaddr,
                (void*) st->rxq->ring);
    ENET_DEBUG("TX phys=%p virt=%p \n", (void*) st->txq->desc_mem.devaddr,
                (void*) st->txq->ring);

    return SYS_ERR_OK;
}

uint16_t packet[21] = { 0xffff, 0xffff, 0xffff, 0x507b,
                        0x9d2b, 0x1cbe, 0x0806, 0x0001,
                        0x0800, 0x0604, 0x0001, 0x507b,
                        0x9d2b, 0x1cbe, 0x0a05, 0x29d6,
                        0x0000, 0x0000, 0x0000, 0x0a6e,
                        0x0404};

static void print_tx_stats(struct enet_driver_state* st)
{
    debug_printf("TX PKT %d \n", enet_rmon_t_packets_rd(st->txq->d));
    debug_printf("TX PKT BC %d \n", enet_rmon_t_bc_pkt_rd(st->txq->d));
    debug_printf("TX PKT MC %d \n", enet_rmon_t_mc_pkt_rd(st->txq->d));
    debug_printf("TX PKT CRC/Align %d \n", enet_rmon_t_crc_align_rd(st->txq->d));
    debug_printf("TX PKT Undersized %d \n", enet_rmon_t_undersize_rd(st->txq->d));
    debug_printf("TX PKT Oversized %d \n", enet_rmon_t_oversize_rd(st->txq->d));
    debug_printf("TX PKT Undersized bad CRC %d \n", enet_rmon_t_frag_rd(st->txq->d));
    debug_printf("TX PKT Oversized bad CRC %d \n", enet_rmon_t_jab_rd(st->txq->d));
    debug_printf("TX PKT Collision %d \n", enet_rmon_t_col_rd(st->txq->d));

}

static errval_t send_pkt(struct enet_driver_state* st, regionid_t rid) 
{
    struct region_entry *entry = get_region(st->txq, rid);
    assert(entry);    
    memcpy((void*) entry->mem.vbase, (void *) packet, 21*sizeof(uint16_t));

    // try sending buffer 0
    
    struct devq_buf buf;
    buf.rid = rid;
    buf.offset = 0;
    buf.valid_data = 0;
    buf.length = 2048;
    buf.valid_length = 21*sizeof(uint16_t);
    buf.flags = 0;

    errval_t err;
    err = devq_enqueue((struct devq*) st->txq, buf.rid, buf.offset,
                       buf.length, buf.valid_data, buf.valid_length,
                       buf.flags);
    if (err_is_ok(err)) {
        // cleanup
        while(true) {
            err = devq_dequeue((struct devq*) st->txq, &buf.rid, &buf.offset,
                               &buf.length, &buf.valid_data, &buf.valid_length,
                               &buf.flags);
            if (err_is_ok(err)) {
                break;
            }
        }
    }
    print_tx_stats(st);
    debug_printf("Finished sending packet \n");
    return err;
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
    
    // Alloc queues
    err = enet_alloc_queues(st);
    if (err_is_fail(err)) {
        USER_PANIC("Failed initalizing queues \n");
        return err;
    }

    // Alloc cleanq data structs
    
    st->rxq->ring_bufs = calloc(st->rxq->size, sizeof(struct devq_buf));
    st->txq->ring_bufs = calloc(st->txq->size, sizeof(struct devq_buf));
    assert(st->rxq->ring_bufs);
    assert(st->txq->ring_bufs);

    err = enet_probe(st);
    if (err_is_fail(err)) {
        // TODO cleanup
        return err;
    }

    //ENET_DEBUG("Init MII bus\n");
    //enet_init_mii(st);

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

    err = enet_init(st);
    if (err_is_fail(err)) {
        // TODO cleanup
        return err;
    }

    err = frame_alloc(&st->tx_mem, 512*2048, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    regionid_t rid;
    err = devq_register((struct devq*) st->txq, st->tx_mem, &rid);
    if (err_is_fail(err)) {
        return err;
    }
    
    struct devq_buf buf;
    while(true) {
        err = devq_dequeue((struct devq*) st->rxq, &buf.rid, &buf.offset,
                           &buf.length, &buf.valid_data, &buf.valid_length,
                           &buf.flags);
        if (err_is_ok(err)) {
            
        } else {

        }

        some_sleep(1000);
        err = send_pkt(st, rid);
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
