/**
 * \file
 * \brief imx8 NIC device queue
 */
/*
 * Copyright (c) 2020, ETH Zurich.
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
#include <devif/backends/net/enet_devif.h>
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


static void enet_activate_tx_ring(enet_t * d)
{
    // bit is always set to 1 only when ring is empty then it is set to 0
    enet_tdar_tdar_wrf(d, 1); 
}

static void enet_activate_rx_ring(enet_t* d)
{
    // bit is always set to 1 only when ring is empty then it is set to 0
    enet_rdar_rdar_wrf(d, 1); 
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

    if (q->head == q->tail) {
        return DEVQ_ERR_QUEUE_EMPTY;
    }
    /*
    ENET_DEBUG("Try dequeue %d RADR %d ENABLED %d STATUS %lx \n", q->head, 
               enet_rdar_rdar_rdf(q->d), enet_ecr_etheren_rdf(q->d), status);
    */
    if (!(status & ENET_RX_EMPTY)) {
        // TODO error handling!
        *valid_length = enet_bufdesc_len_extract(desc);
        ENET_DEBUG("Received Packet len=%lu entry=%zu \n", *valid_length,
                   q->head);
        ENET_DEBUG("offset=%lu length=%lu valid_data=%lu rid=%lu \n",
                   buf->offset, buf->length, 0, buf->rid);
        *offset = buf->offset;
        *valid_data = 0;
        *length = 2048;
        *rid = buf->rid;
        *flags = buf->flags;
    } else {
        return DEVQ_ERR_QUEUE_EMPTY;
    }
    status &= ~ENET_RX_STATS;

    __builtin___clear_cache(q->ring[q->head], q->ring[q->head+1]);    

    enet_bufdesc_sc_insert(desc, status);
    
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
            *length = buf->length;
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


static errval_t enet_tx_enqueue(struct devq* que, regionid_t rid, genoffset_t offset,
                                genoffset_t length, genoffset_t valid_data,
                                genoffset_t valid_length, uint64_t flags)
{
    
    struct enet_queue* q = (struct enet_queue*) que;   

    assert(valid_length > 0 && valid_length < ENET_MAX_PKT_SIZE);

    if (enet_full_slots(q) == q->size) {
        return DEVQ_ERR_QUEUE_FULL;
    }

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
    __builtin___clear_cache(q->ring[q->tail], q->ring[q->tail+1]);

    if (q->tail == (q->size -1)) {
        enet_bufdesc_sc_insert(desc, ENET_TX_READY | ENET_TX_CRC | 
                               ENET_TX_LAST | ENET_TX_WRAP);
    } else {
        enet_bufdesc_sc_insert(desc, ENET_TX_READY | ENET_TX_CRC | ENET_TX_LAST);
    }

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
    
    assert(valid_length > 0 && length <= ENET_MAX_BUF_SIZE);

    if (enet_full_slots(q) == q->size) {
        return DEVQ_ERR_QUEUE_FULL;
    }
  
    lpaddr_t addr = 0;
    addr = (lpaddr_t) entry->mem.devaddr + offset;
 
    struct devq_buf* buf= &q->ring_bufs[q->tail];
    buf->offset = offset;
    buf->length = length;
    buf->valid_length = valid_length;
    buf->valid_data = valid_data;
    buf->rid = rid;
    buf->flags = flags;
   
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
    // activate RX (This is only needed if ring is empty)
    enet_activate_rx_ring(q->d);

    q->tail = (q->tail + 1) & (q->size -1);
    return SYS_ERR_OK;
}

errval_t enet_rx_queue_create(struct enet_queue ** q, struct capref* ep, 
                              struct capref* regs, void(*int_handler)(void*))
{
    errval_t err;
    debug_printf("Allocating rx device queue \n");
    struct enet_queue* rxq;
    rxq = calloc(1, sizeof(struct enet_queue));
    assert(rxq);

    rxq->size = RX_RING_SIZE;
    
    lvaddr_t vaddr;
    err = map_device_cap(*regs, &vaddr);
    if (err_is_fail(err)) {
        USER_PANIC("map_device_cap failed \n");
    }

    /* Initialize Mackerel binding */
    rxq->d = (enet_t *) malloc(sizeof(enet_t));
    assert(rxq->d);
    enet_initialize(rxq->d, (void *) vaddr);

    rxq->align = 0x3f;

    // TODO check for advanced descriptors
    // TODO linking iommu driverkit library does not seem to work ...
    ENET_DEBUG("Allocating RX/TX descriptor ring \n");
    size_t tot_size = (rxq->size)*sizeof(enet_bufdesc_t);
    err = frame_alloc(&(rxq->desc_mem.mem), tot_size, (size_t *)&(rxq->desc_mem.size));
    if (err_is_fail(err)) {
        return err;
    }

    ENET_DEBUG("Mapping RX/TX descriptor ring\n");
    err = vspace_map_one_frame_attr((void**) &(rxq->desc_mem.vbase), tot_size, 
                                    rxq->desc_mem.mem, 
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(rxq->desc_mem.mem);
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        return err;
    }

    struct frame_identity id;
    err = frame_identify(rxq->desc_mem.mem, &id);
    if (err_is_fail(err)) {
        return err;
    }

    rxq->desc_mem.devaddr = id.base;
    rxq->ring = (void*) rxq->desc_mem.vbase;
    assert(rxq->ring);
    assert((rxq->desc_mem.devaddr & rxq->align) == 0);

    enet_rdsr_wr(rxq->d, rxq->desc_mem.devaddr);

    memset(rxq->ring, 0, rxq->size*sizeof(enet_bufdesc_t));

    rxq->ring_bufs = calloc(rxq->size, sizeof(struct devq_buf));
    assert(rxq->ring_bufs);

    enet_bufdesc_t desc;
    debug_printf("RX %p ring init to default values \n", rxq->ring);
    for (int i = 0; i < rxq->size; i++) {
        desc = rxq->ring[i];
        enet_bufdesc_sc_insert(desc, 0);
    }

    uint64_t val;
    // set last one to wrap
    desc = rxq->ring[rxq->size - 1];
    val = enet_bufdesc_sc_extract(desc);
    val |= ENET_SC_WRAP;
    enet_bufdesc_sc_insert(desc, val);
    rxq->head = 0;
    rxq->tail = 0;

    err = devq_init(&rxq->q, false);
    if (err_is_fail(err)) {
        debug_printf("enet devq_init error\n");
        return err;
    }

    rxq->q.f.reg = enet_register;
    rxq->q.f.enq = enet_rx_enqueue;
    rxq->q.f.deq = enet_rx_dequeue;

    *q = rxq;

    return SYS_ERR_OK;
}


errval_t enet_tx_queue_create(struct enet_queue ** q, struct capref* ep, 
                              struct capref* regs, void(*int_handler)(void*))
{
    errval_t err;
    debug_printf("Allocating rx device queue \n");
    struct enet_queue* txq;
    txq = calloc(1, sizeof(struct enet_queue));
    txq->size = TX_RING_SIZE;
    
    lvaddr_t vaddr;
    err = map_device_cap(*regs, &vaddr);
    if (err_is_fail(err)) {
        USER_PANIC("map_device_cap failed \n");
    }

    /* Initialize Mackerel binding */
    txq->d = (enet_t *) malloc(sizeof(enet_t));
    assert(txq->d);
    enet_initialize(txq->d, (void *) vaddr);

    txq->align = 0x3f;

    debug_printf("Allocating TX descriptor ring \n");
    size_t tot_size = (txq->size)*sizeof(enet_bufdesc_t);
    err = frame_alloc(&(txq->desc_mem.mem), tot_size, (size_t *)&(txq->desc_mem.size));
    if (err_is_fail(err)) {
        return err;
    }

    debug_printf("Mapping RX/TX descriptor ring\n");
    err = vspace_map_one_frame_attr((void**) &(txq->desc_mem.vbase), tot_size, 
                                    txq->desc_mem.mem, VREGION_FLAGS_READ_WRITE_NOCACHE, 
                                    NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(txq->desc_mem.mem);
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        return err;
    }

    struct frame_identity id;
    err = frame_identify(txq->desc_mem.mem, &id);
    if (err_is_fail(err)) {
        return err;
    }

    txq->desc_mem.devaddr = id.base;
    txq->ring = (void*) txq->desc_mem.vbase;
    assert(txq->ring);
    assert((txq->desc_mem.devaddr & txq->align) == 0);

    // Tell card beginning of rx/tx rings
    enet_tdsr_wr(txq->d, txq->desc_mem.devaddr);

    memset(txq->ring, 0, txq->size*sizeof(enet_bufdesc_t));

    txq->ring_bufs = calloc(txq->size, sizeof(struct devq_buf));
    assert(txq->ring_bufs);

    debug_printf("TX %p ring init to default values \n", txq->ring);

    enet_bufdesc_t desc;
    // init send buffer descriptors
    for (int i = 0; i < txq->size; i++) {
        desc = txq->ring[i];
        enet_bufdesc_sc_insert(desc, 0);
        enet_bufdesc_addr_insert(desc, 0);
    }

    uint64_t val;
    // set last one to wrap
    desc = txq->ring[txq->size - 1];
    val = enet_bufdesc_sc_extract(desc);
    val |= ENET_SC_WRAP;
    enet_bufdesc_sc_insert(desc, val);

    txq->head = 0;
    txq->tail = 0;

    err = devq_init(&txq->q, false);
    if (err_is_fail(err)) {
        debug_printf("enet devq_init error\n");
        return err;
    }

    txq->q.f.reg = enet_register;
    txq->q.f.enq = enet_tx_enqueue;
    txq->q.f.deq = enet_tx_dequeue;

    *q = txq;

    return SYS_ERR_OK;
}
