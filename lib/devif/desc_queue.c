/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <devif/queue_interface.h>
#include "desc_queue.h"
#include "dqi_debug.h"


struct __attribute__((aligned(DESCQ_ALIGNMENT))) desc {
    regionid_t rid; // 4
    bufferid_t bid; // 8
    lpaddr_t addr; // 16
    size_t len; // 24
    uint64_t flags; // 32
    uint8_t pad[32];
};

union __attribute__((aligned(DESCQ_ALIGNMENT))) pointer {
    size_t value;
    uint8_t pad[64];
};

struct descq {
    // Shared memory of the queue
    struct capref shm;
    size_t slots;

    size_t local_head;
    size_t local_tail;
    // Queue pointers
    volatile union pointer* head;
    volatile union pointer* tail;

    // The queue itself
    struct desc* descs;
};


/**
 * @brief initialized a descriptor queue
 *
 * @param q                     Return pointer to the descriptor queue
 * @param shm                   Cap of the shared memory of the queue
 * @param slots                 Number of slots in the queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t descq_init(struct descq** q,
                    struct capref shm,
                    size_t slots)
{
    errval_t err;
    struct descq* tmp;
    
    // Init basic struct fields
    tmp = malloc(sizeof(struct descq));
    assert(tmp != NULL);

    tmp->shm = shm;
    tmp->slots = slots-2;

    struct frame_identity id;
    // Check if the frame is big enough
    err = invoke_frame_identify(shm, &id);
    if (err_is_fail(err)) {
        free(tmp);
        return DEVQ_ERR_DESCQ_INIT;
    } 

    if (id.bytes < DESCQ_ALIGNMENT*slots) {
        free(tmp);
        return DEVQ_ERR_DESCQ_INIT;
    }

    // TODO what about the non cache coherent case?
    err = vspace_map_one_frame_attr((void**) &(tmp->head),
                                    slots*DESCQ_ALIGNMENT, shm, 
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        free(tmp);
        return DEVQ_ERR_DESCQ_INIT;
    }

    tmp->tail = tmp->head + 1;
    tmp->descs = (struct desc*) tmp->head + 2;
    tmp->tail->value = 0;
    tmp->head->value = 0;    
    tmp->local_head = 0;
    tmp->local_tail = 0;

    *q = tmp;
    return SYS_ERR_OK;
}


/**
 * @brief Destroys a descriptor queue and frees its resources
 *
 * @param q                     The descriptor queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t descq_destroy(struct descq* q)
{   
    errval_t err;
    err = vspace_unmap((void*) (q->descs));
    if (err_is_fail(err)){
        return err;
    }
    
    free(q);

    return SYS_ERR_OK;
}

/**
 * @brief Enqueue a descriptor (as seperate fields) 
 *        into the descriptor queue
 *
 * @param q                     The descriptor queue
 * @param region_id             Region id of the enqueued buffer
 * @param buffer_id             Buffer id of the buffer
 * @param base                  Physical address of hte buffer
 * @param len                   Lenght of the buffer
 * @param misc_flags            Miscellaneous flags
 *
 * @returns error if queue is full or SYS_ERR_OK on success
 */
errval_t descq_enqueue(struct descq* q,
                       regionid_t region_id,
                       bufferid_t buffer_id,
                       lpaddr_t base,
                       size_t len,
                       uint64_t misc_flags)
{
    if (descq_full(q)) {
        return DEVQ_ERR_TX_FULL;
    }
    
    size_t head = q->local_head;
    q->descs[head].rid = region_id;
    q->descs[head].bid = buffer_id;
    q->descs[head].addr = base;
    q->descs[head].len = len;
    q->descs[head].flags = misc_flags;
    
    // only write local head
    q->local_head = q->local_head + 1 % q->slots;

    return SYS_ERR_OK;
}
/**
 * @brief Dequeue a descriptor (as seperate fields) 
 *        from the descriptor queue
 *
 * @param q                     The descriptor queue
 * @param region_id             Return pointer to the region id of 
 *                              the denqueued buffer
 * @param buffer_id             Return pointer to the buffer id of the buffer
 * @param base                  Return pointer to the physical address 
 *                              of the buffer
 * @param len                   Return pointer to the lenght of the buffer
 * @param misc_flags            Return pointer to miscellaneous flags
 *
 * @returns error if queue is empty or SYS_ERR_OK on success
 */
errval_t descq_dequeue(struct descq* q,
                       regionid_t* region_id,
                       bufferid_t* buffer_id,
                       lpaddr_t* base,
                       size_t* len,
                       uint64_t* misc_flags)
{
    if (descq_empty(q)) {
        return DEVQ_ERR_RX_EMPTY;
    }
    
    size_t tail = q->local_tail;
    *region_id = q->descs[tail].rid;
    *buffer_id = q->descs[tail].bid;
    *base = q->descs[tail].addr;
    *len = q->descs[tail].len;
    *misc_flags = q->descs[tail].flags;
    
    q->local_tail = q->local_tail + 1 % q->slots;

    return SYS_ERR_OK;
}

/**
 * @brief Writes the local head pointer into the shared memory
 *        making the state of the queue visible to the other end
 *
 * @param q                     The descriptor queue
 *
 */
void descq_writeout_head(struct descq* q)
{
    q->head->value = q->local_head;
}

/**
 * @brief Writes the local tail pointer into the shared memory
 *        making the state of the queue visible to the other end
 *
 * @param q                     The descriptor queue
 *
 */
void descq_writeout_tail(struct descq* q)
{
    q->tail->value = q->local_tail;
}

/**
 * @brief Check if the descriptor queue is full
 *
 * @param q                     The descriptor queue
 *
 * @returns true if the queue is full, otherwise false
 */
bool descq_full(struct descq* q)
{
    size_t head = q->local_head;
    size_t tail = q->tail->value;
    if (head >= tail) {
        return ((q->slots - (head - tail)) == 0);
    } else {
        return ((q->slots - (head + q->slots - tail)) == 0);
    }
}
/**
 * @brief Check if the descriptor queue is empty
 *
 * @param q                     The descriptor queue
 *
 * @returns true if the queue is empty, otherwise false
 */
bool descq_empty(struct descq* q)
{
    size_t head = q->head->value;
    size_t tail = q->local_tail;
    if (head >= tail) {
        return ((head - tail) == 0);
    } else {
        return (((head + q->slots) - tail) == 0);
    }
}
/**
 * @brief Returns the number of occupied slots in the queue
 *
 * @param q                     The descriptor queue
 *
 * @returns the number of occupied slots
 */
size_t descq_full_slots(struct descq* q)
{
    size_t head = q->head->value;
    size_t tail = q->local_tail;
    if (head >= tail) {
        return (head - tail);
    } else {
        return (head + q->slots) - tail;
    }
}
