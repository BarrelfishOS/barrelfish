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
    regionid_t region_id; // 4
    bufferid_t buffer_id; // 8
    lpaddr_t base; // 16
    size_t length; // 24
    uint64_t misc_flags; // 32
    uint8_t pad[32];
};

struct descq {
    // Shared memory of the queue
    struct capref shm;
    size_t slots;

    // Queue pointers
    size_t head;
    size_t tail;

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
    USER_PANIC("NIY");
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
    USER_PANIC("NIY");
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
    USER_PANIC("NIY");
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
    USER_PANIC("NIY");
    return SYS_ERR_OK;
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
    USER_PANIC("NIY");
    return false;
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
    USER_PANIC("NIY");
    return false;
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
    USER_PANIC("NIY");
    return 0;
}
