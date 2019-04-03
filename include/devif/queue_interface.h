/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef QUEUE_INTERFACE_H_
#define QUEUE_INTERFACE_H_ 1


#include <barrelfish/barrelfish.h>


#define DEVQ_FLAG_LAST (1UL << 30)

typedef uint32_t regionid_t;
typedef uint32_t bufferid_t;
typedef uint64_t genoffset_t;


struct devq;
struct region_pool;

// For convinience reason buffer descritpion in one struct
struct devq_buf{
    genoffset_t offset; // 8
    genoffset_t length; // 16
    genoffset_t valid_data; // 24
    genoffset_t valid_length; // 32
    uint64_t flags; // 40
    regionid_t rid; // 44
};

/*
 * ===========================================================================
 * Datapath functions
 * ===========================================================================
 */
/*
 *
 * @brief enqueue a buffer into the device queue
 *
 * @param q             The device queue to call the operation on
 * @param region_id     Id of the memory region the buffer belongs to
 * @param offset        Offset into the region i.e. where the buffer starts
 *                      that is enqueued
 * @param lenght        Lenght of the enqueued buffer
 * @param valid_data    Offset into the buffer where the valid data of this buffer
 *                      starts
 * @param valid_length  Length of the valid data of this buffer
 * @param misc_flags    Any other argument that makes sense to the device queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_enqueue(struct devq *q,
                      regionid_t region_id,
                      genoffset_t offset,
                      genoffset_t lenght,
                      genoffset_t valid_data,
                      genoffset_t valid_lenght,
                      uint64_t misc_flags);

/**
 * @brief dequeue a buffer from the device queue
 *
 * @param q             The device queue to call the operation on
 * @param region_id     Return pointer to the id of the memory
 *                      region the buffer belongs to
 * @param region_offset Return pointer to the offset into the region where
 *                      this buffer starts.
 * @param lenght        Return pointer to the lenght of the dequeue buffer
 * @param valid_data    Return pointer to an offset into the buffer where the
 *                      valid data of this buffer starts
 * @param valid_length  Return pointer to the length of the valid data of
 *                      this buffer
 * @param misc_flags    Return value from other endpoint
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_dequeue(struct devq *q,
                      regionid_t* region_id,
                      genoffset_t* offset,
                      genoffset_t* langht,
                      genoffset_t* valid_data,
                      genoffset_t* valid_length,
                      uint64_t* misc_flags);

/*
 * ===========================================================================
 * Control Path
 * ===========================================================================
 */

/**
 * @brief Add a memory region that can be used as buffers to
 *        the device queue
 *
 * @param q              The device queue to call the operation on
 * @param cap            A Capability for some memory
 * @param region_id      Return pointer to a region id that is assigned
 *                       to the memory
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_register(struct devq *q,
                       struct capref cap,
                       regionid_t* region_id);

/**
 * @brief Remove a memory region
 *
 * @param q              The device queue to call the operation on
 * @param region_id      The region id to remove from the device
 *                       queues memory
 * @param cap            The capability to the removed memory
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_deregister(struct devq *q,
                         regionid_t region_id,
                         struct capref* cap);

/**
 * @brief Send a notification about new buffers on the queue
 *
 * @param q      The device queue to call the operation on
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_notify(struct devq *q);

/**
 * @brief Enforce coherency between of the buffers in the queue
 *        by either flushing the cache or invalidating it
 *
 * @param q      The device queue to call the operation on
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_prepare(struct devq *q);

/**
 * @brief Send a control message to the device queue
 *
 * @param q          The device queue to call the operation on
 * @param request    The type of the control message*
 * @param value      The value for the request
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_control(struct devq *q,
                      uint64_t request,
                      uint64_t value,
                      uint64_t *result);


 /**
  * @brief destroys the device queue
  *
  * @param q           The queue state to free (and the device queue to be 
                       shut down)
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
errval_t devq_destroy(struct devq *q);

void devq_set_state(struct devq *q, void *state);
void * devq_get_state(struct devq *q);


 /**
  * @brief gets iommu client for this device queue so we can allocate
  *        memory for virtualized devices
  *
  * @param q           The queue state to free (and the device queue to be 
                       shut down)
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
struct iommu_client * devq_get_iommu_client(struct devq *q);

#endif /* QUEUE_INTERFACE_H_ */
