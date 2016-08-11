/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DEVICE_QUEUE_INTERFACE_H_
#define DEVICE_QUEUE_INTERFACE_H_ 1

#include <barrelfish/barrelfish.h>

typedef uint64_t regionid_t;
typedef uint64_t bufferid_t;

/*
 * ===========================================================================
 * Device queue creation and destruction
 * ===========================================================================
 */

/**
 * @brief creates a queue
 * @returns error on failure or SYS_ERR_OK on success
 */

errval_t devq_create(void* st, char *device_name, uint64_t flags, void **queue);

/**
 * @brief destroys the device queue
 *
 * @param q           The queue state to free (and the device queue to be
                      shut down in the driver)
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t devq_destroy(void *queue);

/*
 * ===========================================================================
 * Datapath functions
 * ===========================================================================
 */

/**
 * @brief enqueue a buffer into the device queue
 *
 * @param q             The device queue to call the operation on
 * @param region_id     Id of the memory region the buffer belongs to
 * @param base          Physical address of the start of the enqueued buffer
 * @param length        Length of the enqueued buffer
 * @param buffer_id     The buffer id of the enqueue buffer (TODO only for
 *                      fixed size buffers?)
 * @param misc_flags    Any other argument that makes sense to the device queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_enqueue(void *q, regionid_t region_id, lpaddr_t base,
                      size_t length, bufferid_t buffer_id, uint64_t flags);

/**
 * @brief dequeue a buffer from the device queue
 *
 * @param q             The device queue to call the operation on
 * @param region_id     Return pointer to the id of the memory
 *                      region the buffer belongs to
 * @param base          Return pointer to the physical address of
 *                      the of the buffer
 * @param length        Return pointer to the length of the dequeue buffer
 * @param buffer_id     Return pointer to the buffer id of the dequeued buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_dequeue(void *q, regionid_t *region_id, lpaddr_t *base,
                      size_t *length, bufferid_t *buffer_id);
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
errval_t devq_register(void *q, struct capref cap, regionid_t *region_id);

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
errval_t devq_remove(void *q, regionid_t region_id);

/**
* @brief Send a notification about new buffers on the queue
*
* @param q      The device queue to call the operation on
*
* @returns error on failure or SYS_ERR_OK on success
*
*/
errval_t devq_sync(void *q);

/**
* @brief Send a control message to the device queue
*
* @param q       The device queue to call the operation on
* @param request A sting encoding the control message
* @param value   A value for the request.
*
* @returns error on failure or SYS_ERR_OK on success
*
*/
errval_t devq_control(void *q, uint64_t request, uint64_t value);

#endif /* DEVICE_QUEUE_INTERFACE_H_ */
