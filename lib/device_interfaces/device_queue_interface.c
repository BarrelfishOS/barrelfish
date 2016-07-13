/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <device_interfaces/device_queue_interface.h>
#include "region_pool.h"
 /*
 * ===========================================================================
 * Device queue creation and destruction
 * ===========================================================================
 */


 /**
  * @brief creates a queue 
  *
  * @param q             Return pointer to the device_queue (handle)
  * @param device_name   Device name of the device to which this queue belongs
  *                      (Driver itself is running in a separate process)
  * @param misc          Anything you can think of that makes sense for the device
  *                      and its driver?
  *
  * @returns error on failure or SYS_ERR_OK on success
  */

errval_t device_queue_create(struct device_queue **q,
                             char* device_name,
                             char* misc)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}


 /**
  * @brief destroys the device queue
  *
  * @param q           The queue state to free (and the device queue to be 
                       shut down in the driver)
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
errval_t device_queue_destroy(struct device_queue *qp)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}

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
 * @param lenght        Lenght of the enqueued buffer
 * @param buffer_id     The buffer id of the enqueue buffer (TODO only for 
 *                      fixed size buffers?)
 * @param misc_flags    Any other argument that makes sense to the device queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
/*
errval_t device_queue_enqueue(struct device_queue *q,
                              regionid_t region_id,
                              lpaddr_t base,
                              size_t length,
                              bufferid_t buffer_id,
                              char* misc_flags)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}
*/
/**
 * @brief enqueue some memory into the device queue
 *
 * @param q             The device queue to call the operation on
 * @param buf           Buffer to enqueue (includes physical address, lenght, 
 *                      region id and buffer id)
 * @param misc_flags    Any other argument that makes sense to the device queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t device_queue_enqueue(struct device_queue *q,
                              struct device_queue_buffer* buf,
                              char* misc_flags)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}

/**
 * @brief dequeue a buffer from the device queue
 *
 * @param q             The device queue to call the operation on
 * @param region_id     Return pointer to the id of the memory 
 *                      region the buffer belongs to
 * @param base          Return pointer to the physical address of 
 *                      the of the buffer
 * @param lenght        Return pointer to the lenght of the dequeue buffer
 * @param buffer_id     Return pointer to thehe buffer id of the dequeued buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
/*
errval_t device_queue_dequeue(struct device_queue *q,
                              regionid_t* region_id,
                              lpaddr_t* base,
                              size_t* length,
                              bufferid_t* buffer_id)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}
*/
/**
 * @brief dequeue a buffer from the device queue
 *
 * @param q             The device queue to call the operation on
 * @param buf           Return pointer to the dequeued buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t device_queue_dequeue(struct device_queue *q,
                              struct device_queue_buffer** buf)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}

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
errval_t device_queue_register(struct device_queue *q,
                               struct capref cap,
                               regionid_t* region_id)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}

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
errval_t device_queue_deregister(struct device_queue *q,
                                 regionid_t region_id,
                                 struct capref* cap)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}
/**
* @brief Send a notification about new buffers on the queue
*
* @param q      The device queue to call the operation on
*
* @returns error on failure or SYS_ERR_OK on success
*
*/
errval_t device_queue_sync(struct device_queue *q)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}
/**
* @brief Send a control message to the device queue
*
* @param q      The device queue to call the operation on
* @param ctrl   A sting encoding the control message
*
* @returns error on failure or SYS_ERR_OK on success
*
*/
errval_t device_queue_control(struct device_queue *q,
                              char* ctrl)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}
