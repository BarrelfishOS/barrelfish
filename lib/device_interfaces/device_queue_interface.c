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
    struct region_pool* pool;
    region_pool_init(&pool);
    struct capref cap;
    uint32_t region_ids[100];
    
    for (int i = 0; i < 100; i++) {
        region_ids[i] = -1;
    }

    srand(rdtsc());

    for (int i = 0; i < 10000000; i++) {
        int id = rand() % 100;
        if (region_ids[id] == -1) {
            region_pool_add_region(pool, cap, 0x1222222, &region_ids[id]);
        } else {
            region_pool_remove_region(pool, region_ids[id], &cap);
            region_ids[id] = -1;
        }
    }
/*
    for (int i = 0; i < 32; i++) {
        region_pool_add_region(pool, cap, 0x1222222, &region_ids[i]);
        printf("Region_id[%d]=%d \n", i, region_ids[i]);
    }

    region_pool_remove_region(pool, region_ids[0], &cap);
    region_pool_remove_region(pool, region_ids[15], &cap);

    region_pool_add_region(pool, cap, 0x1222222, &region_ids[0]);
    region_pool_add_region(pool, cap, 0x1222222, &region_ids[15]);
*/
    //USER_PANIC("NIY\n");
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
