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
#include "region_pool.h"
#include "dqi_debug.h"
 /*
 * ===========================================================================
 * Device queue creation and destruction
 * ===========================================================================
 */


 /**
  * @brief creates a queue 
  *
  * @param q             Return pointer to the devq (handle)
  * @param device_name   Device name of the device to which this queue belongs
  *                      (Driver itself is running in a separate process)
  * @param flags          Anything you can think of that makes sense for the device
  *                      and its driver?
  *
  * @returns error on failure or SYS_ERR_OK on success
  */

errval_t devq_create(struct devq **q,
                     char* device_name,
                     uint64_t flags)
{
    /*
    struct region_pool* pool;
    region_pool_init(&pool);
    struct capref cap;
    uint32_t region_ids[100];

    for (int i = 0; i < 100; i++) {
        region_ids[i] = -1;
    }

    srand(rdtsc());

    errval_t err = frame_alloc(&cap, 131072, NULL);
    assert(err_is_ok(err));
    
    void* va;
    err = vspace_map_one_frame_attr(&va, 131072, cap, 
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);

    for (int i = 0; i < 100; i++) {
        int id = rand() % 100;
        if (region_ids[id] == -1) {
            region_pool_add_region(pool, cap, 0x1222222, &region_ids[id]);
        } else {
            region_pool_remove_region(pool, region_ids[id], &cap);
            region_ids[id] = -1;
        }
    }

    // Buffer ids are from 0 to 16
    for (int i = 0; i < 100; i++) {
        if (region_ids[i] == -1) {      
            region_pool_add_region(pool, cap, 0x1222222, &region_ids[i]);
        }
    }

    for (int i = 0; i < 100; i++) {
        lpaddr_t addr;
        uint32_t buffer_id;
        for (int j = 0; j < 32; j++) {
            region_pool_get_buffer_from_region(pool, region_ids[i], &buffer_id, &addr);
        }
    
        for (int j = 0; j < 100; j++) {
            uint32_t index = rand() % 32;
            if (region_pool_buffer_of_region_in_use(pool, region_ids[i], index)) {
                region_pool_return_buffer_to_region(pool, region_ids[i], index);
            } else {
                region_pool_get_buffer_from_region(pool, region_ids[i], &index, &addr);
            }
        }
    }
    */   
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
errval_t devq_destroy(struct devq *qp)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}

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
 * @param base          Physical address of the start of the enqueued buffer
 * @param lenght        Lenght of the enqueued buffer
 * @param buffer_id     The buffer id of the enqueue buffer (TODO only for 
 *                      fixed size buffers?)
 * @param misc_flags    Any other argument that makes sense to the device queue
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_enqueue(struct devq *q,
                      regionid_t region_id,
                      lpaddr_t base,
                      size_t length,
                      bufferid_t buffer_id,
                      uint64_t misc_flags) 
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

errval_t devq_dequeue(struct devq *q,
                      regionid_t* region_id,
                      lpaddr_t* base,
                      size_t* length,
                      bufferid_t* buffer_id)
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
errval_t devq_register(struct devq *q,
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
errval_t devq_deregister(struct devq *q,
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
errval_t devq_sync(struct devq *q)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}

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
                      uint64_t value)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}
