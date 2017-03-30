/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/nameservice_client.h>
#include <devif/queue_interface.h>

#include "region_pool.h"
#include "dqi_debug.h"
#include "queue_interface_internal.h"


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
 * @param valid_data    Offset into the region where the valid data of this buffer
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
                      genoffset_t length,
                      genoffset_t valid_data,
                      genoffset_t valid_length,
                      uint64_t misc_flags)
{
    assert(q != NULL);
    errval_t err;
    
    // check if the buffer to enqueue is valid
    if (!region_pool_buffer_check_bounds(q->pool, region_id, offset,
        length, valid_data, valid_length)) {
        return DEVQ_ERR_INVALID_BUFFER_ARGS;
    }


    err = q->f.enq(q, region_id, offset, length, valid_data,
                   valid_length, misc_flags);

    DQI_DEBUG("Enqueue q=%p rid=%d, offset=%lu, lenght=%lu, err=%s \n",
              q, region_id, offset, length, err_getstring(err));

    return err;
}

/**
 * @brief dequeue a buffer from the device queue
 *
 * @param q             The device queue to call the operation on
 * @param region_id     Return pointer to the id of the memory
 *                      region the buffer belongs to
 * @param region_offset Return pointer to the offset into the region where
 *                      this buffer starts.
 * @param lenght        Return pointer to the lenght of the dequeue buffer
 * @param valid_data    Return pointer to an offset into the region where the
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
                      genoffset_t* length,
                      genoffset_t* valid_data,
                      genoffset_t* valid_length,
                      uint64_t* misc_flags)
{
    errval_t err;

    assert(q != NULL);
    assert(offset != NULL);
    assert(length != NULL);

    err = q->f.deq(q, region_id, offset, length, valid_data,
                   valid_length, misc_flags);
    if (err_is_fail(err)) {
        return err;
    }

    // check if the dequeue buffer is valid
    if (!region_pool_buffer_check_bounds(q->pool, *region_id, *offset,
        *length, *valid_data, *valid_length)) {
        return DEVQ_ERR_INVALID_BUFFER_ARGS;
    }

    DQI_DEBUG("Dequeue q=%p rid=%d, bid=%d \n", q, *region_id, *buffer_id);

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
    errval_t err;

    err = region_pool_add_region(q->pool, cap, region_id);
    if (err_is_fail(err)) {
        return err;
    }

    DQI_DEBUG("register q=%p, cap=%p, regionid=%d \n", (void*) q,
              (void*) &cap, *region_id);

    err = q->f.reg(q, cap, *region_id);

    return err;
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
    errval_t err;
    
    err = region_pool_remove_region(q->pool, region_id, cap);
    if (err_is_fail(err)) {
        return err;
    }
    DQI_DEBUG("deregister q=%p, cap=%p, regionid=%d \n", (void*) q,
              (void*) cap, region_id);
    
    err = q->f.dereg(q, region_id);

    return err;
}

/**
 * @brief Send a notification about new buffers on the queue
 *        Does nothing for direct queues.
 *
 * @param q      The device queue to call the operation on
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_notify(struct devq *q)
{
    errval_t err;
  
    err = q->f.notify(q);

    return err;
}

/**
 * @brief Enforce coherency between of the buffers in the queue
 *        by either flushing the cache or invalidating it
 *
 * @param q      The device queue to call the operation on
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_prepare(struct devq *q)
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
                      uint64_t value,
                      uint64_t *result)
{
    errval_t err;

    err = q->f.ctrl(q, request, value, result);

    return err;

}

void devq_set_state(struct devq *q, void *state)
{
    q->state = state;
}

void * devq_get_state(struct devq *q)
{
    return q->state;
}
