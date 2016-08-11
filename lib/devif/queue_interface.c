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

#define DESCQ_SIZE 64
#define DESCQ_ALIGNMENT 64

struct __attribute__((aligned(DESCQ_ALIGNMENT))) descriptor {
    regionid_t region_id; // 4
    bufferid_t buffer_id; // 8
    lpaddr_t base; // 16
    size_t length; // 24
    uint64_t misc_flags; // 32
    uint8_t pad[32];
};

struct devq_func_pointer {
    devq_create_t create;
    devq_destroy_t destroy;
    devq_register_t reg;
    devq_deregister_t dereg;
    devq_enqueue_t enq;
    devq_dequeue_t deq;
    devq_control_t ctrl;
    devq_notify_t notify;
};

/**
 * Represent the device queue itself
 */
struct devq {
    // device type
    uint8_t device_type;

    // name of the device
    char device_name[MAX_DEVICE_NAME];
    // pointer to device queue state
    void* q;
    // Region management
    struct region_pool* pool;

    // Function pointers for backend
    struct devq_func_pointer f;

    // queue state 
    uint16_t tx_head;
    uint16_t tx_tail;

    uint16_t rx_head;
    uint16_t rx_tail;

    // Queues themselves
    struct descriptor rx[DESCQ_SIZE];
    struct descriptor tx[DESCQ_SIZE];

    //TODO Other state needed ...
};

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
  * @param device_type   The type of the device
  * @param flags         Anything you can think of that makes sense for the device
  *                      and its driver?
  *
  * @returns error on failure or SYS_ERR_OK on success
  */

errval_t devq_create(struct devq **q,
                     char* device_name,
                     uint8_t device_type,
                     uint64_t flags)
{
    errval_t err;
    struct devq* tmp = malloc(sizeof(struct devq));
    strncpy(tmp->device_name, device_name, MAX_DEVICE_NAME);

    tmp->rx_head = 0;
    tmp->tx_head = 0;
    tmp->rx_tail = 0;
    tmp->tx_tail = 0;
    
    err = region_pool_init(&(tmp->pool));
    if (err_is_fail(err)) {
        free(tmp);
        return err;
    }

    switch (device_type) {
        case DEVICE_TYPE_BLOCK:
            break;
        case DEVICE_TYPE_NET:
            break;
        default:
            USER_PANIC("Devq: unknown device type \n");

    }
    // TODO initalize device 
    // TODO initalize device state
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
errval_t devq_destroy(struct devq *q)
{
    errval_t err;

    err = region_pool_destroy(q->pool);
    if (err_is_fail(err)) {
        return err;
    }

    free(q);
    return SYS_ERR_OK;
}


/**
 * @brief get the device specific state for a queue
 *
 * @param q           The device queue to get the state for
 *
 * @returns void pointer to the defice specific state
 */
void* devq_get_state(struct devq *q) 
{
    return q->q;
}


/**
 * @brief get the device specific state for a queue
 *
 * @param q           The device queue to set the state for
 * @param state       The state
 *
 * @returns void pointer to the defice specific state
 */
void devq_set_state(struct devq *q,
                    void* state)
{
    q->q = state;
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
 * @param misc_flags    Any other argument that makes sense to the device queue
 * @param buffer_id     Return pointer to buffer id of the enqueued buffer 
 *                      buffer_id is assigned by the interface
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */

errval_t devq_enqueue(struct devq *q,
                      regionid_t region_id,
                      lpaddr_t base,
                      size_t length,
                      uint64_t misc_flags,
                      bufferid_t* buffer_id)
{
    size_t num_free = 0;
    if (q->tx_head >= q->tx_tail) {
       num_free = DESCQ_SIZE - (q->tx_head - q->tx_tail);
    } else {
       num_free = DESCQ_SIZE - (q->tx_head + DESCQ_SIZE - q->tx_tail);
    }

    if (num_free == 0) {
        // TODO reasonable error
        return -1;
    }

    *buffer_id = 0;
    q->tx[q->tx_head].region_id = region_id;
    q->tx[q->tx_head].base = base;
    q->tx[q->tx_head].length = length;
    q->tx[q->tx_head].buffer_id = *buffer_id;
    q->tx[q->tx_head].misc_flags = misc_flags;
    q->tx_head = q->tx_head + 1 % DESCQ_SIZE;    

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
 * @param buffer_id     Return pointer to the buffer id of the dequeued buffer 
 * @param misc_flags    Return value from other endpoint
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */

errval_t devq_dequeue(struct devq *q,
                      regionid_t* region_id,
                      lpaddr_t* base,
                      size_t* length,
                      bufferid_t* buffer_id,
                      uint64_t* misc_flags)
{
    size_t num_used = 0;
    if (q->rx_head >= q->rx_tail) {
       num_used = (q->rx_head - q->rx_tail);
    } else {
       num_used = (q->rx_head + DESCQ_SIZE - q->rx_tail);
    }

    if (num_used == 0) {
        // TODO reasonable error
        return -1;
    }

    *region_id = q->rx[q->rx_head].region_id;
    *base = q->rx[q->rx_head].base;
    *length = q->rx[q->rx_head].length;
    *buffer_id = q->rx[q->rx_head].buffer_id;
    *misc_flags = q->rx[q->rx_head].misc_flags;

    q->rx_head = q->rx_head + 1 % DESCQ_SIZE;
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
    return err;
}

/**
 * @brief Send a notification about new buffers on the queue
 *
 * @param q      The device queue to call the operation on
 *
 * @returns error on failure or SYS_ERR_OK on success
 *
 */
errval_t devq_notify(struct devq *q)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
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
                      uint64_t value)
{
    USER_PANIC("NIY\n");
    return SYS_ERR_OK;
}

