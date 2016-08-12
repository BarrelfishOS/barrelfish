/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <devif/queue_interface.h>
#include <if/devif_defs.h>

#include "region_pool.h"
#include "desc_queue.h"
#include "dqi_debug.h"


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

    // queues
    struct descq* rx;
    struct descq* tx;

    // out of band communication to endpoint
    struct devif_binding* b;       

    //TODO Other state needed ...
};

// Prototypes
// Init functions
static errval_t devq_init_forward(struct devq *q, uint64_t flags);
static errval_t devq_init_net(struct devq *q, uint64_t flags);
static errval_t devq_init_block(struct devq *q, uint64_t flags);
static errval_t devq_init_user(struct devq *q, uint64_t flags);

static struct devif_rx_vtbl rx_vtbl = {
    // TODO
};

// Message Passing functions
// ...
static void bind_cb(void *st, errval_t err, struct devif_binding *b)
{
    struct devq* q = (struct devq*) st;
    assert(err_is_ok(err));
    
    b->rx_vtbl = rx_vtbl;
    q->b = b;
}

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
                     uint8_t endpoint_type,
                     uint64_t flags)
{
    errval_t err;

    struct devq* tmp = malloc(sizeof(struct devq));
    strncpy(tmp->device_name, device_name, MAX_DEVICE_NAME);

    err = region_pool_init(&(tmp->pool));
    if (err_is_fail(err)) {
        free(tmp);
        return err;
    }

    /* 
       Each type of endpoint has a different init
       Devices like block/net have to export the devif interface
       so others can connect.
       If an endpoint is a user endpoint, it has to setup the 
       rx/tx channels to the other endpoint and connect to the 
       exported interface of the device.
       If the endpoint is forward, it hast to export the devif interface
       and setup the rx/tx channels
    */

    switch (endpoint_type) {
        case ENDPOINT_TYPE_FORWARD:
            err = devq_init_forward(tmp, flags);
            if (err_is_fail(err)) {
                return err;
            }
            break;
        case ENDPOINT_TYPE_BLOCK:
            err = devq_init_block(tmp, flags);
            if (err_is_fail(err)) {
                return err;
            }
            break;
        case ENDPOINT_TYPE_NET:
            err = devq_init_net(tmp, flags);
            if (err_is_fail(err)) {
                return err;
            }
            break;
        case ENDPOINT_TYPE_USER:
            err = devq_init_user(tmp, flags);
            if (err_is_fail(err)) {
                return err;
            }
            break;
        default:
            USER_PANIC("Devq: unknown device type \n");

    }
    
    *q = tmp;
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

    err = descq_destroy(q->rx);
    if (err_is_fail(err)) {
        return err;
    }

    err = descq_destroy(q->tx);
    if (err_is_fail(err)) {
        return err;
    }

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


static errval_t devq_init_forward(struct devq *q,
                                  uint64_t flags)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

static errval_t devq_init_net(struct devq *q,
                              uint64_t flags)
{
    
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}


static errval_t devq_init_block(struct devq *q,
                                uint64_t flags)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

static errval_t devq_init_user(struct devq *q,
                               uint64_t flags)
{
    errval_t err;
    struct capref rx;
    struct capref tx;

    // Allocate shared memory
    err = frame_alloc(&rx, DESCQ_DEFAULT_SIZE*DESCQ_ALIGNMENT, NULL); 
    if (err_is_fail(err)) {
        return err;
    }   

    err = frame_alloc(&tx, DESCQ_DEFAULT_SIZE*DESCQ_ALIGNMENT, NULL); 
    if (err_is_fail(err)) {
        return err;
    }   

    // Initialize rx/tx queues
    err = descq_init(&(q->rx), rx, DESCQ_DEFAULT_SIZE);
    if (err_is_fail(err)){
        return err;
    }   

    err = descq_init(&(q->tx), tx, DESCQ_DEFAULT_SIZE);
    if (err_is_fail(err)){
        return err;
    }
 
    iref_t iref;
    const char* suffix = "_devif";
    char name[strlen(q->device_name)+ strlen(suffix)+1];
    err = nameservice_blocking_lookup(name, &iref);
    if (err_is_fail(err)) {
        return err;
    }
    
    err = devif_bind(iref, bind_cb, q, get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    // TODO send the caps to the other endpoint  
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
    errval_t err;

    // Add buffer to used ones
    err = region_pool_get_buffer_id_from_region(q->pool, region_id, base,
                                                buffer_id);
    if (err_is_fail(err)) {
        return DEVQ_ERR_BUFFER_ID;
    }

    // Enqueue into queue
    err = descq_enqueue(q->tx, region_id, *buffer_id,
                        base, length, misc_flags);
    if (err_is_fail(err)) {
        return err;
    }

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
    errval_t err;

    // Dequeue descriptor from descriptor queue
    err = descq_dequeue(q->rx, region_id, buffer_id,
                        base, length, misc_flags);
    if (err_is_fail(err)) {
        return err;
    }

    // Add buffer to free ones
    err = region_pool_return_buffer_id_to_region(q->pool, *region_id,
                                                 *buffer_id);
    if (err_is_fail(err)) {
        return err;
    }

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
    DQI_DEBUG("register q=%p, cap=%p, regionid=%d \n", (void*) q, 
              (void*) &cap, *region_id);
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
    DQI_DEBUG("deregister q=%p, cap=%p, regionid=%d \n", (void*) q, 
              (void*) cap, region_id);
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

