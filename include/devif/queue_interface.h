/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef QUEUE_INTERFACE_H_
#define QUEUE_INTERFACE_H_ 1


#include <barrelfish/barrelfish.h>

#define DEVQ_BUF_FLAG_TX 0x1
#define DEVQ_BUF_FLAG_RX 0x2
#define DEVQ_BUF_FLAG_TX_LAST 0x4



typedef uint32_t regionid_t;
typedef uint32_t bufferid_t;


struct devq;
struct region_pool;

// For convinience reason buffer descritpion in one struct
struct devq_buf{
    regionid_t rid; // 4
    bufferid_t bid; // 8
    lpaddr_t addr; // 16
    size_t len; // 24
    uint64_t flags; // 32
};

/*
 * ===========================================================================
 * Backend function definitions
 * ===========================================================================
 */
// Creation and Destruction of queues is device specific

 /**
  * @brief Notifies the device of new descriptors in the queue. 
  *        On a notificaton, the device can dequeue descritpors
  *        from the queue. NOTE: Does nothing for direct queues since there
  *        is no other endpoint to notify! (i.e. it is the same process)
  *        
  * @param q         The device queue
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
typedef errval_t (*devq_notify_t) (struct devq *q);

 /**
  * @brief Registers a memory region. For direct queues this function 
  *        Has to handle the communication to the device driver since
  *        there might also be a need to set up some local state for the
  *        direct queue that is device specific
  *        
  * @param q         The device queue handle
  * @param cap       The capability of the memory region
  * @param reigon_id The region id
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
typedef errval_t (*devq_register_t)(struct devq *q, struct capref cap,
                                    regionid_t region_id);

 /**
  * @brief Deregisters a memory region. (Similar communication constraints 
  *        as register)
  *        
  * @param q         The device queue handle
  * @param reigon_id The region id
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
typedef errval_t (*devq_deregister_t)(struct devq *q, regionid_t region_id);

 /**
  * @brief handles a control message to the device (Similar communication 
  *        constraints as register)
  *        
  * @param q         The device queue handle
  * @param request   The request type
  * @param value     The value to the request
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
typedef errval_t (*devq_control_t)(struct devq *q, 
                                   uint64_t request,
                                   uint64_t value);


 /**
  * @brief Directly enqueues something into a hardware queue. Only used by
  *        direct endpoints
  *        
  * @param q            The device queue handle
  * @param region_id    The region id of the buffer
  * @param buffer_id    The buffer id of the buffer
  * @param base         The base address of the buffer
  * @param length       The length of the buffer
  * @param misc_flags   Misc flags
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
typedef errval_t (*devq_enqueue_t)(struct devq *q, regionid_t region_id,
                                   bufferid_t buffer_id, lpaddr_t base, size_t length,
                                   uint64_t misc_flags);

 /**
  * @brief Directly dequeus something from a hardware queue. Only used by
  *        direct endpoints
  *        
  * @param q            The device queue handle
  * @param region_id    The region id of the buffer
  * @param buffer_id    The buffer id of the buffer
  * @param base         The base address of the buffer
  * @param length       The length of the buffer
  * @param misc_flags   Misc flags
  *
  * @returns error on failure if the queue is empty or SYS_ERR_OK on success
  */
typedef errval_t (*devq_dequeue_t)(struct devq *q, regionid_t* region_id,
                                   bufferid_t* buffer_id, lpaddr_t* base, size_t* length,
                                   uint64_t* misc_flags);

/**
 * Represent the device queue itself
 */


// The functions that the device driver has to export
struct devq_func_pointer {
    devq_register_t reg;
    devq_deregister_t dereg;
    devq_control_t ctrl;
    devq_notify_t notify;
    devq_enqueue_t enq;
    devq_dequeue_t deq;
};

struct devq {
    // Region management
    struct region_pool* pool;
 
    // Funciton pointers
    struct devq_func_pointer f;

    // exported devq
    /* Depending on the side of the channel (if there are two), 
       adding/removing regions and enqueueing/dequeueing buffers
       has to be handeled differently in the bookkeeping part
    */ 
    bool exp;
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
                      bufferid_t* buffer_id);

/**
 * @brief dequeue a buffer from the device queue
 *
 * @param q             The device queue to call the operation on
 * @param region_id     Return pointer to the id of the memory 
 *                      region the buffer belongs to
 * @param base          Return pointer to the physical address of 
 *                      the of the buffer
 * @param lenght        Return pointer to the lenght of the dequeue buffer
 * @param buffer_id     Reutrn pointer to the buffer id of the dequeued buffer 
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
                      uint64_t value);

#endif /* QUEUE_INTERFACE_H_ */
