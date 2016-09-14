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

#define MAX_DEVICE_NAME 256

#define ENDPOINT_TYPE_FORWARD 0x1
#define ENDPOINT_TYPE_BLOCK 0x3
#define ENDPOINT_TYPE_DEVICE 0x4
#define ENDPOINT_TYPE_DIRECT 0x5

// the user side of the queue
#define ENDPOINT_TYPE_USER 0xF
// only internal?
#define ENDPOINT_TYPE_FORWARD_TX 0x11


#define DEVQ_BUF_FLAG_TX 0x1
#define DEVQ_BUF_FLAG_RX 0x2
#define DEVQ_BUF_FLAG_TX_LAST 0x4
#define DEVQ_SETUP_FLAGS_DIRECT 0x1

#define DEVQ_FEATURE_DIRECT 0x1


typedef uint32_t regionid_t;
typedef uint32_t bufferid_t;

struct devq;

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

// These functions must be implemented by the driver which is using the library

 /**
  * @brief Setup function called on the device side
  *
  * @param coreid           The core the client side is running on
  * @param flags            Flags to prepare for create
  * @param features         Return pointer to the features of the device
  * @param default_qsize    Return pointer to the default hardware device 
  *                         queue size
  * @param default_bufsize  Return pointer to the default buffer size used by the
  *                         device
  * @param reconnect        Return pointer to a bool that inicates if the other 
  *                         other endpoint has to reconnect to another serivce
  *                         (i.e. reconnect from device_manager to queue_manager)
  * @param name             String of the service to reconnect to
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
typedef errval_t (*devq_setup_t)(uint32_t coreid, uint64_t flags,
                                 uint64_t *features, uint32_t* default_qsize, 
                                 uint32_t* default_bufsize, bool* reconnect, char* name);

 /**
  * @brief Create function that initializes the queue on the device side, or 
  *        for direct queues get all the necessary state so that direct function calls
  *        to the hardware registers can be used. To set this state up, 
  *        communication to other services might be necessary (e.g. device_manager).
  *        The pointer to the device specific state can be aquired by devq_get_state()
  *        
  * @param q         The device queue to create the device state from
  * @param flags     Flags that inidicate which features the queue should have
  *                  enabled
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
typedef errval_t (*devq_create_t)(struct devq *q, uint64_t flags);
 /**
  * @brief Destroy function that cleans up all the resouces used by the queue.
  *        Similar to create, for direct endpoint types further communication
  *        is necessary in this function
  *        
  * @param q         The device queue to destroy
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
typedef errval_t (*devq_destroy_t)(struct devq *q);
   
 /**
  * @brief Notifies the device of new descriptors in the queue. 
  *        On a notificaton, the device can dequeue num_slots descritpors
  *        from the queue. NOTE: Does nothing for direct queues since there
  *        is no other endpoint to notify! (i.e. it is the same process)
  *        
  * @param q         The device queue to destroy
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
typedef errval_t (*devq_notify_t) (struct devq *q, uint8_t num_slots);

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
typedef errval_t (*devq_control_t)(struct devq *q, uint64_t request,
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

// The functions that the device driver has to export
struct devq_func_pointer {
    devq_setup_t setup;
    devq_create_t create;
    devq_destroy_t destroy;
    devq_register_t reg;
    devq_deregister_t dereg;
    devq_control_t ctrl;
    devq_notify_t notify;
    devq_enqueue_t enq;
    devq_dequeue_t deq;
};

// The devif device state
struct endpoint_state {
    // device type
    uint8_t endpoint_type;
    // name of the endpoint
    char device_name[MAX_DEVICE_NAME];
    // features of the endpoint
    uint64_t features;
    // setup function pointer
    struct devq_func_pointer f;
    // bool to check export
    bool export_done;
    // binding
    struct devif_ctrl_binding* ctrl;
    struct devif_data_binding* data;
};

 /*
 * ===========================================================================
 * Device queue interface export (for devices)
 * ===========================================================================
 */
 /**
  * @brief exports the devq interface so others (client side) can connect
  *
  * @param s             An endpoint state containing all the information
  *                      of a device including function pointers
  *
  * @returns error on failure or SYS_ERR_OK on success
  */

errval_t devq_driver_export(struct endpoint_state* s);

/* ===========================================================================
 * Device queue creation and destruction
 * ===========================================================================
 */

 /**
  * @brief creates a queue 
  *
  * @param q             Return pointer to the devq (handle)
  * @param end           Endpoint state containing the function pointers
  * @param device_name   Device name of the device to which this queue belongs
  *                      (Driver itself is running in a separate process)
  * @param device_type   The type of the device
  * @param flags         Anything you can think of that makes sense for the device
  *                      and its driver?
  *
  * @returns error on failure or SYS_ERR_OK on success
  */

errval_t devq_create(struct devq **q,
                     struct endpoint_state* end,
                     char* device_name,
                     uint64_t flags);

 /**
  * @brief destroys the device queue
  *
  * @param q           The queue state to free (and the device queue to be 
                       shut down in the driver)
  *
  * @returns error on failure or SYS_ERR_OK on success
  */
errval_t devq_destroy(struct devq *q);


/*
 * ===========================================================================
 * Device specific state
 * ===========================================================================
 */

/**
 * @brief allocate device specific state of size bytes
 *
 * @param q           The device queue to allocate the state for
 * @param bytes       Size of the state to allocate
 *
 */
void devq_allocate_state(struct devq *q, size_t bytes);

/**
 * @brief get the device specific state for a queue
 *
 * @param q           The device queue to get the state for
 *
 * @returns void pointer to the defice specific state
 */
void* devq_get_state(struct devq *q);


/**
 * @brief set the device specific state for a queue
 *
 * @param q           The device queue to get the state for
 * @param state       The device specific state
 *
 */
void devq_set_state(struct devq *q, void* state);

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


errval_t devq_event_loop(struct endpoint_state* s);   
#endif /* QUEUE_INTERFACE_H_ */
