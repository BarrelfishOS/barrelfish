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
#include <if/devif_defs.h>
#include <if/devif_rpcclient_defs.h>

#include "region_pool.h"
#include "desc_queue.h"
#include "dqi_debug.h"

enum devq_state {
    DEVQ_STATE_UNINITIALIZED,
    DEVQ_STATE_BINDING,
    DEVQ_STATE_CONNECTED,
    DEVQ_STATE_RECONNECTING,
};

/**
 * Represent the device queue itself
 */
struct devq {
    // Device state
    struct device_state* d;
    // Region management
    struct region_pool* pool;

    // queues
    struct capref rx_tx;
    struct descq* rx;
    struct descq* tx;

    // out of band communication to endpoint
    struct devif_binding* b;       
    struct devif_rpc_client* rpc; 
   
    // state of the queue
    enum devq_state state;
    
    // features
    uint64_t features;
    //TODO Other state needed ...
};


// Prototypes
// Init functions
static errval_t devq_init_forward(struct devq *q, uint64_t flags);
static errval_t devq_init_net(struct devq *q, uint64_t flags);
static errval_t devq_init_block(struct devq *q, uint64_t flags);
static errval_t devq_init_user(struct devq *q, uint64_t flags);

static errval_t devq_init_descqs(struct devq *q, struct capref rx_tx,
                                 size_t slots);

// Message Passing functions
// ...

static void mp_setup_request(struct devif_binding* b)
{
    DQI_DEBUG("setup_request\n");
    errval_t err;
    uint64_t features;
    bool reconnect;
    char name[MAX_DEVICE_NAME];

    struct device_state* state = (struct device_state*) b->st;

    // Call setup function on local device
    err = state->f.setup(&features, &reconnect, name);
    
    err = b->tx_vtbl.setup_response(b, NOP_CONT, features, reconnect, 
                                    name);
    assert(err_is_ok(err));
}

static bool done = false;
static void mp_create_reply(struct devif_binding* b, errval_t err)
{
    done = true;
    DQI_DEBUG("setup_reply");
}

static void mp_create_request(struct devif_binding* b, struct capref rx_tx, 
                              uint64_t flags, uint64_t size)
{
    errval_t err;
    DQI_DEBUG("create_request \n");
    struct device_state* state = (struct device_state* ) b->st;
        
    struct devq* q;    
    
    // Create the device queue
    err = devq_create(&q, state->device_name, state->device_type, 0);
    assert(err_is_ok(err));
    
    q->d = state;

    // Init queues
    q->rx_tx = rx_tx;
    err = devq_init_descqs(q, rx_tx, size);
    assert(err_is_ok(err));

    // Call create on device
    err = q->d->f.create(q, flags);    
    assert(err_is_ok(err));

    // Send response
    err = b->tx_vtbl.create_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}


static void mp_register_request(struct devif_binding* b,
                                struct capref mem, uint64_t size, 
                                lpaddr_t base, regionid_t region_id)
{

}

static void mp_register_reply(struct devif_binding* b, errval_t err)
{
    
}

static void mp_deregister_request(struct devif_binding* b, regionid_t region_id,
                                  uint64_t size, lpaddr_t base)
{

}

static void mp_deregister_reply(struct devif_binding* b, errval_t err)
{

}

static void mp_control_request(struct devif_binding* b, uint64_t cmd,
                               uint64_t value)
{
    printf("Control \n");
    b->tx_vtbl.control_response(b, NOP_CONT, SYS_ERR_OK);
}

static void mp_control_reply(struct devif_binding* b, errval_t err)
{

}

static void mp_notify(struct devif_binding* b, uint8_t num_slots)
{

}

static struct devif_rx_vtbl rx_vtbl = {
    // TODO
    .setup_call = mp_setup_request,
    //.setup_response = mp_setup_reply,
    .create_call = mp_create_request,
    .create_response = mp_create_reply,
    .reg_call = mp_register_request,
    .reg_response = mp_register_reply,
    .deregister_call = mp_deregister_request,
    .deregister_response = mp_deregister_reply,
    .control_call = mp_control_request,
    .control_response = mp_control_reply,
    .notify = mp_notify,
};

static void bind_cb(void *st, errval_t err, struct devif_binding *b)
{
    struct devq* q = (struct devq*) st;
    assert(err_is_ok(err));
    
    b->rx_vtbl = rx_vtbl;
    q->b = b;

    // Initi RPC client
    q->rpc = malloc(sizeof(struct devif_rpc_client));
    assert(q->rpc != NULL);

    err = devif_rpc_client_init(q->rpc, b);
    if (err_is_fail(err)) {
       free(q->rpc);
    }

    q->state = DEVQ_STATE_BINDING;
    DQI_DEBUG("Bound to interface \n");
}

static void export_cb(void *st, errval_t err, iref_t iref) 
{
    assert(err_is_ok(err));
    struct device_state* state = (struct device_state*) st;
    const char *suffix = "_devif";

    char name[strlen(state->device_name) + strlen(suffix) + 1];
    sprintf(name, "%s%s", state->device_name, suffix);

    err = nameservice_register(name, iref);
    assert(err_is_ok(err));

    DQI_DEBUG("Interface %s exported \n", name);
    state->export_done = true;
}

static errval_t connect_cb(void *st, struct devif_binding* b) 
{
    DQI_DEBUG("New connection on interface \n");
    b->rx_vtbl = rx_vtbl;
    b->st = st;
    return SYS_ERR_OK;
}

 /*
 * ===========================================================================
 * Device queue interface export (for devices)
 * ===========================================================================
 */
 /**
  * @brief exports the devq interface so others (client side) can connect
  *
  * @param device_name   Device name that is exported to the other endpoints.
  *                      Required by the "user" side to connect to
  * @param features      The features the driver exports
  *
  * @returns error on failure or SYS_ERR_OK on success
  */

errval_t devq_driver_export(struct device_state* s)
{
    errval_t err;
        
    err = devif_export(s, export_cb, connect_cb, get_default_waitset(),
                       IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DQI_DEBUG("Exporting devif interface failed \n");   
        return err;
    }

    while (!s->export_done) {
        event_dispatch(get_default_waitset());
    }

    return SYS_ERR_OK;
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
    // TODO do this in another way
    tmp->d = malloc(sizeof(struct device_state));
    strncpy(tmp->d->device_name, device_name, MAX_DEVICE_NAME);
    
    err = region_pool_init(&(tmp->pool));
    if (err_is_fail(err)) {
        free(tmp);
        return err;
    }
    
    tmp->state = DEVQ_STATE_UNINITIALIZED;

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
    return q->d->q;
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
    q->d->q = state;
}


static errval_t connect_to_if(struct devq *q, char* if_name)
{
    errval_t err;
    iref_t iref;

    const char* suffix = "_devif";
    char name[strlen(q->d->device_name)+ strlen(suffix)+1];
    sprintf(name, "%s%s", q->d->device_name, suffix);
    DQI_DEBUG("connecting to %s \n", name);

    err = nameservice_blocking_lookup(name, &iref);
    if (err_is_fail(err)) {
        return err;
    }
    
    // Bind to driver interface
    err = devif_bind(iref, bind_cb, q, get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    // wiat until bound
    while (q->state == DEVQ_STATE_UNINITIALIZED) {
        event_dispatch(get_default_waitset());  
    }

    return SYS_ERR_OK;
}

static errval_t devq_init_descqs(struct devq *q,
                                 struct capref rx_tx,
                                 size_t slots)
{
    errval_t err;
    struct frame_identity id;
    // Check if the frame is big enough
    err = invoke_frame_identify(rx_tx, &id);
    if (err_is_fail(err)) {
        return DEVQ_ERR_DESCQ_INIT;
    } 

    if (id.bytes < 2*DESCQ_ALIGNMENT*slots) {
        return DEVQ_ERR_DESCQ_INIT;
    }

    // TODO what about the non cache coherent case?
    void* rx_base;
    err = vspace_map_one_frame_attr((void**) &rx_base,
                                    2*slots*DESCQ_ALIGNMENT, rx_tx, 
                                    VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(err)) {
        return DEVQ_ERR_DESCQ_INIT;
    }

    err = descq_init(&q->rx, rx_base, slots);
    if (err_is_fail(err)) {
        // TODO cleanup mapped frame
        return DEVQ_ERR_DESCQ_INIT;
    }

    err = descq_init(&q->tx, rx_base + (DESCQ_ALIGNMENT*slots), slots);
    if (err_is_fail(err)) {
        // TODO cleanup mapped frame
        return DEVQ_ERR_DESCQ_INIT;
    }

    DQI_DEBUG("RX/TX queue mapped and initaialized, successfully\n");
    return SYS_ERR_OK;
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
    printf("devq_init_net NYI still continue\n");
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
    struct capref rx_tx;

    // Allocate shared memory
    err = frame_alloc(&rx_tx, 2*DESCQ_DEFAULT_SIZE*DESCQ_ALIGNMENT, NULL); 
    if (err_is_fail(err)) {
        return err;
    }   
    q->rx_tx = rx_tx;    

    err = devq_init_descqs(q, rx_tx, DESCQ_DEFAULT_SIZE);
    if (err_is_fail(err)) {
        return err;
    }
    
    err = connect_to_if(q, q->d->device_name);
    if (err_is_fail(err)) {
        return err;
    }

    // Do setup
    char lookup_name[MAX_DEVICE_NAME];
    bool reconnect;
    err = q->rpc->vtbl.setup(q->rpc, &q->features, &reconnect, lookup_name);
    if (err_is_fail(err)) {
        return err;
    }
    // might have to connect again to different process
    if (reconnect) {    
        DQI_DEBUG("Reconnecting \n");
        q->state = DEVQ_STATE_RECONNECTING;
        err = connect_to_if(q, lookup_name);
        if (err_is_fail(err)) {
            return err;
        }
    }
    q->state = DEVQ_STATE_CONNECTED;

    DQI_DEBUG("Start create \n");

    errval_t err2;
    err = q->rpc->vtbl.create(q->rpc, rx_tx, flags, DESCQ_DEFAULT_SIZE,
                              &err2);
    if (err_is_fail(err)) {
        return err;
    }

    DQI_DEBUG("End create \n");
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

