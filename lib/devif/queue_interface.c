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
#include <if/devif_ctrl_defs.h>
#include <if/devif_ctrl_rpcclient_defs.h>
#include <if/devif_data_defs.h>

#include "region_pool.h"
#include "desc_queue.h"
#include "dqi_debug.h"

#define FORWARD_PREFIX_LEN 8

enum devq_state {
    DEVQ_STATE_UNINITIALIZED,
    DEVQ_STATE_BINDING_CTRL,
    DEVQ_STATE_BINDING_DATA,
    DEVQ_STATE_CONNECTED,
    DEVQ_STATE_RECONNECTING,
    DEVQ_STATE_READY,
};

/**
 * Represent the device queue itself
 */
struct devq {
    // Endpoint state
    struct endpoint_state* end;
    // The name of the endpoint on the other side
    char endpoint_name[MAX_DEVICE_NAME];
    // Region management
    struct region_pool* pool;

    // queues
    struct capref rx_cap;
    struct capref tx_cap;

    struct descq* rx;
    struct descq* tx;

    // out of band communication to endpoint
    struct devif_ctrl_binding* ctrl;       
    struct devif_ctrl_rpc_client* ctrl_rpc; 
    struct devif_data_binding* data;       
  
    // state of the queue
    enum devq_state state;
    // pointer to queue specific state
    void* q;
    
    // setup 
    bool reconnect;
    char reconnect_name[MAX_DEVICE_NAME];
    // rpc error
    errval_t err;

    // default device state (mostly for direct interface)
    uint32_t q_size;
    uint32_t buf_size;
    
    //TODO Other state needed ...
};


// Prototypes
// Init functions
static errval_t devq_init_forward(struct devq *q, uint64_t flags);
static errval_t devq_init_user(struct devq *q, uint64_t flags);
static errval_t devq_init_direct(struct devq *q, uint64_t flags);

static errval_t devq_init_descqs(struct devq *q, struct capref rx, 
                                 struct capref tx, size_t slots);

// Message Passing functions
// ...

static void mp_setup_request(struct devif_ctrl_binding* b)
{
    DQI_DEBUG("setup_request\n");
    errval_t err;
    uint64_t features;
    uint32_t default_qsize, default_bufsize;
    bool reconnect;
    char name[MAX_DEVICE_NAME];

    struct endpoint_state* state = (struct endpoint_state*) b->st;

    // Call setup function on local device
    if (state->endpoint_type == ENDPOINT_TYPE_FORWARD) {
        // TODO forward default size etc. 
        reconnect = false;
    } else {
        err = state->f.setup(&features, &default_qsize, &default_bufsize, 
                             &reconnect, name);
        assert(err_is_ok(err));    
    }

    err = b->tx_vtbl.setup_response(b, NOP_CONT, features, default_qsize,
                                    default_bufsize, reconnect, 
                                    name);
    assert(err_is_ok(err));
}


static void mp_create(struct devif_ctrl_binding* b, struct capref rx, 
                      struct capref tx, uint64_t flags, uint64_t size)
{
    errval_t err;
    struct endpoint_state* state = (struct endpoint_state* ) b->st;
        
    struct devq* q;    
    
    // Create the device queue
    // TODO we might need to know the name of the 
    // endpoint that connected to the device

    if (state->endpoint_type != ENDPOINT_TYPE_FORWARD) {
        err = devq_create(&q, state, "", 0);
        if (err_is_fail(err)) {   
            err = b->tx_vtbl.create_response(b, NOP_CONT, err);
            assert(err_is_ok(err));
        }
    } else {

        struct devq* q2;    
        state->endpoint_type = ENDPOINT_TYPE_DEVICE;
        err = devq_create(&q, state, "", 0);
        if (err_is_fail(err)) {   
            err = b->tx_vtbl.create_response(b, NOP_CONT, err);
            assert(err_is_ok(err));
        }

        state->endpoint_type = ENDPOINT_TYPE_USER;
        err = devq_create(&q2, state, state->device_name+FORWARD_PREFIX_LEN, 0);
        if (err_is_fail(err)) {   
            err = b->tx_vtbl.create_response(b, NOP_CONT, err);
            assert(err_is_ok(err));
        }
        state->endpoint_type = ENDPOINT_TYPE_FORWARD;
        q->q = q2;
    }
    
    q->end = state;

    // Init queues (tx/rx are switched)
    err = devq_init_descqs(q, tx, rx, size);
    if (err_is_fail(err)) {   
        // TODO deallocate queues
        err = b->tx_vtbl.create_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
    }

    // Call create on device
    err = q->end->f.create(q, flags);    
    if (err_is_fail(err)) {   
        err = b->tx_vtbl.create_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
    }

    DQI_DEBUG("create_request %p \n", q);
    // Send response
    err = b->tx_vtbl.create_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
    b->st = q;
    q->data->st = q;
}

static void mp_register(struct devif_ctrl_binding* b, struct capref mem, 
                        regionid_t region_id)
{
    errval_t err;
    struct devq* q = (struct devq*) b->st;
   
    if (q->end->endpoint_type == ENDPOINT_TYPE_FORWARD) {
        // forward directly
        assert(!capcmp(mem, NULL_CAP));
        struct devq* tx = (struct devq*) q->q;

        DQI_DEBUG("Register top_q q=%p tx_q=%p Rid=%d \n", q, tx, region_id);
        err = tx->ctrl_rpc->vtbl.reg(tx->ctrl_rpc, mem, region_id, &tx->err);
        if (err_is_fail(tx->err) || err_is_fail(err)) {
            err = err_is_fail(err) ? err: tx->err;
            err = b->tx_vtbl.reg_response(b, NOP_CONT, err);
            assert(err_is_ok(err));
        }
        
        if (tx->q == NULL) {
            tx->q = q;
        }

    } else {

        DQI_DEBUG("Register q=%p Rid=%d \n", q, region_id);
        err = region_pool_add_region_with_id(q->pool, mem, region_id);
        if (err_is_fail(err)) {
            err = b->tx_vtbl.reg_response(b, NOP_CONT, err);
            assert(err_is_ok(err));
        }

    }

    err = q->end->f.reg(q, mem, region_id); 
    if (err_is_fail(err)) {
        err = b->tx_vtbl.reg_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
    }

    err = b->tx_vtbl.reg_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void mp_deregister(struct devif_ctrl_binding* b, regionid_t region_id)
{
    errval_t err;
    DQI_DEBUG("Deregister Rid=%d\n", region_id);
    struct devq* q = (struct devq*) b->st;
    struct capref* cap = NULL; 

 
    if (q->end->endpoint_type == ENDPOINT_TYPE_FORWARD) {
        // forward directly
        struct devq* tx = (struct devq*) q->q;

        err = tx->ctrl_rpc->vtbl.dereg(tx->ctrl_rpc, region_id, &tx->err);
        if (err_is_fail(tx->err) || err_is_fail(err)) {
            err = err_is_fail(err) ? err: tx->err;
            err = b->tx_vtbl.dereg_response(b, NOP_CONT, err);
            assert(err_is_ok(err));
        }
    
    } else {
        err = region_pool_remove_region(q->pool, region_id, cap);
        if (err_is_fail(err)) {
            err = b->tx_vtbl.dereg_response(b, NOP_CONT, err);
            assert(err_is_ok(err));
        }

    }

    err = q->end->f.dereg(q, region_id); 
    if (err_is_fail(err)) {
        err = b->tx_vtbl.dereg_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
    }

    err = b->tx_vtbl.dereg_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void mp_control(struct devif_ctrl_binding* b, uint64_t cmd,
                       uint64_t value)
{
    errval_t err;
    struct devq* q = (struct devq*) b->st;

    DQI_DEBUG("Control cmd=%lu value=%lu \n", cmd, value);
    if (q->end->endpoint_type == ENDPOINT_TYPE_FORWARD) {
        struct devq* tx = (struct devq*) q->q;
        err = tx->ctrl_rpc->vtbl.control(tx->ctrl_rpc, cmd, value, &tx->err);
        if (err_is_fail(err) || err_is_fail(tx->err)) {
            err = err_is_fail(err) ? err: tx->err;
            err = b->tx_vtbl.control_response(b, NOP_CONT, err);
            assert(err_is_ok(err));
        }

    }

    err = q->end->f.ctrl(q, cmd, value);
    if (err_is_fail(err)) {
        err = b->tx_vtbl.control_response(b, NOP_CONT, err);
        assert(err_is_ok(err));
    }   

    err = b->tx_vtbl.control_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

static void mp_notify(struct devif_data_binding* b, uint8_t num_slots)
{
    errval_t err;
    struct devq* q = (struct devq*) b->st;
    // Sending notification

    regionid_t region_id;
    bufferid_t buffer_id;
    lpaddr_t base;
    size_t len;
    uint64_t misc_flags;

    DQI_DEBUG("notify q=%p slots=%d endpoint %p \n", q, num_slots, q->end);
    
    if (q->end->endpoint_type == ENDPOINT_TYPE_FORWARD) {
        // forward user -> device:
        struct devq* tx = (struct devq*) q->q;

        if (tx->q == NULL) {
            tx->q = q;
        }

        DQI_DEBUG("notify forward q=%p slots=%d \n", tx, num_slots);
        for (int i = 0; i < num_slots; i++) {

            err = descq_dequeue(q->rx, &region_id, &buffer_id,
                                &base, &len, &misc_flags);
            assert(err_is_ok(err));

            // local function call
            err = q->end->f.enq(q, region_id, buffer_id, base, len , misc_flags);
            assert(err_is_ok(err));
            DQI_DEBUG("Dequeue q=%p rid=%d, bid=%d \n", q, region_id, buffer_id);
            // TODO call function locally to inform point (dequeue)
            err = descq_enqueue(tx->tx, region_id, buffer_id,
                                base, len, misc_flags);
            DQI_DEBUG("Enqueue q=%p rid=%d, bid=%d \n", tx, region_id, buffer_id);
            assert(err_is_ok(err));
        }

        descq_writeout_head(tx->tx);
        err = tx->data->tx_vtbl.notify(tx->data, NOP_CONT, num_slots);
        assert(err_is_ok(err));
    
        err = tx->end->f.notify(q, num_slots);
    } else if (q->end->endpoint_type == ENDPOINT_TYPE_FORWARD_TX) {
        // forward device -> user
        struct devq* tx = (struct devq*) q->q;
        DQI_DEBUG("notify forward_tx q=%p slots=%d \n", tx, num_slots);
        for (int i = 0; i < num_slots; i++) {
            err = descq_dequeue(q->rx, &region_id, &buffer_id,
                                &base, &len, &misc_flags);
            assert(err_is_ok(err));
            DQI_DEBUG("Dequeue q=%p rid=%d, bid=%d \n", q, region_id, buffer_id);

            // local function call // TODO only enqueue makes sense ? use flags?
            err = q->end->f.enq(q, region_id, buffer_id, base, len , misc_flags);
            assert(err_is_ok(err));

            err = descq_enqueue(tx->tx, region_id, buffer_id,
                                base, len, misc_flags);
            DQI_DEBUG("Enqueue q=%p rid=%d, bid=%d \n", tx, region_id, buffer_id);
            assert(err_is_ok(err));
        }

        descq_writeout_head(tx->tx);
        err = tx->data->tx_vtbl.notify(tx->data, NOP_CONT, num_slots);
        assert(err_is_ok(err));
   
        err = q->end->f.notify(q, num_slots);
    } else {
        err = q->end->f.notify(q, num_slots); 
        assert(err_is_ok(err));
        
        descq_writeout_tail(q->rx);
    }
}

static struct devif_ctrl_rx_vtbl rx_vtbl_ctrl = {
    .setup_call = mp_setup_request,
    .create_call= mp_create,
    .reg_call = mp_register,
    .dereg_call = mp_deregister,
    .control_call = mp_control,
};


static struct devif_data_rx_vtbl rx_vtbl_data = {
    .notify = mp_notify,
};

static void bind_cb_ctrl(void *st, errval_t err, struct devif_ctrl_binding *b)
{
    struct devq* q = (struct devq*) st;
    assert(err_is_ok(err));
    
    b->rx_vtbl = rx_vtbl_ctrl;
    b->st = q;
    q->ctrl = b;
    // Initi RPC client
    
    q->ctrl_rpc = malloc(sizeof(struct devif_ctrl_rpc_client));
    assert(q->ctrl_rpc != NULL);

    err = devif_ctrl_rpc_client_init(q->ctrl_rpc, b);
    if (err_is_fail(err)) {
       free(q->ctrl_rpc);
    }   
    
    q->state = DEVQ_STATE_BINDING_CTRL;
    DQI_DEBUG("Bound to interface \n");
}

static void bind_cb_data(void *st, errval_t err, struct devif_data_binding *b)
{
    struct devq* q = (struct devq*) st;
    assert(err_is_ok(err));
    
    b->rx_vtbl = rx_vtbl_data;
    b->st = q;
    q->data = b;
    // Initi RPC client
    
    q->state = DEVQ_STATE_BINDING_DATA;
    DQI_DEBUG("Bound to interface \n");
}

static void export_cb_ctrl(void *st, errval_t err, iref_t iref) 
{
    assert(err_is_ok(err));
    struct endpoint_state* state = (struct endpoint_state*) st;
    const char *suffix = "_devif_ctrl";

    char name[strlen(state->device_name) + strlen(suffix) + 1];
    sprintf(name, "%s%s", state->device_name, suffix);

    err = nameservice_register(name, iref);
    assert(err_is_ok(err));

    DQI_DEBUG("Interface %s exported \n", name);
}

// TODO try to not duplicate this?
static void export_cb_data(void *st, errval_t err, iref_t iref) 
{
    assert(err_is_ok(err));
    struct endpoint_state* state = (struct endpoint_state*) st;
    const char *suffix = "_devif_data";

    char name[strlen(state->device_name) + strlen(suffix) + 1];
    sprintf(name, "%s%s", state->device_name, suffix);

    err = nameservice_register(name, iref);
    assert(err_is_ok(err));

    DQI_DEBUG("Interface %s exported \n", name);
    state->export_done = true;
}

static errval_t connect_cb_ctrl(void *st, struct devif_ctrl_binding* b) 
{
    DQI_DEBUG("New connection on interface \n");
    struct endpoint_state* end = (struct endpoint_state*) st;    

    b->rx_vtbl = rx_vtbl_ctrl;
    b->st = st;
    // Set binding for bidirectional communication
    end->ctrl = b;
    return SYS_ERR_OK;
}


static errval_t connect_cb_data(void *st, struct devif_data_binding* b) 
{
    DQI_DEBUG("New connection on interface \n");
    struct endpoint_state* end = (struct endpoint_state*) st;    

    b->rx_vtbl = rx_vtbl_data;
    b->st = st;
    // Set binding for bidirectional communication
    end->data = b;
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

errval_t devq_driver_export(struct endpoint_state* s)
{
    DQI_DEBUG("Driver export with endpoint %p \n", s);   
    errval_t err;
    
    err = devif_ctrl_export(s, export_cb_ctrl, connect_cb_ctrl, get_default_waitset(),
                            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DQI_DEBUG("Exporting devif interface failed \n");   
        return err;
    }

    err = devif_data_export(s, export_cb_data, connect_cb_data, get_default_waitset(),
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
  * @param end           Endpoint state containing require function pointers and
  *                      some other info
  * @param endpoint_name Device name of the device to which this queue belongs
  *                      (Driver itself is running in a separate process)
  * @param flags         Anything you can think of that makes sense for the device
  *                      and its driver?
  *
  * @returns error on failure or SYS_ERR_OK on success
  */

errval_t devq_create(struct devq **q,
                     struct endpoint_state* end,
                     char* endpoint_name,
                     uint64_t flags)
{
    
    errval_t err;

    struct devq* tmp = malloc(sizeof(struct devq));
    tmp->end = end;

    DQI_DEBUG("start create %p\n", tmp);
    strncpy(tmp->endpoint_name, endpoint_name, MAX_DEVICE_NAME);

    err = region_pool_init(&(tmp->pool));
    if (err_is_fail(err)) {
        free(tmp);
        return err;
    }
    
    tmp->end = end;
    tmp->state = DEVQ_STATE_UNINITIALIZED;

    switch (tmp->end->endpoint_type) {
        case ENDPOINT_TYPE_FORWARD:
            err = devq_init_forward(tmp, flags);
            if (err_is_fail(err)) {
                return err;
            }
            break;
        case ENDPOINT_TYPE_DEVICE:
            // set bindings for communication back
            tmp->ctrl = tmp->end->ctrl;
            tmp->data = tmp->end->data;
            break;
        case ENDPOINT_TYPE_DIRECT:
            err = devq_init_direct(tmp, flags);
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
    
    tmp->state = DEVQ_STATE_READY;
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
 * @brief allocate device specific state of size bytes
 *
 * @param q           The device queue to allocate the state for
 * @param bytes       Size of the state to allocate
 *
 */
void devq_allocate_state(struct devq *q, size_t bytes)
{   
    q->q = calloc(1, bytes);
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

void devq_set_state(struct devq *q, void* state) 
{
    q->q = state;
}

static errval_t connect_to_if(struct devq *q, char* if_name, bool data)
{
    enum devq_state state;
    errval_t err;
    iref_t iref;

    char* suffix;
    if (data) {
        suffix = "_devif_data";
    } else {
        suffix = "_devif_ctrl";
    }

    char name[strlen(if_name) + strlen(suffix)+1];
    sprintf(name, "%s%s", if_name, suffix);
    DQI_DEBUG("connecting to %s \n", name);

    err = nameservice_blocking_lookup(name, &iref);
    if (err_is_fail(err)) {
        return err;
    }
    
    // Bind to driver interface
    if (data) {
        err = devif_data_bind(iref, bind_cb_data, q, get_default_waitset(),
                              IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            return err;
        }
        state = DEVQ_STATE_BINDING_DATA;
    } else {
        err = devif_ctrl_bind(iref, bind_cb_ctrl, q, get_default_waitset(),
                              IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            return err;
        }
        state = DEVQ_STATE_BINDING_CTRL;
    }

    // wiat until bound
    while (q->state != state) {
        event_dispatch(get_default_waitset());  
    }

    return SYS_ERR_OK;
}

static errval_t devq_init_descqs(struct devq *q,
                                 struct capref rx,
                                 struct capref tx,
                                 size_t slots)
{
    errval_t err;
    q->rx_cap = rx;
    q->tx_cap = tx;

    err = descq_init(&q->rx, rx, slots);
    if (err_is_fail(err)) {
        // TODO cleanup mapped frame
        return DEVQ_ERR_DESCQ_INIT;
    }

    err = descq_init(&q->tx, tx, slots);
    if (err_is_fail(err)) {
        // TODO cleanup mapped frame
        return DEVQ_ERR_DESCQ_INIT;
    }

    
    DQI_DEBUG("RX/TX queue mapped and initaialized, successfully" 
	      " q %p tx %p rx %p \n", q, q->tx, q->rx);
    return SYS_ERR_OK;
}

static errval_t devq_init_forward(struct devq *q,
                                  uint64_t flags)
{
    errval_t err;
    
    char* end_name = malloc(MAX_DEVICE_NAME);
    
    sprintf(end_name, "%s_%s", "forward", q->endpoint_name);
    strncpy(q->end->device_name, end_name, MAX_DEVICE_NAME);

    // export interface
    err = devif_ctrl_export(q->end, export_cb_ctrl, connect_cb_ctrl, get_default_waitset(),
                            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DQI_DEBUG("Exporting devif interface failed \n");   
        return err;
    }

    err = devif_data_export(q->end, export_cb_data, connect_cb_data, get_default_waitset(),
                            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DQI_DEBUG("Exporting devif interface failed \n");   
        return err;
    }

    DQI_DEBUG("Waiting for export done\n");   
    while (!q->end->export_done) {
        event_dispatch(get_default_waitset());
    }

    DQI_DEBUG("Export done\n");   

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

    err = devq_init_descqs(q, rx, tx, DESCQ_DEFAULT_SIZE);
    if (err_is_fail(err)) {
        return err;
    }
    
    err = connect_to_if(q, q->endpoint_name, false);
    if (err_is_fail(err)) {
        return err;
    }

    err = q->ctrl_rpc->vtbl.setup(q->ctrl_rpc, &q->end->features, &q->q_size, 
                                  &q->buf_size, &q->reconnect, q->reconnect_name);
    if (err_is_fail(err)) {
        return err;
    }

    // Do setup
    // might have to connect again to different process
    if (q->reconnect) {    
        DQI_DEBUG("Reconnecting \n");
        q->state = DEVQ_STATE_RECONNECTING;
        err = connect_to_if(q, q->reconnect_name, false);
        if (err_is_fail(err)) {
            return err;
        }
        
        err = connect_to_if(q, q->reconnect_name, true);
        if (err_is_fail(err)) {
            return err;
        }
        
    } else {
        err = connect_to_if(q, q->endpoint_name, true);
        if (err_is_fail(err)) {
            return err;
        }
    }


    q->state = DEVQ_STATE_CONNECTED;

    err = q->ctrl_rpc->vtbl.create(q->ctrl_rpc, rx, tx, flags, DESCQ_DEFAULT_SIZE,
                                   &q->err);
    if (err_is_fail(err) || err_is_fail(q->err)) {
        return err_is_fail(err) ? err: q->err;
    }

    return SYS_ERR_OK;
}

static errval_t devq_init_direct(struct devq *q,
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

    err = devq_init_descqs(q, rx, tx, DESCQ_DEFAULT_SIZE);
    if (err_is_fail(err)) {
        return err;
    }

    // need to get some stuff from the device itself
    err = connect_to_if(q, q->endpoint_name, false);
    if (err_is_fail(err)) {
        return err;
    }

    err = q->ctrl_rpc->vtbl.setup(q->ctrl_rpc, &q->end->features, &q->q_size, 
                                  &q->buf_size, &q->reconnect, q->reconnect_name);
    if (err_is_fail(err)) {
        return err;
    }
    err = q->end->f.create(q, flags);
    
    return err;
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

    /* In the user case we keep track of the buffers the user should not
       access. In the device case, we keep track of the buffers the device
       actually has access to.
    */
    if (q->end->endpoint_type == ENDPOINT_TYPE_USER || 
        q->end->endpoint_type == ENDPOINT_TYPE_DIRECT) {
        err = region_pool_get_buffer_id_from_region(q->pool, region_id, base,
                                                    buffer_id);
        if (err_is_fail(err)) {
            return DEVQ_ERR_BUFFER_ID;
        }
    } else {
        err = region_pool_return_buffer_to_region(q->pool, region_id,
                                                  base);
        if (err_is_fail(err)) {
            return DEVQ_ERR_BUFFER_ID;
        }
    }
    // Enqueue into queue
    if (q->end->endpoint_type == ENDPOINT_TYPE_DIRECT) {
        err = q->end->f.enq(q, region_id, *buffer_id, base, length, 
                            misc_flags);
    } else {
        err = descq_enqueue(q->tx, region_id, *buffer_id,
                            base, length, misc_flags);
    }

    DQI_DEBUG("Enqueue q=%p rid=%d, bid=%d \n", q, region_id, *buffer_id);

    return err;
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

    // Directly queue 
    if (q->end->endpoint_type == ENDPOINT_TYPE_DIRECT) {
        err = q->end->f.deq(q, region_id, buffer_id, base, length, 
                            misc_flags);
        if (err_is_fail(err)) {
            return err;
        }
    } else {

        // Dequeue descriptor from descriptor queue
        err = descq_dequeue(q->rx, region_id, buffer_id,
                            base, length, misc_flags);
        if (err_is_fail(err)) {
            return err;
        }
    }

    /* In the user case we keep track of the buffers the user should not
       access. In the device case, we keep track of the buffers the device
       actually has access to.
    */
    // Add buffer to free ones
    if (q->end->endpoint_type == ENDPOINT_TYPE_USER ||
        q->end->endpoint_type == ENDPOINT_TYPE_DIRECT) {
        err = region_pool_return_buffer_id_to_region(q->pool, *region_id,
                                                     *buffer_id);

        if (err_is_fail(err)) {
            return err;
        }
    } else {
        err = region_pool_set_buffer_id_from_region(q->pool, *region_id, *base,
                                                    *buffer_id);
        if (err_is_fail(err)) {
            return DEVQ_ERR_BUFFER_ID;
        }
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

    if (q->end->endpoint_type == ENDPOINT_TYPE_DIRECT) {
        /* Directly call it, if communication to the device
           driver is necessary it is most likely device specific
           and can not be handled in the devq library
        */
        err = q->end->f.reg(q, cap, *region_id);   
    } else {
        err = q->ctrl_rpc->vtbl.reg(q->ctrl_rpc, cap, *region_id, &q->err);
        if (err_is_fail(q->err)) {
            return q->err;
        }
    }
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
    
    if (q->end->endpoint_type == ENDPOINT_TYPE_DIRECT) {
        err = q->end->f.dereg(q, region_id);   
    } else {
        err = q->ctrl_rpc->vtbl.dereg(q->ctrl_rpc, region_id, &q->err);
        if (err_is_fail(q->err)) {
            return q->err;
        }
    }

    return err;
}

// Notify helper
struct pending_reply {
    uint8_t num_slots;
    struct devif_data_binding* b;
};

static void resend_notify(void* a)
{
    errval_t err;
    struct pending_reply* r = (struct pending_reply*) a;

    err = r->b->tx_vtbl.notify(r->b, NOP_CONT, r->num_slots);
    if (err_is_fail(err)) {
        err = r->b->register_send(r->b, get_default_waitset(),
                                  MKCONT(resend_notify, r));
        assert(err_is_ok(err));
    } else {
        free(r);
    }   

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
  
    if (q->end->endpoint_type == ENDPOINT_TYPE_DIRECT) {
        return SYS_ERR_OK;
    } else {
        // get number of full slots
        uint8_t num_slots = descq_full_slots(q->tx);
        DQI_DEBUG("notify called q=%p slots=%d \n",q, num_slots);
        descq_writeout_head(q->tx);

        err = q->data->tx_vtbl.notify(q->data, NOP_CONT, num_slots);
        // send notification
        if (err == FLOUNDER_ERR_TX_BUSY) {
            struct pending_reply* r = malloc(sizeof(struct pending_reply));
            r->num_slots = num_slots;
            r->b = q->data;

            err = q->data->register_send(q->data, get_default_waitset(),
                                      MKCONT(resend_notify, r));
            // If registering fails, dispatch event to get further with 
            // state and retry
            if (err_is_fail(err)) {
                while (true) {
                    err = r->b->register_send(r->b, get_default_waitset(),
                                              MKCONT(resend_notify, r));
                    if (err_is_ok(err)) {
                        break;
                    } else {
                        event_dispatch(get_default_waitset());
                    }
                }
            }
        } 
    }
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
    errval_t err;
    
    DQI_DEBUG("control request=%lu, value=%lu \n", request, value);
    if (q->end->endpoint_type == ENDPOINT_TYPE_DIRECT) {
        err = q->end->f.ctrl(q, request, value);
        return err;
    } else {
        err = q->ctrl_rpc->vtbl.control(q->ctrl_rpc, request, value, &q->err);
        if (err_is_fail(err) || err_is_fail(q->err)) {
            err = err_is_fail(err) ? err : q->err;
            return err;
        }
    }

    return q->err;
}


errval_t devq_event_loop(struct endpoint_state* s)
{   
    while(true) {
        event_dispatch(get_default_waitset());
    }
}
