/**
 * \file
 * \brief This code shall be intantiated as a service within kaluga.
 * It's main purpose is to have a service where the applications network
 * stack can requests endpoints of NIC drivers. From the NIC drivers the applicatons
 * network stack can request to initalized a queue and in turn receives the  
 * resources to manage this queue. 
 *
 */

/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <collections/list.h>
#include <driverkit/driverkit.h>
#include <driverkit/control.h>
#include <queue_service/queue_service.h>

#include <if/queue_service_defs.h>

#include "debug.h"
#include "qs_internal.h"

struct queue_service_state {
    char* name;
    bool is_done;
    errval_t err;
    collections_listnode* ep_factory_list;
    struct slab_allocator alloc;
};

struct ep_factory_state {
    char name[256];
    coreid_t core;
    struct driver_instance* factory_ep;
};  


// the full name has to be correct
static int32_t check_name_full(void *data, void *arg)
{
    char* name1 = (char*) data;
    char* name2 = (char*) arg;
    
    return !strncmp(name1, name2, strlen(name1));
}


// checks also if first part is isimilar i.e. e10k and e10k:8086....
static int32_t check_name_part(void *data, void *arg)
{
    char* name1 = (char*) data;
    char* name2 = (char*) arg;
    
    return !strncmp(name2, name1, strlen(name2));
}

static errval_t request_queue_rpc(struct queue_service_binding *b, coreid_t coreid,
                                  struct capref* ep, errval_t* err)
{
    *err = SYS_ERR_OK;
    *ep = NULL_CAP;
    QUEUE_SERVICE_DEBUG("%s:%s:%d: Request queue rpc \n", __FILE__, __FUNCTION__, __LINE__);

    return SYS_ERR_OK;
}

static void request_queue(struct queue_service_binding *b, coreid_t coreid)
{
    errval_t err;
    struct capref ep;
    QUEUE_SERVICE_DEBUG("%s:%s:%d: Request queue\n", __FILE__, __FUNCTION__, __LINE__);
    
    err = request_queue_rpc(b, coreid, &ep, &err);
    
    err = b->tx_vtbl.request_queue_with_constraints_response(b, NOP_CONT, ep, err);
    assert(err_is_ok(err));
}


static errval_t request_queue_with_constraints_rpc(struct queue_service_binding *b, coreid_t coreid,
                                                   struct capref* ep, errval_t* err)
{
    *err = SYS_ERR_OK;
    *ep = NULL_CAP;
    QUEUE_SERVICE_DEBUG("%s:%s:%d: Request queue with constraints rpc \n", __FILE__, __FUNCTION__, __LINE__);

    return SYS_ERR_OK;
}

static void request_queue_with_constraints(struct queue_service_binding *b, coreid_t coreid)
{
    errval_t err;
    struct capref ep;
    QUEUE_SERVICE_DEBUG("%s:%s:%d: Request queue with constraints \n", __FILE__, __FUNCTION__, __LINE__);
    
    err = request_queue_with_constraints_rpc(b, coreid, &ep, &err);
    
    err = b->tx_vtbl.request_queue_with_constraints_response(b, NOP_CONT, ep, err);
    assert(err_is_ok(err));
}

static errval_t request_queue_by_name_rpc(struct queue_service_binding *b, 
                                          const char* name, coreid_t coreid,
                                          struct capref* ep, errval_t* err)
{
    *err = SYS_ERR_OK;

    QUEUE_SERVICE_DEBUG("%s:%s:%d: Request queue by name rpc \n", __FILE__, __FUNCTION__, __LINE__);

    struct queue_service_state* st = (struct queue_service_state*) b->st;

    struct ep_factory_state* factory_ep = (struct ep_factory_state*) 
                                          collections_list_find_if(st->ep_factory_list, 
                                                                   check_name_part, (char*) name);

    if (factory_ep == NULL) {
        *err = QSERVICE_ERR_NOT_FOUND;
        return *err;
    }

    *err = driverkit_get_driver_ep_cap(factory_ep->factory_ep, ep, 
                                       (coreid == factory_ep->core));
    if (err_is_fail(*err)) {
        return *err;
    }

    return SYS_ERR_OK;
}


static void request_queue_by_name(struct queue_service_binding *b, const char* name, coreid_t core)
{
    errval_t err;
    struct capref ep;
    QUEUE_SERVICE_DEBUG("%s:%s:%d: Request queue by name \n", __FILE__, __FUNCTION__, __LINE__);
    
    err = request_queue_by_name_rpc(b, name, core, &ep, &err);
    
    err = b->tx_vtbl.request_queue_by_name_response(b, NOP_CONT, ep, err);
    assert(err_is_ok(err));
}

static struct queue_service_rx_vtbl rx_vtbl = {
    .request_queue_call = request_queue,
    .request_queue_with_constraints_call = request_queue_with_constraints,
    .request_queue_by_name_call = request_queue_by_name
};

static struct queue_service_rpc_rx_vtbl rpc_rx_vtbl = {
    .request_queue_call = request_queue_rpc,
    .request_queue_with_constraints_call = request_queue_with_constraints_rpc,
    .request_queue_by_name_call = request_queue_by_name_rpc
};

static void export_cb(void *st, errval_t err, iref_t iref)
{
    struct queue_service_state* state = (struct queue_service_state*) st;
    state->err = err;

    if (err_is_ok(err)) {
        if (state->name == NULL) {
            err = nameservice_register(DEFAULT_SERVICE_NAME, iref);
        } else {
            err = nameservice_register(state->name, iref);
        }
        state->err = err;
    }

    state->is_done = true;
}

static errval_t connect_cb(void *st, struct queue_service_binding *b)
{
    b->rx_vtbl = rx_vtbl;
    b->rpc_rx_vtbl = rpc_rx_vtbl;
    queue_service_rpc_client_init(b);

    // Set up continuation queue
    b->st = st;

    QUEUE_SERVICE_DEBUG("%s:%s:%d: New connection\n", __FILE__, __FUNCTION__, __LINE__);
    return SYS_ERR_OK;
}

/**
 * @brief initializes the queue service with a provided name
 *
 * @param st            returned queue service state handle
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_init(struct queue_service_state** state, char* name)
{
    struct queue_service_state* st = calloc(sizeof(struct queue_service_state), 1);
    if (st == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    collections_list_create(&st->ep_factory_list, NULL);

    st->err = SYS_ERR_OK;
    st->is_done = false;

    if (name != NULL) {
        st->name = malloc(strlen(name));
        strncpy(st->name, name, strlen(name));
    } else {
        st->name = NULL;
    }
    
    slab_init(&st->alloc, sizeof(struct ep_factory_state), slab_default_refill);

    errval_t err = queue_service_export(st, export_cb, connect_cb,
                                        get_default_waitset(), 
                                        IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        free(st);
        return err;
    }

    QUEUE_SERVICE_DEBUG("%s:%s:%d: Trying to export queue service interface\n", __FILE__, __FUNCTION__, __LINE__);

    while (!st->is_done) {
        messages_wait_and_handle_next();
    }

    *state = st;
    return st->err;
}

/**
 * @brief initializes the queue service with a default name
 *
 * @param st            returned queue service state handle
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_init_default(struct queue_service_state** st)
{
    QUEUE_SERVICE_DEBUG("%s:%s:%d: calling init default \n", __FILE__, __FUNCTION__, __LINE__);
    return queue_service_init(st, NULL);
}

/**
 * @brief Adds an endpoint factory to the queue service. The queu service can
 *        request endpoints form these factories which represent connectons
 *        to drivers on which a single queue can be requested.
 *
 * @param st            service state handle
 * @param name          factory name
 * @param core          core id on which the factory is running
 * @param factory       driver_instance struct of a endpoint factory 
 *                      (normally only available in kaluga)
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_add_ep_factory(struct queue_service_state* st, 
                                      char* factory_name,
                                      coreid_t core,
                                      struct driver_instance* factory)
{   
    if (strlen(factory_name) > MAX_NAME_LEN) {
        return QSERVICE_ERR_NAME;
    }


    struct ep_factory_state* epf_state = slab_alloc(&st->alloc);
 
    // check if aleady in list
    void* item = collections_list_find_if(st->ep_factory_list, check_name_full, 
                                          factory_name);   
    if (item == NULL) {
        QUEUE_SERVICE_DEBUG("%s:%s:%d: Add factory ep %s\n", __FILE__, __FUNCTION__, __LINE__, factory_name);
        strncpy(epf_state->name, factory_name, strlen(factory_name));
        epf_state->factory_ep = factory;
        epf_state->core = core;
        collections_list_insert_tail(st->ep_factory_list, epf_state);
    } else {
        QUEUE_SERVICE_DEBUG("%s:%s:%d: Already have an ep: %s\n", __FILE__, __FUNCTION__, __LINE__, factory_name);
        return QSERVICE_ERR_ALREADY_ADDED;
    }

    return SYS_ERR_OK;
};


/**
 * @brief Create an endpoint to the queue service itself
 *
 * @param st            service state handle
 * @param core          core on which the program requesting the ep runs
 * @param ep            returend endpoint of this queue service
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_create_self_ep(struct queue_service_state* st, 
                                      coreid_t core,
                                      struct capref* ep)
{   
    if (st == NULL) {
        return QSERVICE_ERR_INVALID_SERVICE;
    }

    errval_t err;
    struct queue_service_binding* b;
   
    QUEUE_SERVICE_DEBUG("%s:%s:%d: Create self ep\n", __FILE__, __FUNCTION__, __LINE__);
    err = queue_service_create_endpoint(core == disp_get_core_id()? 
                                        IDC_ENDPOINT_LMP: IDC_ENDPOINT_UMP, 
                                        &rx_vtbl, st,
                                        get_default_waitset(),
                                        IDC_ENDPOINT_FLAGS_DUMMY,
                                        &b, *ep);
    
    return err;
};
