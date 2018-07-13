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
#include <queue_service/client.h>

#include <if/queue_service_defs.h>

#include "debug.h"
#include "qs_internal.h"

struct queue_service_client {
    volatile bool connected;
    coreid_t core;
    struct capref* ep;
    struct queue_service_binding* b;
};  


static void bind_cb(void *st, errval_t err, struct queue_service_binding *b)
{
    struct queue_service_client* q = (struct queue_service_client*) st;
    assert(err_is_ok(err));

    b->st = q;
    q->b = b;
    queue_service_rpc_client_init(q->b);

    q->connected = true;

    QUEUE_SERVICE_DEBUG("%s:%s:%d: binding done\n", __FILE__, __FUNCTION__, __LINE__);
}

/**
 * @brief initializes the queue service client using an endpoint
 *
 * @param q            returned queue service client state handle
 * @param ep           the queue service endpoint to connect to
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */

errval_t queue_service_client_init_with_ep(struct queue_service_client** cl,
                                           struct capref* ep)
{
    errval_t err;

    struct queue_service_client* tmp = calloc(sizeof(struct queue_service_client), 1);
    if (tmp == NULL) {
        return LIB_ERR_MALLOC_FAIL; 
    }

    if (ep == NULL || capref_is_null(*ep)) {
        return QSERVICE_ERR_NO_VALID_EP;
    }
    
    tmp->connected = false;

    err = queue_service_bind_to_endpoint(*ep, bind_cb, tmp, get_default_waitset(),
                                         IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        goto out;
    }

    tmp->core = disp_get_core_id();
    tmp->ep = ep;

    while(!tmp->connected) {
        event_dispatch(get_default_waitset());
    }   
    
    *cl = tmp;

    return SYS_ERR_OK;
out:
    free(cl);
    return err;
}

/**
 * @brief initializes the queue service client using the nameservice
 *
 * @param q            returned queue service client state handle
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_client_init(struct queue_service_client** cl)
{
    errval_t err;
    iref_t iref;

    QUEUE_SERVICE_DEBUG("%s:%s:%d: nameservice lookup\n", __FILE__, __FUNCTION__, __LINE__);

    err = nameservice_blocking_lookup(DEFAULT_SERVICE_NAME, &iref);
    if (err_is_fail(err)) {
        return err; 
    }

    struct queue_service_client* tmp = calloc(sizeof(struct queue_service_client), 1);
    if (tmp == NULL) {
        return LIB_ERR_MALLOC_FAIL; 
    }
    
    tmp->core = disp_get_core_id();
    tmp->connected = false;

    QUEUE_SERVICE_DEBUG("%s:%s:%d: binding\n", __FILE__, __FUNCTION__, __LINE__);
    err = queue_service_bind(iref, bind_cb, tmp, get_default_waitset(),
                             IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        goto out; 
    }

    while(!tmp->connected) {
        event_dispatch(get_default_waitset());
    }   

    QUEUE_SERVICE_DEBUG("%s:%s:%d: connected\n", __FILE__, __FUNCTION__, __LINE__);
    *cl = tmp;

    return SYS_ERR_OK;
out:
    free(cl);
    return err;
}

/**
 * @brief requests a endpoint to a NIC by name. Using the endpoint the queue
 *        itself can be initalized. 
 *
 * @param cl            queue service client
 * @param name          name of the service we want an endpont from.
 * @param ep            returned endpoint, slot has to be allocated beforehand
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
errval_t queue_service_client_request_ep_by_name(struct queue_service_client* cl,
                                                 char* name, struct capref* ep)
{
    errval_t err, err2;

    if (cl == NULL || cl->b == NULL) {
        return QSERVICE_ERR_INVALID_CLIENT;
    }
 
    if (strlen(name) > MAX_NAME_LEN) {
        return QSERVICE_ERR_NAME;
    }
    
    QUEUE_SERVICE_DEBUG("%s:%s:%d: requesting queue by name %s \n", __FILE__, __FUNCTION__, __LINE__, name);
    err = cl->b->rpc_tx_vtbl.request_queue_by_name(cl->b, name, cl->core, ep, &err2);
    if (err_is_fail(err) || err_is_fail(err2)) {
        err = err_is_fail(err) ? err: err2;
        goto out;
    }

    return SYS_ERR_OK;
out:
    slot_free(*ep);
    return err;
}
