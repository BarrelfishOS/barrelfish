/**
 * \file
 * \brief This code shall be used in Kaluga for receiving replies of various
 * driver domains and sending requests to driver domains.
 *
 * Note the client/service distinction can be confusing here. Altough, Kaluga
 * exports the service (for bootstrapping reasons and Barrelfish limitations
 * -> can't just ship an endpoint). We call this the client even tough
 * it exports the service. The ddomain_service.c which runs as part of
 * the driver domain connects to 'this file' at runtime.
 */

/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <driverkit/driverkit.h>
#include <if/ddomain_defs.h>

#include "debug.h"
#define SERVICE_NAME "ddomain_controller"

#define MAX_CAPS 6
#define MAX_ARGS 4

static collections_listnode* driver_domain_instances;

void ddomain_free_driver_inst(void* arg) {
    struct driver_instance* di = (struct driver_instance*) arg;
    free(di->driver_name);
    free(di->inst_name);
    free(di->args);
    free(di->caps);
    free(di);
}

struct domain_instance* ddomain_create_domain_instance(uint64_t id) {
    struct domain_instance* di = calloc(1, sizeof(struct domain_instance));
    assert(di != NULL);
    di->b = NULL;
    di->service_id = id;
    collections_list_create(&di->to_spawn, ddomain_free_driver_inst);
    collections_list_create(&di->spawned, ddomain_free_driver_inst);

    collections_list_insert(driver_domain_instances, di);
    return di;
}

struct driver_instance* ddomain_create_driver_instance(char* driver_name, char* inst_name) {
    struct driver_instance* di = calloc(sizeof(struct driver_instance), 1);
    assert(di != NULL);

    di->driver_name = strdup(driver_name);
    assert(di->driver_name);
    di->inst_name = strdup(inst_name);
    assert(di->inst_name);

    di->args = calloc(sizeof(char*), MAX_ARGS);
    assert(di->args);
    di->caps = calloc(sizeof(struct capref), MAX_CAPS);
    assert(di->caps);

    return di;
}

errval_t ddomain_driver_add_cap(struct driver_instance* drv, struct capref cap) {
    assert(drv != NULL);
    if (drv->cap_idx < MAX_CAPS) {
        drv->caps[drv->cap_idx++] = cap;
        return SYS_ERR_OK;
    }
    else {
        return DRIVERKIT_ERR_CAP_CAPACITY;
    }
}

static errval_t create_call(struct ddomain_binding *b, struct driver_instance* drv) {
    assert(b != NULL);
    assert(b->rpc_tx_vtbl.create != NULL);
    assert(drv != NULL);

    errval_t out_err = SYS_ERR_OK;
    DRIVERKIT_DEBUG("Trying to spawn %s (named %s)\n", drv->driver_name, drv->inst_name);
    errval_t err = b->rpc_tx_vtbl.create(b, drv->driver_name, strlen(drv->driver_name)+1,
                                          drv->inst_name, strlen(drv->inst_name)+1,
                                          drv->args[0], (drv->args[0] != NULL) ? strlen(drv->args[0]) : 0,
                                          drv->args[1], (drv->args[1] != NULL) ? strlen(drv->args[1]) : 0,
                                          drv->args[2], (drv->args[2] != NULL) ? strlen(drv->args[2]) : 0,
                                          drv->args[3], (drv->args[3] != NULL) ? strlen(drv->args[3]) : 0,
                                          drv->caps[0], drv->caps[1], drv->caps[2], drv->caps[3], drv->caps[4], drv->caps[5],
                                          drv->flags, &drv->dev, &drv->control, &out_err);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to create driver %s\n", drv->driver_name);
        return err;
    }
    else {
        printf("Driver %s created, reachable at [%"PRIuIREF", %"PRIuIREF"]\n", drv->driver_name, drv->dev, drv->control);
        return out_err;
    }
}

void ddomain_instantiate_driver(struct domain_instance* di, struct driver_instance* drv) {
    // Driver domain not up, make sure we spawn drivers later
    if (di->b == NULL) {
        collections_list_insert(di->to_spawn, drv);
    }
    // Driver domain up, spawn driver now
    else {
        errval_t err = create_call(di->b, drv);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "create driver instance failed.");
        }
        collections_list_insert(di->spawned, drv);
    }
}

void ddomain_free_domain_inst(void* arg) {
    struct domain_instance* di = (struct domain_instance*) arg;
    collections_list_release(di->to_spawn);
    collections_list_release(di->spawned);
}

static struct export_state {
    struct ddomain_binding* b;
    bool is_done;
    errval_t err;
} rpc_export;

static void rpc_export_cb(void *st, errval_t err, iref_t iref)
{
    rpc_export.is_done = true;
    rpc_export.err = err;

    if (err_is_ok(err)) {
        DRIVERKIT_DEBUG("Exported ddomain service with iref: %"PRIu32"\n", iref);
        err = nameservice_register(SERVICE_NAME, iref);
        assert(err_is_ok(err));
    }
}

static int32_t find_id(void* data, void* arg) {
    struct domain_instance* di = (void*) data;
    uint64_t id = (uint64_t)(uintptr_t) arg;
    return di->service_id == id;
}

static void ddomain_identify_handler(struct ddomain_binding* binding, uint64_t id) {
    DRIVERKIT_DEBUG("Got identify message %"PRIu64".\n", id);

    void* found = collections_list_find_if(driver_domain_instances, find_id, (void*)(uintptr_t) id);
    if (found) {
        DRIVERKIT_DEBUG("Found driver for id %"PRIu64"\n", id);
        struct domain_instance* di = (struct domain_instance*) found;
        assert(di->service_id == id);
        di->b = binding;

        void* removed;
        while ( (removed = collections_list_remove_ith_item(di->to_spawn, 0)) != NULL ) {
            struct driver_instance* driver = (struct driver_instance*) removed;
            DRIVERKIT_DEBUG("Trying to spawn %s\n", driver->inst_name);
            errval_t err = create_call(di->b, driver);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "Can't spawn driver intstance.");
            }
            collections_list_insert(di->spawned, driver);
        }
    }
    else {
        // XXX: Handle gracefully
        USER_PANIC("Unknown identify call from ddomain...");
    }
}

static const struct ddomain_rx_vtbl rpc_rx_vtbl = {
    .identify = ddomain_identify_handler,
};

static errval_t rpc_connect_cb(void *st, struct ddomain_binding *b)
{
    DRIVERKIT_DEBUG("%s:%s:%d: Got a new connection from driver domain.\n", __FILE__, __FUNCTION__, __LINE__);
    rpc_export.b = b;

    b->rx_vtbl = rpc_rx_vtbl;
    // Make sure we can send RPC messages to the client (driver domain)
    // if we could send/pass endpoints caps directly this whole bootstrapping
    // mess would be easier.
    ddomain_rpc_client_init(b);

    // Set up continuation queue
    b->st = NULL;
    return SYS_ERR_OK;
}

errval_t ddomain_controller_init(void)
{
    collections_list_create(&driver_domain_instances, ddomain_free_domain_inst);

    rpc_export.err = SYS_ERR_OK;
    rpc_export.is_done = false;

    errval_t err = ddomain_export(&rpc_export, rpc_export_cb, rpc_connect_cb,
                                   get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        return err;
    }

    DRIVERKIT_DEBUG("%s:%s:%d: Trying to export ddomain interface\n", __FILE__, __FUNCTION__, __LINE__);
    // XXX: broken
    while (!rpc_export.is_done) {
        messages_wait_and_handle_next();
    }

    return rpc_export.err;
}
