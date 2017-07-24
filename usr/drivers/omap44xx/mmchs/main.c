/**
 * \file
 * \brief MMCHS Driver main routine.
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <driverkit/driverkit.h>

#include "mmchs.h"


static void cm2_connected(void *st, errval_t err, struct cm2_binding *b) {
    MMCHS_DEBUG("Connected to cm2 driver\n");
    assert(err_is_ok(err));
    struct mmchs_driver_state* dst = (struct mmchs_driver_state*) st;
    cm2_rpc_client_init(b);
    dst->cm2_binding = b;
}

static void twl6030_connected(void *st, errval_t err, struct twl6030_binding *b) {
    MMCHS_DEBUG("Connected to twl6030 driver\n");
    assert(err_is_ok(err));
    struct mmchs_driver_state* dst = (struct mmchs_driver_state*) st;
    twl6030_rpc_client_init(b);
    dst->twl6030_binding = b;
}


/**
 * Driver initialization function. This function is called by the driver domain
 * (see also 'create_handler' in ddomain_service.c).
 * Typically through a request from the device manager.
 *
 * The init function is supposed to set `dev` to the exported service iref.
 * The init function may use `bfi->dstate` to store additional state about the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \param[in]   name  The name of this driver instance.
 * \param[in]   flags Additional flags (The exact flags supported is device/driver specific).
 * \param[in]   c     Capabilities (for registers etc.) as provided by the device manager.
 *                    The exact layout of the `c` is device specific.
 * \param[out]  dev   The service iref over which the device can be contacted.
 *
 * \retval SYS_ERR_OK Device initialized successfully.
 * \retval LIB_ERR_MALLOC_FAIL Unable to allocate memory for the driver.
 */
static errval_t init(struct bfdriver_instance* bfi, const char* name, uint64_t flags,
                     struct capref* caps, size_t caps_len, char** args, size_t args_len, iref_t* dev) {
    MMCHS_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    bfi->dstate = malloc(sizeof(struct mmchs_driver_state));
    if (bfi->dstate == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    assert(bfi->dstate != NULL);
    struct mmchs_driver_state* st = (struct mmchs_driver_state*) bfi->dstate;
    st->caps = caps;

    // Connect to the cm2 driver
    iref_t cm2_iref;
    errval_t err = nameservice_lookup("cm2", &cm2_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "finding cm2 service failed.");
    }
    err = cm2_bind(cm2_iref, cm2_connected, st, get_default_waitset(), IDC_EXPORT_FLAG_NO_NOTIFY);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
    while(st->cm2_binding == NULL) {
        messages_wait_and_handle_next();
    }
    assert(st->cm2_binding != NULL);

    // Connect to the twl6030 driver
    iref_t twl6030_iref;
    err = nameservice_lookup("twl6030", &twl6030_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "finding cm2 service failed.");
    }
    err = twl6030_bind(twl6030_iref, twl6030_connected, st, get_default_waitset(), IDC_EXPORT_FLAG_NO_NOTIFY);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "call failed.");
    }
    while(st->twl6030_binding == NULL) {
        messages_wait_and_handle_next();
    }
    assert(st->twl6030_binding != NULL);

    // 1. Initialize the device:
    ctrlmod_init(st);
    err = st->cm2_binding->rpc_tx_vtbl.enable_hsmmc1(st->cm2_binding);
    assert(err_is_ok(err));
    sdmmc1_enable_power(st);
    mmchs_init(st);

    // 2. Export service to talk to the device:
    // FYI: Making this use THC+thread currently fails somewhere in the THC runtime
    mmchs_init_service(st, dev);

    return SYS_ERR_OK;
}

/**
 * Instructs driver to attach to the device.
 * This function is only called if the driver has previously detached
 * from the device (see also detach).
 *
 * \note After detachment the driver can not assume anything about the
 * configuration of the device.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t attach(struct bfdriver_instance* bfi) {
    MMCHS_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    return SYS_ERR_OK;
}

/**
 * Instructs driver to detach from the device.
 * The driver must yield any control over to the device after this function returns.
 * The device may be left in any state.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t detach(struct bfdriver_instance* bfi) {
    MMCHS_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    return SYS_ERR_OK;
}

/**
 * Instructs the driver to go in a particular sleep state.
 * Supported states are platform/device specific.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t set_sleep_level(struct bfdriver_instance* bfi, uint32_t level) {
    MMCHS_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    struct mmchs_driver_state* uds = bfi->dstate;
    uds->level = level;

    return SYS_ERR_OK;
}

/**
 * Destroys this driver instance. The driver will yield any
 * control over the device and free any state allocated.
 *
 * \param[in]   bfi   The instance of this driver.
 * \retval SYS_ERR_OK Device initialized successfully.
 */
static errval_t destroy(struct bfdriver_instance* bfi) {
    MMCHS_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);
    struct mmchs_driver_state* uds = bfi->dstate;
    free(uds);
    bfi->dstate = NULL;

    // XXX: Tear-down the service
    bfi->device = 0x0;

    return SYS_ERR_OK;
}

/**
 * Registers the driver module with the system.
 *
 * To link this particular module in your driver domain,
 * add it to the addModules list in the Hakefile.
 */
DEFINE_MODULE(mmchs, init, attach, detach, set_sleep_level, destroy);
