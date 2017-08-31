/**
 * \file
 * \brief Driver module example template.
 *
 * In summary, every driver (struct bfdriver) shall implement the five
 * life-cycle functions init/set_sleep_level/attach/detach/destroy
 * (along with additional IRQ handler functions etc.).
 *
 * A driver module is linked with a driver domain (see main.c in this directory).
 * At runtime, a driver domain will instantiate a driver instance (struct bfdriver_instance)
 * of a given module (or class if you want) using the `init` function. During the lifetime
 * of a driver instance it may be `detached` from and re-`attach`-ed to the
 * device, set in different sleep levels, and finally it can be `destroy`-ed.
 *
 * For every driver instance (i.e., struct bfdriver_instance), a corresponding
 * control interface created and exported. The interface is defined in dcontrol.if,
 * the corresponding code is located in the driverkit library (dcontrol_service.c).
 *
 * \note For more information about driver domains check the main.c file in this
 * directory.
 */
/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>

#define DRIVER_DEBUG(x...) printf("[drv] " x)

struct uart_driver_state {
    uint32_t level;
};

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
    DRIVER_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    bfi->dstate = malloc(sizeof(struct uart_driver_state));
    if (bfi->dstate == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    assert(bfi->dstate != NULL);

    // 1. Initialize the device:

    // 2. Export service to talk to the device:

    // 3. Set iref of your exported service (this is reported back to Kaluga)
    *dev = 0x00;

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
    DRIVER_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

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
    DRIVER_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

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
    DRIVER_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);

    struct uart_driver_state* uds = bfi->dstate;
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
    DRIVER_DEBUG("%s:%s:%d: %s\n", __FILE__, __FUNCTION__, __LINE__, bfi->driver->name);
    struct uart_driver_state* uds = bfi->dstate;
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
DEFINE_MODULE(uart, init, attach, detach, set_sleep_level, destroy);
