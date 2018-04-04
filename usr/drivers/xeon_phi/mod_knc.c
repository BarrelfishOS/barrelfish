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
#include <driverkit/iommu.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_manager_client.h>

#include "xeon_phi_internal.h"
#include "smpt.h"
#include "dma_service.h"
#include "service.h"
#include "xphi_service.h"
#include "interphi.h"
#include "domain.h"
#include "sysmem_caps.h"



extern uint8_t xeon_phi_dma_enabled;
extern char *xeon_phi_mod_uri;
extern char *xeon_phi_mod_list;



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
static errval_t init(struct bfdriver_instance *bfi, uint64_t flags, iref_t* dev) {

    errval_t err;
    // 1. Initialize the device:

    debug_printf("[knc] attaching new co-processor\n");

    /* allocate the Xeon Phi state */
    struct xeon_phi *xphi = calloc(1, sizeof(*xphi));
    if (xphi == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct capref iommuep;
    err = driverkit_get_iommu_cap(bfi, &iommuep);
    if (err_is_fail(err)) {
        goto err_out;
    }

    err = driverkit_iommu_client_init_cl(iommuep, &xphi->iommu_client);
    if (err_is_fail(err)) {
        goto err_out;
    }

    debug_printf("[knc] iommu is %s.\n",
                 driverkit_iommu_present(xphi->iommu_client) ? "on" : "off");

    /* set the client flag to false */
    xphi->is_client = XEON_PHI_IS_CLIENT;
    xphi->state = XEON_PHI_STATE_NULL;

    debug_printf("[knc] obtaining the bar caps.\n");

    struct capref mmio;
    err = driverkit_get_bar_cap(bfi, 1, &mmio);
    if (err_is_fail(err)) {
        goto err_out2;
    }

    struct capref apt;
    err = driverkit_get_bar_cap(bfi, 0, &apt);
    if (err_is_fail(err)) {
        goto err_out2;
    }

    debug_printf("[knc] initialize the xeon phi\n");

    err = xeon_phi_init(xphi, mmio, apt);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not do the card initialization\n");
        goto err_out2;
    }

    debug_printf("[knc] booting it.\n");

    err = xeon_phi_boot(xphi, xeon_phi_mod_uri, xeon_phi_mod_list);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not boot the card\n");
        goto err_out2;
    }

#if 0
    err = service_init(xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not start the driver service\n");
    }

    uint8_t num;
    iref_t *irefs;
    err = xeon_phi_manager_client_register(xphi.iref, &xphi.id, &num, &irefs);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register with the Xeon Phi manager\n");
    }

#endif

    interphi_wait_for_client(xphi);

#if 0
    err = service_register(&xphi, irefs, num);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not register with the other drivers");
    }

#endif

    err = xdma_service_init(xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the DMA engine\n");
    }

    err = xeon_phi_service_init(xphi);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "could not initialize the messaging service");
    }

    #if 0
    /*
     * in case there are more than one Xeon Phi present in the system, indicated
     * by an id > 0, the driver will register itself with the other Xeon Phi
     * driver instances running in the system and initializes the inter-Phi
     * messaging frame
     */
    if (xphi.id != 0) {
        XDEBUG("Doing Intra Xeon Phi setup with %u other instances\n", xphi.id);
        for (uint32_t i = 0; i < xphi.id; ++i) {
            /* initialize the messaging frame */
            err = interphi_init_xphi(i, &xphi, NULL_CAP, XEON_PHI_IS_CLIENT);
            if (err_is_fail(err)) {
                XDEBUG("Could not initialize messaging\n");
                continue;
            }
        }
    }
    #endif

    char buf[20];
    snprintf(buf, 20, "xeon_phi.%u.ready", xphi->id);

    XDEBUG("registering ready\n");
    err = domain_register(buf, 0xcafebabe);
    assert(err_is_ok(err));

    /* signal for the test */
    debug_printf("Xeon Phi operational: %s\n", buf);

    XDEBUG("initialization done. Going into main message loop\n");

    // 2. Export service to talk to the device:

    // 3. Set iref of your exported service (this is reported back to Kaluga)
    *dev = 0x00;

    return SYS_ERR_OK;
    err_out2:
    driverkit_iommu_client_disconnect_cl(xphi->iommu_client);
    err_out:
    free(xphi);
    return err;
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

    // XXX: Tear-down the service
    bfi->device = 0x0;

    return SYS_ERR_OK;
}

static errval_t get_ep(struct bfdriver_instance* bfi, bool lmp, struct capref* ret_cap)
{
    return SYS_ERR_OK;
}

/**
 * Registers the driver module with the system.
 *
 * To link this particular module in your driver domain,
 * add it to the addModules list in the Hakefile.
 */
DEFINE_MODULE(knc_module, init, attach, detach, set_sleep_level, destroy, get_ep);
