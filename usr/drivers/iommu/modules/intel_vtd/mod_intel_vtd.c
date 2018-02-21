/*
 * Copyright (c) 2018, ETH Zurich.
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

#include <dev/vtd_dev.h>
#include <dev/vtd_iotlb_dev.h>

#define DRIVER_DEBUG(x...) debug_printf("[iommu] " x)


#define VTD_NUM_ROOT_ENTRIES	512

typedef enum {
    VTD_ENTRY_TYPE_BASE,
    VTD_ENTRY_TYPE_EXTENDED,
} vtd_entry_type_t;

union vtd_root_table {
    vtd_root_entry_array_t      *base;
    vtd_ext_root_entry_array_t  *extended;
};

union vtd_context_table {
    vtd_context_entry_array_t     *base [VTD_NUM_ROOT_ENTRIES];
    vtd_ext_context_entry_array_t *extended[VTD_NUM_ROOT_ENTRIES];
};

struct vtd {
    struct {
        vtd_t               vtd;
        vtd_iotlb_t         iotlb;
    } registers;

    uint16_t                pci_segment;
    vtd_entry_type_t        entry_type;

    /* The root table */
    struct capref           root_table_cap;
    union vtd_root_table    root_table;

    /* the context descriptor tables */
    struct capref           context_table_caps[VTD_NUM_ROOT_ENTRIES];
    union vtd_context_table context_table;
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
    errval_t err;

    DRIVER_DEBUG("Initialize: %s\n", name);

    if (capref_is_null(caps[0])) {
        return DRIVERKIT_ERR_NO_CAP_FOUND;
    }

    struct vtd *vtd = calloc(sizeof(*vtd), 1);
    if (vtd == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    /* map the registers */
    void *registers_vbase;
    err = vspace_map_one_frame_attr(&registers_vbase, BASE_PAGE_SIZE, caps[0],
                                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        goto err_out;
    }
    DRIVER_DEBUG("Registers mapped at: %p\n", registers_vbase);

    vtd_initialize(&vtd->registers.vtd, registers_vbase);
    size_t iro = vtd_ECAP_iro_rdf(&vtd->registers.vtd);

    vtd_iotlb_initialize(&vtd->registers.iotlb, registers_vbase + iro);

    DRIVER_DEBUG("Mackerel initalized\n");

    /* TODO: set these according to the record or args */
    vtd->entry_type = VTD_ENTRY_TYPE_BASE;
    vtd->pci_segment = 0;


    bfi->dstate = vtd;


    // 3. Set iref of your exported service (this is reported back to Kaluga)
    *dev = 0x00;

    return SYS_ERR_OK;

err_out:
    free(vtd);
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


    return SYS_ERR_OK;
}

/**
 * Registers the driver module with the system.
 *
 * To link this particular module in your driver domain,
 * add it to the addModules list in the Hakefile.
 */
DEFINE_MODULE(iommu_intel_module, init, attach, detach, set_sleep_level, destroy);
