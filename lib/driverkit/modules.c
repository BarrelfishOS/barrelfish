/**
 * \file
 * \brief Driverkit module implementation.
 *
 * Contians helper functions to iterate over driver modules in a domain
 * and create driver instances from driver modules.
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

#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>
#include <collections/hash_table.h>


#include "debug.h"

#pragma GCC diagnostic ignored "-Wdiscarded-qualifiers"

///< Points to start of the ELF section where the bfdriver structs are.
extern struct bfdriver bfdrivers_start[];
///< Points to the end of the ELF section where the bfdriver structs are.
extern struct bfdriver bfdrivers_end[];

static collections_listnode* instances = NULL;

/**
 * Initializes a pointer to point to the list of all bfdrivers.
 *
 * The driver structs are stored in a special ELF section (called .bfdrivers).
 * The custom link script of the driver domain makes sure there is a bfdrivers_start
 * and bfdrivers_end symbol at the beginning and end of that section.
 *
 * \param[out] start Initialized to point to the first bfdriver instance.
 * \param[out] num   How many drivers there are.
 */
void driverkit_list(struct bfdriver** start, size_t* num) {
    *start = (struct bfdriver*) bfdrivers_start;
    *num = ((ptrdiff_t)bfdrivers_end - (ptrdiff_t)bfdrivers_start) / sizeof(struct bfdriver);
}

/**
 * Finds a driver instance linked with the driver domain:
 * \param  name The name of the driver we're interested in.
 * \return      bfdriver instance with matching name or NULL in case not found.
 */
struct bfdriver* driverkit_lookup_cls(const char* name) {
    assert(name != NULL);

    size_t drivers = 0;
    struct bfdriver* cur = NULL;
    driverkit_list(&cur, &drivers);

    for (size_t i=0; i<drivers; i++) {
        if (strcmp(name, cur->name) == 0) {
            return cur;
        }
        cur += 1;
    }

    return NULL; // not found
}

/**
 * Frees a bfdriver instance. Callers need to make sure
 * that bfi->destroy was called beforehand to clean up
 * state that is owned by the driver.
 *
 * \param arg bfdriver instance.
 */
static void free_driver_instance(void* arg) {
    assert (arg != NULL);
    struct bfdriver_instance* bfi = (struct bfdriver_instance*) arg;
    free(bfi);
}

/**
 * Compare driver instance name with supplied argument.
 *
 * Helper function for collection_list_* data-structure.
 *
 * \param[in]  elem   bfdriver instance
 * \param[in]  name   String
 * \retval True   iff name == bfi->name
 * \retval False  iff name != bfi->name
 */
static int32_t match_name(void* elem, void* name) {
    assert(elem != NULL);
    assert(name != NULL);

    struct bfdriver_instance* bfi = (struct bfdriver_instance*) elem;
    assert (bfi->name != NULL);
    assert (name != NULL);
    if (strcmp((char*)name, bfi->name) == 0) {
        return 1;
    }
    else {
        return 0;
    }
}

/**
 * Destroys a driver instances identified by its name.
 * \todo various tricky service clean-up issues are simply ignored here.
 *
 * \param  name Name of the instance
 * \retval SYS_ER_OK Driver successfully destroyed.
 * \retval DRIVERKIT_ERR_DRIVER_DETACH detaching failed.
 */
errval_t driverkit_destroy(const char* name) {
    assert(name != NULL);
    if (instances == NULL) {
        collections_list_create(&instances, free_driver_instance);
    }

    void* namearg = (void*) name; // Get rid of the const because collections_* API is not specific enough...
    struct bfdriver_instance* bfi = collections_list_find_if(instances, match_name, namearg);
    errval_t err = bfi->driver->destroy(bfi);
    if (err_is_ok(err)) {
        struct bfdriver_instance* bfi2 = (struct bfdriver_instance*) collections_list_remove_if(instances, match_name, namearg);
        free_driver_instance(bfi2);
    }
    else {
        err = err_push(err, DRIVERKIT_ERR_DRIVER_DETACH);
    }

    return err;
}

/**
 * Destroys a driver instances identified by its name.
 * \todo various tricky service clean-up issues are simply ignored here.
 *
 * \param  name Name of the instance
 * \retval SYS_ER_OK Driver successfully destroyed.
 * \retval DRIVERKIT_ERR_DRIVER_DETACH detaching failed.
 */
/*
errval_t driverkit_get_ep(const char* name) {
    assert(name != NULL);
    if (instances == NULL) {
        collections_list_create(&instances, free_driver_instance);
    }

    void* namearg = (void*) name; // Get rid of the const because collections_* API is not specific enough...
    struct bfdriver_instance* bfi = collections_list_find_if(instances, match_name, namearg);
    errval_t err = bfi->driver->destroy(bfi);
    if (err_is_ok(err)) {
        struct bfdriver_instance* bfi2 = (struct bfdriver_instance*) collections_list_remove_if(instances, match_name, namearg);
        free_driver_instance(bfi2);
    }
    else {
        err = err_push(err, DRIVERKIT_ERR_DRIVER_DETACH);
    }

    return err;
}
*/

/**
 * Create a driver instance within the driver domain.
 *
 * \param[in]   cls     The class of driver (found in bfdriver).
 * \param[in]   name    The name of the driver instance.
 * \param[in]   caps    Caps provided to the driver's init function.
 * \param[in]   flags   Flags provided to the driver's init function.
 * \param[out]  device  iref of the device interface (as created by the device).
 * \param[out]  control endpoint cap of the control interface (created as part of this function).
 * \return      Error status of driver creation.
 */
errval_t driverkit_create_driver(const char* cls, struct bfdriver_instance *inst,
                                 uint64_t flags, iref_t* device, struct capref* control)
{
    assert(cls != NULL);
    assert(device != NULL);
    assert(inst != NULL);

    errval_t err = SYS_ERR_OK;

    struct bfdriver* drv = driverkit_lookup_cls(cls);
    if (drv == NULL) {
        return DRIVERKIT_ERR_NO_DRIVER_FOUND;
    }
    DRIVERKIT_DEBUG("Using driver %s for class %s\n", drv->name, cls);

#if defined(ENABLE_DRIVERKIT_DEBUG)
    {
        char caps[1024];
        for(int i=0; i<16; i++){
            struct capref cc = {
                .cnode = inst->argcn,
                .slot = i
            };
            debug_print_cap_at_capref(caps, sizeof(caps), cc);
            debug_printf("[dkit] cap %d = %s\n", i, caps);
        }
    }
#endif

    inst->driver = drv;

    err = drv->init(inst, flags, device);
    if (err_is_fail(err)) {
        //DRIVERKIT_DEBUG("Init returned error...\n");
        DEBUG_ERR(err, "Can't initialize the device");
        free_driver_instance(inst);
        return err_push(err, DRIVERKIT_ERR_DRIVER_INIT);
    }

    // Since Kaluga always has to be on core 0, we can do this ...
    err = dcontrol_service_init(inst, NULL, (disp_get_core_id() == 0), control);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Can't set-up control interface for device.");
        free_driver_instance(inst);
        return err_push(err, DRIVERKIT_ERR_CONTROL_SERVICE_INIT);
    }

    DRIVERKIT_DEBUG("Driver class %s initialized.\n", drv->name);
    if (instances == NULL) {
        collections_list_create(&instances, free_driver_instance);
    }
    collections_list_insert(instances, inst);

    return err;
}


errval_t driverkit_local_service_register(char* name, void* tbl)
{
    
    return SYS_ERR_OK;
}

void* driverkit_local_service_lookup(char* name)
{

    return NULL;
}

static errval_t get_cap(struct bfdriver_instance *bfi, cslot_t slot, struct capref *cap)
{
    if (slot >= DRIVERKIT_ARGCN_SLOT_MAX) {
        return DRIVERKIT_ERR_CAP_CAPACITY;
    }

    struct capref dest = {
        .cnode = bfi->argcn,
        .slot = slot
    };

    *cap = dest;

    return SYS_ERR_OK;
}

errval_t driverkit_get_interrupt_cap(struct bfdriver_instance *bfi, struct capref *cap)
{
    return get_cap(bfi, DRIVERKIT_ARGCN_SLOT_INT, cap);
}

errval_t driverkit_get_iommu_cap(struct bfdriver_instance *bfi, struct capref *cap)
{
    return get_cap(bfi, DRIVERKIT_ARGCN_SLOT_IOMMU, cap);
}

errval_t driverkit_get_pci_cap(struct bfdriver_instance *bfi, struct capref *cap)
{
    return get_cap(bfi, DRIVERKIT_ARGCN_SLOT_PCI_EP, cap);
}

errval_t driverkit_get_bar_cap(struct bfdriver_instance *bfi, uint8_t idx,
                               struct capref *cap)
{
    if (idx >= 6) {
        return DRIVERKIT_ERR_CAP_CAPACITY;
    }
    return get_cap(bfi, DRIVERKIT_ARGCN_SLOT_BAR0 + idx, cap);
}
