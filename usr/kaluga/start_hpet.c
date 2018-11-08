/**
 * \file
 * \brief Code responsible for starting hpet
 */

/*
 * Copyright (c) 2018 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#define KALUGA_DEBUG_ON 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <skb/skb.h>
#include <if/octopus_defs.h>
#include <octopus/trigger.h>
#include <acpi_client/acpi_client.h>
#include <hw_records.h>
#include <if/acpi_defs.h>

#include "kaluga.h"

#define DRIVER_CORE 0
#define HPET_INT_CAP 1 // should change it  to refer to the one in hpet.h

errval_t start_hpet_driver(coreid_t where, struct module_info *driver,
                           char *record ) {
    errval_t err;
    static struct domain_instance *inst = NULL;
    struct driver_instance *drv = NULL;
    int64_t address = 0, uid = 0;

    KALUGA_DEBUG("start_hpet_driver: record=%s\n", record);
    err = oct_read(record, "_ { " HW_HPET_RECORD_FIELDS_READ " }", 
                   &address, &uid);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_read");
        goto out;
    }

    // Fail early
    assert(address != 0);

    errval_t msgerr;

    struct capref arg_caps;
    struct cnoderef argnode_ref;
    err = cnode_create_l2(&arg_caps, &argnode_ref);
    struct capref devcap = {
        .cnode = argnode_ref,
        .slot = 0
    };
    
    struct capref devcap_tmp;
    err = slot_alloc(&devcap_tmp);
    assert(err_is_ok(err));

    // store mem caps
    err = connect_to_acpi();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "connect_to_acpi");
        goto out;
    }
    struct acpi_binding *acpi = get_acpi_binding();
    err = acpi->rpc_tx_vtbl.mm_alloc_range_proxy(acpi, BASE_PAGE_BITS, address,
                                                 address + BASE_PAGE_SIZE,
                                                 &devcap_tmp, &msgerr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "mm_alloc_range_proxy\n");
        goto out;
    }
    if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "mm_alloc_range_proxy msgerr\n");
        err = msgerr;
        goto out;
    }
    KALUGA_DEBUG("start_hpet_driver: got mem cap for hpet \n");

    err = cap_copy(devcap, devcap_tmp);
    assert(err_is_ok(err));

    drv = ddomain_create_driver_instance("hpet_module", "key");
    if (drv == NULL) {
        err = DRIVERKIT_ERR_DRIVER_INIT;
        goto out;
    }

    KALUGA_DEBUG("start_hpet_driver: created int cap for hpet \n");

    // add mem cap to driver
    drv->caps[0] = arg_caps;
    //err = ddomain_driver_add_cap(drv, devcap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "add_cap");
        goto out;
    }

    drv->args[0] = malloc(50);
    snprintf(drv->args[0], 50, "%ld", uid);

    KALUGA_DEBUG("start_hpet_driver: Instantiating driver\n");

    // create driver instance
    if (driver->driverinstance == NULL) {
        KALUGA_DEBUG("Driver instance not running, starting...\n");

        // create driver domain
        inst = instantiate_driver_domain(driver->binary, where);
        driver->driverinstance = inst;

        while (inst->b == NULL) {
            event_dispatch(get_default_waitset());
        }
    }

    ddomain_instantiate_driver(inst, drv);

out:
    free(drv->args[0]);
    drv->args[0] = NULL;
    return err;
}

/*
 * For a hpet comp, instantiate the correct irq controller in the skb and return 
 * the input singleton range.
 */
static errval_t hpet_comp_get_irq_index(const char *record, char *ctrl_label,
                                        uint64_t *irq_idx) {
    int64_t hpet_uid, index;
    errval_t err;
    err = oct_read(record, "_ { " HW_HPET_COMP_RECORD_FIELDS_READ " }", &hpet_uid,
                   &index);
    if (err_is_fail(err))
        return err;

    err = skb_execute_query("Uid=%"PRIi64",Index=%"PRIi64",add_hpet_comp_controller(Lbl, Uid, "
                            "Index),write('\n'),print_int_controller(Lbl)",
                            hpet_uid, index);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "");
        return err;
    }

    uint64_t end = 0;
    err = skb_read_output("%*[^\n]\n%64[^,],%*[^,],%" SCNu64 ",%" SCNu64,
                          ctrl_label, irq_idx, &end);

    if (err_is_fail(err))
        return err;

    // we expect a range of only one element
    assert(*irq_idx == end);

    return SYS_ERR_OK;
}


static errval_t hpet_comp_store_irq_info(const char *device_record,
                               struct driver_instance *drv) {
    errval_t err;
    struct capref *all_irq_cap = get_irq_cap();
    struct capref intcap;

    err = slot_alloc(&intcap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not retype int_src cap");
        return err;
    }
    uint64_t irq_idx;
    drv->args[0] = malloc(128);
    assert(drv->args[0]);
    err = hpet_comp_get_irq_index(device_record, drv->args[0], &irq_idx);
    if (err_is_fail(err)) {
        return err;
    }
    struct capref arg_caps;
    struct cnoderef argnode_ref;
    err = cnode_create_l2(&arg_caps, &argnode_ref);
    struct capref irq_cap = {
        .cnode = argnode_ref,
        .slot = 0
    };
    err = cap_retype(irq_cap, *all_irq_cap, irq_idx, ObjType_IRQSrc, irq_idx, 1);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not retype int_src cap");
        return err;
    }

    return ddomain_driver_add_cap(drv, arg_caps);
}

static errval_t hpet_comp_store_index_arg(const char *record,
                               struct driver_instance *drv) {
    int64_t hpet_uid, index;
    errval_t err;
    err = oct_read(record, "_ { " HW_HPET_COMP_RECORD_FIELDS_READ " }", &hpet_uid,
                   &index);
    if (err_is_fail(err))
        return err;

    drv->args[1] = malloc(128);
    assert(drv->args[1] != NULL);
    snprintf(drv->args[1], 128, "%"PRIi64, index);
    return err;
}

static errval_t start_hpet_comp_driver(const char *device_record){
    errval_t err;

    struct module_info *mi = find_module("hpet");
    assert(mi != NULL);
    assert(mi->driverinstance != NULL);

    struct driver_instance *drv = NULL;
    drv = ddomain_create_driver_instance("hpet_comp_module", "key");

    err = hpet_comp_store_irq_info(device_record, drv); 
    if(err_is_fail(err)) return err;

    err = hpet_comp_store_index_arg(device_record, drv); 
    if(err_is_fail(err)) return err;

    KALUGA_DEBUG("start_hpet_comp_driver: Instantiating driver \n");
    ddomain_instantiate_driver(mi->driverinstance, drv);

    return SYS_ERR_OK;
}

static void hpet_comp_change_event(oct_mode_t mode, const char *device_record, void *st) {
    if ((mode & OCT_ON_SET) > 0) {
        KALUGA_DEBUG("HPET_comp change event: start\n");
        errval_t err = start_hpet_comp_driver(device_record);
        if(err_is_fail(err)){
            DEBUG_ERR(err,"");
        }
    }
}

void hpet_change_event(oct_mode_t mode, const char *device_record, void *st) {
    if ((mode & OCT_ON_SET) > 0) {
        KALUGA_DEBUG("HPET change event: start \n");
        errval_t err;

        struct module_info *mi = find_module("hpet");
        if (mi == NULL) {
            printf("HPET driver not found or not declared as auto.");
            return;
        }

        // Todo : change 0 is for core_0
        err = start_hpet_driver(0, mi, (CONST_CAST)device_record);

        switch (err_no(err)) {
        case SYS_ERR_OK:
            KALUGA_DEBUG("Spawned HPET driver: %s\n", mi->binary);
            break;

        case KALUGA_ERR_DRIVER_NOT_AUTO:
            KALUGA_DEBUG("%s not declared as auto, ignore.\n", mi->binary);
            break;

        default:
            DEBUG_ERR(err, "Unhandled error while starting hpet\n");
            break;
        }
    }
}

/*
 * Kaluga is notified (from acpi) of a new hpet. Kaluga obtains the page for reading
 * the base registers of hpet starts hpet domain with hpet_module.
 *
 * The hpet_module will read the number of timers (we call this comparators)
 * and insert for each comparator an skb hpet_comp and and octopus hpet_comp entry.
 *
 * Kaluga will react on this event, instantiate the hpet_comp interrupt controller driver.
 * and start the hpet_comp_module with: the input interrupt cap and the label of
 * the interrupt controller.
 */

static void irq_ready_event(oct_mode_t mode, const char *device_record,
                            void *st) {
    KALUGA_DEBUG("irq_ready_event: watching for HPET now\n");

    errval_t err;
    const char *hpet_device = HW_HPET_RECORD_REGEX;
    err = oct_trigger_existing_and_watch(hpet_device, hpet_change_event, NULL,
                                         NULL);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_trigger... hpet_device");
    }
}

errval_t watch_for_hpet(void) {
    // We only start watching for HPETs once we get the base_irq_controller
    // ready. Only then it's safe to call the add_hpet_controller method.
    
    errval_t err;
    const char *irq_ready = "base_irq_controller_ready {}";
    err = oct_trigger_existing_and_watch(irq_ready, irq_ready_event, NULL, NULL);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_trigger... base_irq_controller_ready");
    }

    err = oct_trigger_existing_and_watch(HW_HPET_COMP_RECORD_REGEX,
            hpet_comp_change_event, NULL, NULL);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_trigger... base_irq_controller_ready");
    }
    return err;
}
