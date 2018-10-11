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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>

#include <skb/skb.h>

#include <if/octopus_defs.h>

#include <octopus/getset.h>
#include <octopus/trigger.h>

#include <arch/arm/omap44xx/device_registers.h>
#include <maps/omap44xx_map.h>

#include "kaluga.h"
#include <acpi_client/acpi_client.h>
#include <barrelfish/nameservice_client.h>
#include <bitmacros.h>
#include <hw_records.h>
#include <if/acpi_defs.h>

#define DRIVER_CORE 0
#define HPET_INT_CAP 1 // should change it  to refer to the one in hpet.h

static errval_t init_int_args(uint64_t start_input_range,
                              uint64_t end_input_range,
                              struct driver_argument *arg,
                              struct capref *intcap) {
    errval_t err;
    arg->int_arg.int_range_start = start_input_range;
    arg->int_arg.int_range_end = end_input_range;

    // store int capability --> change this to make it modular
    arg->int_arg.model = INT_MODEL_MSI;

    struct capref *all_irq_cap = get_irq_cap();
    // code from store_int_cap to retype int cap
    assert(!cnoderef_is_null(arg->argnode_ref));
    assert(!capref_is_null(*all_irq_cap));

    intcap->cnode = arg->argnode_ref;
    intcap->slot = HPET_INT_CAP;

    err = cap_retype(*intcap, *all_irq_cap, start_input_range, ObjType_IRQSrc,
                     end_input_range, 1);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Could not retype int_src cap");
        return err;
    }
    return SYS_ERR_OK;
}

errval_t start_hpet_driver(coreid_t where, struct module_info *driver,
                           char *record, struct driver_argument *arg)
{
    errval_t err;
    KALUGA_DEBUG("start_hpet_driver: enter\n");
    static struct domain_instance *inst;
    struct driver_instance *drv;

    if (!is_auto_driver(driver)) {
        return KALUGA_ERR_DRIVER_NOT_AUTO;
    }

    char **names;
    size_t len = 0;
    char *key;
    uint64_t address;
    int size, nTimers, ierr;
    // retrieve HPET data from ACPI
    err = oct_get_names(&names, &len, HW_HPET_RECORD_REGEX);

    if (err_is_fail(err)) {
        if (err == OCT_ERR_NO_RECORD) {
            printf("######## Stop watching for HPET as since there are "
                         "no records available\n");
            return SYS_ERR_OK;
        }
        return err;
    }

    err = oct_get(&record, names[0]);
    if (err_is_fail(err)) {
        goto out;
    }

    err = oct_read(record, "%s { " HW_HPET_RECORD_FIELDS_READ " }", &key,
                   &address, &size, &nTimers);
    KALUGA_DEBUG("start_hpet_driver: nTimers = %d, size = %d, address = %lu "
                 ", key=%s \n", nTimers, size, address, key);

    // add HPET info in Kaluga
    char *q, *res;
    ierr = asprintf(
        &q, "add_hpet_controller(Lbl, %d),write('\n'),print_int_controller(Lbl).",
        nTimers);
    assert(ierr > 0);
    err = skb_evaluate(q, &res, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "add hpet controller");
        goto out;
    }

    uint64_t start_in=0, end_in=0, start_out=0, end_out=0;
    char ctrl_label[64];
    // Format is: ctrlinfo\nLbl,Class,InLo,InHi,OutLo,OutHi
    ierr = sscanf(res, "%*[^\n]\n%64[^,],%*[^,],%"SCNu64",%"SCNu64",%"SCNu64",%"SCNu64,
            ctrl_label, &start_in, &end_in, &start_out, &end_out);
    if(ierr != 5){
        printf("Can't parse skb result: %s\n", res);
        err = SKB_ERR_EXECUTION;
        goto out;
    }



    errval_t msgerr;
    struct capref devcap = NULL_CAP;
    err = slot_alloc(&devcap);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "slot_alloc");
        goto out;
    }

    // store mem caps
    struct acpi_binding *acpi;
    err = connect_to_acpi();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "connect_to_acpi");
        goto out;
    }
    acpi = get_acpi_binding();
    assert(err_is_ok(err));
    err = acpi->rpc_tx_vtbl.mm_alloc_range_proxy(acpi, BASE_PAGE_BITS, address,
                                                 address + BASE_PAGE_SIZE,
                                                 &devcap, &msgerr);
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

    // create int cap for diver
    struct capref *intcap = malloc(sizeof(struct capref));
    err = init_int_args(start_in, end_in, arg, intcap);
    drv = ddomain_create_driver_instance("hpet_module", "key");

    if (drv == NULL) {
        err = DRIVERKIT_ERR_DRIVER_INIT;
        goto out;
    }
    KALUGA_DEBUG("start_hpet_driver: created int cap for hpet \n");

    // add mem cap to driver
    err = ddomain_driver_add_cap(drv, devcap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "init_int_args");
        goto out;
    }

    // add int cap to driver
    err = ddomain_driver_add_cap(drv, *intcap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "init_int_args");
        goto out;
    }

    // add input int start range ,  input int end range , output int range to
    // drv args
    drv->args[0] = malloc(50);
    drv->args[1] = malloc(50);
    drv->args[2] = malloc(50);

    snprintf(drv->args[0], 50, "%lu", start_in);
    snprintf(drv->args[1], 50, "%lu", end_in);
    snprintf(drv->args[2], 50, "%lu", start_out);

    KALUGA_DEBUG("start_hpet_driver: Instantiating driver \n ");

    // create driver instance
    if (driver->driverinstance == NULL) {
        KALUGA_DEBUG("Driver instance not running, starting...\n");

        // create driver domain
        inst = instantiate_driver_domain(driver->binary, where);

        if (inst == NULL) {
            err = DRIVERKIT_ERR_DRIVER_INIT;
            KALUGA_DEBUG("Unable to instantiate the driver \n");
            goto out;
        }

        driver->driverinstance = inst;

        while (inst->b == NULL) {
            event_dispatch(get_default_waitset());
        }

    }

    ddomain_instantiate_driver(inst, drv);

out:
    free(key);
    return err;
}

void hpet_change_event(oct_mode_t mode, const char *device_record, void *st) {
    if ((mode & OCT_ON_SET) > 0) {
        KALUGA_DEBUG("Hpet change event: start \n ");
        errval_t err;

        struct module_info *mi = find_module("hpet");
        if (mi == NULL) {
            printf("hpet driver not found or not declared as auto.");
            return;
        }

        struct driver_argument *arg;
        arg = malloc(sizeof(struct driver_argument));
        err = cnode_create_l2(&arg->arg_caps, &arg->argnode_ref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Could not cnode_create_l2");
        }

        // Todo : change 0 is for core_0
        err = start_hpet_driver(0, mi, (CONST_CAST)device_record, arg);

        switch (err_no(err)) {
        case SYS_ERR_OK:
            KALUGA_DEBUG("\n Spawned hpet driver: %s\n", mi->binary);
            break;

        case KALUGA_ERR_DRIVER_NOT_AUTO:
            KALUGA_DEBUG("%s not declared as auto, ignore.\n", mi->binary);
            break;

        default:
            DEBUG_ERR(err, "Unhandled error while starting %s\n",
                      mi->binary);
            break;
        }
    }
}

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
    err =
        oct_trigger_existing_and_watch(irq_ready, irq_ready_event, NULL, NULL);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "oct_trigger... base_irq_controller_ready");
    }
    return err;
}
