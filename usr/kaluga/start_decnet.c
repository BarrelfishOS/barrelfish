/**
 * \file
 * \brief Code responsible for starting devices discovered from decoding nets
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <skb/skb.h>

#include <if/octopus_defs.h>

#include <octopus/getset.h>
#include <octopus/trigger.h>

#include <maps/omap44xx_map.h>
#include <arch/arm/omap44xx/device_registers.h>

#include <bitmacros.h>

#include "kaluga.h"

#define DRIVER_CORE 0

static void start_driver_for_device(struct domain_instance *inst, char* device) {

    errval_t err = skb_execute_query("find_dn_driver('%s',R),write(R)", device);
    if (err == SKB_ERR_EXECUTION) {
        KALUGA_DEBUG("No driver found for device %s\n", device);
        return;
    } else if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "Getting driver info");
        return;
    }
    // Save driver info for later
    char *driver_info = strdup(skb_get_output());

    char *name = malloc(strlen(driver_info));
    int start_on_discovery = 0;
    err = skb_read_output_at(driver_info, "driver(%[^,],%d", name, &start_on_discovery);
    if (err_is_fail(err)) {
        USER_PANIC_SKB_ERR(err, "SKB parse error while getting driver info.");
    }
    else if (!start_on_discovery && inst == NULL) {
        KALUGA_DEBUG("Driver start deferred until other driver depends on it.\n");
        goto out;
    }
    KALUGA_DEBUG("Driver module name: %s\n", name);

    if (inst == NULL) {
        // XXX: for now we start all drivers on core 0
        inst = instantiate_driver_domain(DRIVER_CORE);
    }

    struct driver_instance *drv = ddomain_create_driver_instance(name, name);

    // Start driver dependencies
    struct list_parser_status status;
    skb_read_list_init_offset(&status, driver_info, 0);

    char *dep = malloc(strlen(driver_info));

    while(skb_read_list(&status, "drv_dep(%[^)]", dep)) {
        KALUGA_DEBUG("Driver %s dependency %s\n", name, dep);
        start_driver_for_device(inst, dep);
    }
    free(dep);

    // Give memory caps to driver by using decoding net
    err = skb_execute_query("dn_driver{module:\"%s\",device_regions:D},"
                                    "driver_register_regions(%d,D,R),write(R)", name, DRIVER_CORE);
    if (err_is_fail(err)) {
        USER_PANIC_SKB_ERR(err, "Could not get device regions");
    }
    KALUGA_DEBUG("Register region list %s\n", skb_get_output());

    lpaddr_t addr, size;

    skb_read_list_init(&status);
    while(skb_read_list(&status, "reg(%" SCNuLPADDR",%" SCNuLPADDR ")", &addr, &size)) {
        KALUGA_DEBUG("Get cap for registers: %"PRIxLPADDR", %"PRIxLPADDR"\n", addr, size);

        struct capref device_frame;
        err = get_device_cap(ROUND_DOWN(addr, BASE_PAGE_SIZE), ROUND_UP(size, BASE_PAGE_SIZE), &device_frame);
        assert(err_is_ok(err));

        KALUGA_DEBUG("get_device_cap worked\n");
        err = ddomain_driver_add_cap(drv, device_frame);
    }

    // Give interrupt source caps to driver by using decoding net
    err = skb_execute_query("dn_driver{module:\"%s\",device_interrupts:Ids},"
                                    "driver_interrupt_enum(Ids,I),write(I)", name);
    KALUGA_DEBUG("Interrupt enum list %s\n", skb_get_output());

    skb_read_list_init(&status);
    uint32_t base, limit;
    while(skb_read_list(&status, "int(%d,%d)", &base, &limit)) {
        KALUGA_DEBUG("Get interrupt cap for range: base=%"PRIu32", "
                "limit=%"PRIu32"\n", base, limit);
    }

    ddomain_instantiate_driver(inst, drv);

    KALUGA_DEBUG("Driver %s started\n", name);

    out:
    free(name);
    free(driver_info);
}

static void decnet_change_event(octopus_mode_t mode, const char* device_record,
                                void* st)
{
    KALUGA_DEBUG("Decoding net record: %s\n", device_record);

    if (mode & OCT_ON_SET) {
        char *device = NULL;

        errval_t err = oct_read(device_record, "_ { device: %s }", &device);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Got malformed device record?");
        }

        KALUGA_DEBUG("Found decoding net device %s\n", device);

        start_driver_for_device(NULL, device);
    }
}

errval_t watch_for_decnet_devices(void)
{
    static char* decnet_device  = "r'hw\\.dn\\.device\\.[0-9]+' { "
            " device: _ }";
    octopus_trigger_id_t tid;
    return oct_trigger_existing_and_watch(decnet_device, decnet_change_event, NULL, &tid);
}
