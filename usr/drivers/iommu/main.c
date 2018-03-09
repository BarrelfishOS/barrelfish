/**
 * \file
 * \brief IOMMU Driver
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
#include <barrelfish/nameservice_client.h>
#include <driverkit/driverkit.h>
#include <driverkit/iommu.h>
#include <skb/skb.h>

#include <numa.h>

#include "common.h"

#define SKB_SCHEMA_DMAR_DEVSC \
    "dmar_dev(%" SCNu32 ", %" SCNu8 ", %" SCNu8 ", "\
                "addr(%" SCNu16 ", %" SCNu8 ", %" SCNu8 ", %" SCNu8 "), "\
                "%" SCNu8  ")"



static errval_t parse_devices_scopes(void)
{
    errval_t err;

    debug_printf("Parsing device scrope\n");

    err = skb_execute_query("dmar_devscopes(L),length(L,Len),writeln(L)");
    assert(err_is_ok(err));

    char *skb_list_output = strdup(skb_get_output());

    struct list_parser_status status;
    skb_read_list_init_offset(&status, skb_list_output, 0);

    printf("==============\n%s\n================\n", skb_get_output());

    uint32_t unit_idx;
    uint8_t type, entrytype, enumid;
    uint16_t seg;
    uint8_t bus, dev, fun;
    while(skb_read_list(&status, SKB_SCHEMA_DMAR_DEVSC,
                        &unit_idx, &type, &entrytype, &seg, &bus,
                        &dev, &fun, &enumid)) {
        debug_printf("%u.%u.%u\n", bus, dev, fun);
        if (entrytype > 2) {
            debug_printf("not a PCI endpoint or bridge, continue\n");
            continue;
        }

        err = skb_execute_query("bridge(pcie,addr(%d,%d,%d),_,_,_,_,_,secondary(BUS)),"
                                        "write(secondary_bus(BUS)).", bus, dev, fun);
        uint32_t next_bus;
        if (err_is_ok(err)) {
            err = skb_read_output("secondary_bus(%d)", &next_bus);


            debug_printf("Bus %u -> %u\n", bus,next_bus);

            assert(err_is_ok(err));
        } else {
            next_bus = bus;
            debug_printf("Bus %u == %u\n", bus,next_bus);
        }

        debug_printf(SKB_SCHEMA_IOMMU_DEVICE "\n", HW_PCI_IOMMU_INTEL, unit_idx, type,
                     entrytype, seg, next_bus, dev, fun, enumid);

        err = skb_add_fact(SKB_SCHEMA_IOMMU_DEVICE, HW_PCI_IOMMU_INTEL, unit_idx, type,
                           entrytype, seg, next_bus, dev, fun, enumid);
        if (err_is_fail(err)) {
            continue;
        }
    }

    free(skb_list_output);

    return SYS_ERR_OK;
}




/**
 * Instantiate the driver domain.
 *
 * Connect to Kaluga and wait for eventual ddomain requests.
 */
int main(int argc, char** argv)
{
    errval_t err;


    /*
     size_t drivers = 0;
    struct bfdriver* cur = NULL;
    driverkit_list(&cur, &drivers);
    for (size_t i=0; i<drivers; i++) {
        printf("%s:%s:%d: Found device driver class = %s\n", __FILE__, __FUNCTION__, __LINE__, cur->name);
        cur += 1;
    }
    for (size_t i=0; i<argc; i++) {
        printf("%s:%s:%d: argv[i] = %s\n", __FILE__, __FUNCTION__, __LINE__, argv[i]);
    }*/

    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to connect to the SKB.");
    }

    err = numa_available();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initialize libnuma");
    }

    err = parse_devices_scopes();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initialize libnuma");
    }

    err = iommu_service_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to create the IOMMU service\n");
    }

    iref_t kaluga_iref = 0;
    err = nameservice_blocking_lookup("ddomain_controller", &kaluga_iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to connect to ddomain controller");
    }

    err = ddomain_communication_init(kaluga_iref, atoi(argv[2]));
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to initiate communication with Kaluga");
    }

    while(1) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch for messages_wait_and_handle_next hack");
        }
    }

    return 0;
}
