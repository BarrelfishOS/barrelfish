/**
 * \file
 * \brief Device manager for Barrelfish.
 *
 * Interacts with the SKB / PCI to start cores, drivers etc.
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

#include <dist2/dist2.h>
#include <barrelfish/nameservice_client.h>

#include <errors/errno.h>

int main(void)
{
    errval_t err = dist_init();
    assert(err_is_ok(err));

    iref_t iref;
    nameservice_blocking_lookup("pci_discovery_done", &iref);


    printf("Hello World this is device manager\n");
    char** names = NULL;
    size_t len = 0;
    err = dist_get_names(&names, &len, "r'device.*' { class: _ }");

    for (size_t i=0; i<len; i++) {
        //debug_printf("found device: %s\n", names[i]);
        char* device_record = NULL;
        err = dist_get(&device_record, names[i]);
        assert(err_is_ok(err));

        uint64_t device_id, vendor_id;
        //debug_printf("device record is: %s\n", device_record);
        err = dist_read(device_record, "_ { device_id: %d, vendor: %d }", &device_id, &vendor_id);
        assert(err_is_ok(err));
        debug_printf("device_record: %s\n", device_record);
        char* db_record = NULL;
        err = dist_get(&db_record, "pci.db.devices.%lu.%lu", vendor_id, device_id);
        if (err_is_ok(err)) {
            char* vendor_name = NULL;
            char* device_name = NULL;
            //debug_printf("found db_record: %s\n", db_record);

            err = dist_read(db_record, "_ { device_name: %s, vendor_name: %s }", &device_name, &vendor_name);
            assert(err_is_ok(err));

            printf("** New device found: %s, %s\n", vendor_name, device_name);
            free(vendor_name);
            free(device_name);
        }
        else {
            printf("** New device not recognized: vendor_id:%lu, device_id:%lu\n", vendor_id, device_id);
        }

        free(device_record);
    }

    dist_free_names(names, len);
    return EXIT_SUCCESS;
}
