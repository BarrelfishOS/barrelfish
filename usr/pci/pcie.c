/**
 * \file
 * \brief PCIe Initialization
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <stdlib.h>
#include <mm/mm.h>
#include <pci/confspace/pci_confspace.h>
#include <acpi_client/acpi_client.h>
#include <skb/skb.h>

#include "pci.h"
#include "pci_debug.h"

errval_t pcie_setup_confspace(void) {

    errval_t err;
    uint64_t address;
    uint16_t segment;
    uint8_t sbus;
    uint8_t ebus;

    /*
    struct acpi_rpc_client* cl = get_acpi_rpc_client();
    cl->vtbl.get_pcie_confspace(cl, &err, &address, &segment,
            &sbus, &ebus);
            */

    err = skb_execute_query("pcie_confspace(Address,Segment,Start,End),"
                            "writeln([Address,Segment,Start,End]).");
    if (err_is_fail(err)) {
        return err;
    }
    PCI_DEBUG("pci confspace: %s\n", skb_get_output());
    err = skb_read_output("[%lu, %hu, %"PRIu8", %"PRIu8"]", &address, &segment, &sbus, &ebus);
    if (err_is_ok(err)) {
        PCI_DEBUG("calling confspace init with: %lu %d %d %d",
                address, segment, sbus, ebus);
        int r = pcie_confspace_init(NULL, address, segment, sbus, ebus);
        assert(r == 0);
    }

    return err;
}

