/**
 * \file
 * \brief PCI
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/monitor_blocking_defs.h>

#include <mm/mm.h>
#include <octopus/init.h>
#include <octopus/barrier.h>
#include <skb/skb.h>
#include <acpi_client/acpi_client.h>
#include <int_route/int_route_server.h>
#include <int_route/int_route_debug.h>

#include "pci.h"
#include "pci_debug.h"
#include "pci_int_ctrl.h"

#include <driverkit/hwmodel.h> // Just for debug print

#if !defined(__ARM_ARCH_8A__)
static errval_t init_io_ports(void)
{
    errval_t err;

    struct monitor_blocking_binding *cl = get_monitor_blocking_binding();
    assert(cl != NULL);

    // Request I/O Cap
    struct capref requested_caps;
    errval_t error_code;
    err = slot_alloc(&requested_caps);
    assert(err_is_ok(err));
    err = cl->rpc_tx_vtbl.get_io_cap(cl, &requested_caps, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));

    // Copy into correct slot
    struct capref caps_io = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_IO
    };
    err = cap_copy(caps_io, requested_caps);

    return SYS_ERR_OK;
}
#endif

static errval_t init_decoding_net(void)
{
    errval_t err;
    // load bride program, otherwise bar() is not defined 
    PCI_DEBUG("PCI: Loading bridge program %s\n", skb_bridge_program);
    err = skb_execute_query("[%s]", skb_bridge_program);
    if (err_is_fail(err)) {
        DEBUG_SKB_ERR(err, "Failed loading brige program \n");
        return err;
    }

    PCI_DEBUG("PCI: Loading decoding net \n");
    err = skb_execute("[decoding_net4_support].");

    if(err_is_fail(err)) {
        //DEBUG_SKB_ERR(err, "Failed loading decoding net module load");
        return err;
    }

    const char * decoding_net_file = "sockeyefacts/x86_iommu";
    HWMODEL_QUERY_DEBUG(
            "load_net(\"%s\"), init(S), state_set(S).",
            decoding_net_file);
    err = skb_execute_query(
            "load_net(\"%s\"), init(S), state_set(S).",
            decoding_net_file);
    if (err_is_fail(err)) {
        //DEBUG_SKB_ERR(err, "load x86_iommu decoding net");
        return err;
    }

    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    errval_t err;

    // Parse commandline arguments
    for(int i = 1; i < argc; i++) {
        if(!strncmp(argv[i], "skb_bridge_program=", strlen("skb_bridge_program="))) {
            skb_bridge_program = argv[i] + strlen("skb_bridge_program=");
        } else if(!strncmp(argv[i], "numvfs=", strlen("numvfs="))) {
            max_numvfs = atoi(argv[i] + strlen("numvfs="));
            enable_vfs = true;
        } else {
            printf("%s: Unknown commandline option \"%s\" -- skipping.\n", argv[0], argv[i]);
        }
    }

    PCI_DEBUG("oct_init.\n");
    err = oct_init();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "dist initialization failed.");
    }

    PCI_DEBUG("skb_client_connect.\n");
    err = skb_client_connect();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Connecting to SKB failed.");
    }

    PCI_DEBUG("int_route_service_init.\n");
    err = int_route_service_init();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "int_route_service_init failed");
        abort();
    }

    PCI_DEBUG("connect_to_acpi.\n");
    err = connect_to_acpi();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ACPI Connection failed.");
    }

#if !defined(__ARM_ARCH_8A__)
    err = init_io_ports();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Init memory allocator failed.");
    }
#endif
    // Load the decoding net (to add bars)
    err = init_decoding_net();
    if (err_is_fail(err)) {
        debug_printf("Failed loading decoding net, continue withouth \n");
        decoding_net = false;
    } else {
        debug_printf("Successfully loaded decoding net\n");
        decoding_net = true;
    }

    err = pcie_setup_confspace();
    if (err_is_fail(err)) {
        if (err_no(err) == ACPI_ERR_NO_MCFG_TABLE) {
            PCI_DEBUG("No PCIe found, continue.\n");
        }
        else {
            USER_PANIC_ERR(err, "Setup PCIe confspace failed.");
        }
    }

    err = pci_setup_root_complex();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Setup PCI root complex failed.");
    }

    err = pci_int_ctrl_init();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Initializing pci_int_ctrl failed");
    }

    // Start configuring PCI
    PCI_DEBUG("Programming PCI BARs and bridge windows\n");
    pci_program_bridges();

    char* record;
    debug_printf("barrier.pci.bridges");
    err = oct_barrier_enter("barrier.pci.bridges", &record, 3);
    assert(err_is_ok(err));
    free(record);

    PCI_DEBUG("PCI programming completed\n");
    pci_init_datastructures();
    pci_init();

    skb_add_fact("pci_discovery_done.");

    /* Using the name server as a lock server,
       register a service with it so that other domains can do a blocking wait
       on pci to finish enumeration */
    err = nameservice_register("pci_discovery_done", 0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }

    messages_handler_loop();
}
