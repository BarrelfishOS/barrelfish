/**
 * \file
 * \brief PCI
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
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

#include <if/monitor_blocking_rpcclient_defs.h>

#include <mm/mm.h>
#include <octopus/init.h>
#include <skb/skb.h>
#include <acpi_client/acpi_client.h>

#include "pci.h"
#include "ioapic_client.h"
#include "pci_debug.h"

/**
 * Number of slots in the cspace allocator.
 * Keep it as a power of two and not smaller than DEFAULT_CNODE_SLOTS.
 */
#define PCI_CNODE_SLOTS 2048

// Memory allocator instance for physical address regions and platform memory
struct mm pci_mm_physaddr;

static errval_t init_allocators(void)
{
    errval_t err, msgerr;

    struct monitor_blocking_rpc_client *cl = get_monitor_blocking_rpc_client();
    assert(cl != NULL);

    // Get the bootinfo and map it in.
    struct capref bootinfo_frame;
    size_t bootinfo_size;
    struct bootinfo *bootinfo;

    msgerr = cl->vtbl.get_bootinfo(cl, &err, &bootinfo_frame, &bootinfo_size);
    if (err_is_fail(msgerr) || err_is_fail(err)) {
        USER_PANIC_ERR(err_is_fail(msgerr) ? msgerr : err, "failed in get_bootinfo");
    }

    err = vspace_map_one_frame((void**)&bootinfo, bootinfo_size, bootinfo_frame,
                               NULL, NULL);
    assert(err_is_ok(err));

    /* Initialize the memory allocator to handle PhysAddr caps */
    static struct range_slot_allocator slot_allocator;
    err = range_slot_alloc_init(&slot_allocator, PCI_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC_INIT);
    }

    err = mm_init(&pci_mm_physaddr, ObjType_DevFrame, 0, 48,
                  /* This next parameter is important. It specifies the maximum
                   * amount that a cap may be "chunked" (i.e. broken up) at each
                   * level in the allocator. Setting it higher than 1 reduces the
                   * memory overhead of keeping all the intermediate caps around,
                   * but leads to problems if you chunk up a cap too small to be
                   * able to allocate a large subregion. This caused problems
                   * for me with a large framebuffer... -AB 20110810 */
                  1 /* was DEFAULT_CNODE_BITS */,
                  slab_default_refill, slot_alloc_dynamic, &slot_allocator, false);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }

    // Request I/O Cap
    struct capref requested_caps;
    errval_t error_code;
    err = cl->vtbl.get_io_cap(cl, &requested_caps, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));
    // Copy into correct slot
    struct capref caps_io = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_IO
    };
    err = cap_copy(caps_io, requested_caps);

    // XXX: The code below is confused about gen/l/paddrs.
    // Caps should be managed in genpaddr, while the bus mgmt must be in lpaddr.
    err = cl->vtbl.get_phyaddr_cap(cl, &requested_caps, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));
    //physical_caps = requested_caps;

    // Build the capref for the first physical address capability
    struct capref phys_cap;
    phys_cap.cnode = build_cnoderef(requested_caps, PAGE_CNODE_BITS);
    phys_cap.slot = 0;

    struct capref my_devframes_cnode;
    struct cnoderef devcnode;
    err = slot_alloc(&my_devframes_cnode);
    assert(err_is_ok(err));
    cslot_t slots;
    err = cnode_create(&my_devframes_cnode, &devcnode, 256, &slots);
    if (err_is_fail(err)) { USER_PANIC_ERR(err, "cnode create"); }
    struct capref devframe;
    devframe.cnode = devcnode;
    devframe.slot = 0;

    for (int i = 0; i < bootinfo->regions_length; i++) {
		struct mem_region *mrp = &bootinfo->regions[i];
		if (mrp->mr_type == RegionType_Module) {
			skb_add_fact("memory_region(%" PRIuGENPADDR ",%u,%zu,%u,%tu).",
						mrp->mr_base,
						0,
						mrp->mrmod_size,
						mrp->mr_type,
						mrp->mrmod_data);
		}
		else {
			skb_add_fact("memory_region(%" PRIuGENPADDR ",%u,%zu,%u,%tu).",
						mrp->mr_base,
						mrp->mr_bits,
						((size_t)1) << mrp->mr_bits,
						mrp->mr_type,
						mrp->mrmod_data);
		}

        if (mrp->mr_type == RegionType_PhyAddr ||
            mrp->mr_type == RegionType_PlatformData) {

            err = cap_retype(devframe, phys_cap, ObjType_DevFrame, mrp->mr_bits);
            DEBUG_ERR(err, "cap retype");
            assert(err_is_ok(err));

            err = mm_add(&pci_mm_physaddr, devframe,
                         mrp->mr_bits, mrp->mr_base);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "adding region %d FAILED\n", i);
            }

            phys_cap.slot++;
            devframe.slot++;
        }
    }
    //debug_my_cspace();

    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    debug_printf("pci staretd\n");
    errval_t err;

    err = oct_init();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "dist initialization failed.");
    }

    err = skb_client_connect();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Connecting to SKB failed.");
    }

    err = init_allocators();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Init memory allocator failed.");
    }

    err = connect_to_acpi();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "ACPI Connection failed.");
    }

    err = connect_to_ioapic();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "IOAPIC Connection failed.");
    }

    err = pcie_setup_confspace();
    if (err_is_fail(err)) {
        if (err_no(err) == ACPI_ERR_NO_MCFG_TABLE) {
            debug_printf("No PCIe found, continue.\n");
        }
        else {
            USER_PANIC_ERR(err, "Setup PCIe confspace failed.");
        }
    }

    err = pci_setup_root_complex();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Setup PCI root complex failed.");
    }

    // Start configuring PCI
    PCI_DEBUG("Programming PCI BARs and bridge windows\n");
    pci_program_bridges();
    PCI_DEBUG("PCI programming completed\n");
    pci_init_datastructures();
    pci_init();


#if 0 // defined(PCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
//output all the facts stored in the SKB to produce a sample data file to use
//for debugging on linux
    skb_execute("listing.");
    while (skb_read_error_code() == SKB_PROCESSING) messages_wait_and_handle_next();
    PCI_DEBUG("\nSKB returned: \n%s\n", skb_get_output());
    const char *errout = skb_get_error_output();
    if (errout != NULL && *errout != '\0') {
        PCI_DEBUG("\nSKB error returned: \n%s\n", errout);
    }
#endif

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
