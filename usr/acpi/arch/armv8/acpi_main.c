/**
 * \file
 * \brief PCI
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <stdlib.h>
#include <mm/mm.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#include <octopus/octopus.h>
#include <skb/skb.h>

#include "acpi_debug.h"
#include "acpi_shared.h"

uintptr_t my_hw_id;

// Memory allocator instance for physical address regions and platform memory
struct mm pci_mm_physaddr;

// BIOS Copy
struct capref physical_caps;
struct capref my_devframes_cnode;

// XXX should be from offests
#define PADDR_SPACE_SIZE_BITS   48

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
    static struct range_slot_allocator devframes_allocator;
    err = range_slot_alloc_init(&devframes_allocator, L2_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC_INIT);
    }

    /* This next parameter is important. It specifies the maximum
     * amount that a cap may be "chunked" (i.e. broken up) at each
     * level in the allocator. Setting it higher than 1 reduces the
     * memory overhead of keeping all the intermediate caps around,
     * but leads to problems if you chunk up a cap too small to be
     * able to allocate a large subregion. This caused problems
     * for me with a large framebuffer... -AB 20110810 */
    err = mm_init(&pci_mm_physaddr, ObjType_DevFrame, 0, PADDR_SPACE_SIZE_BITS,
                  1, slab_default_refill, slot_alloc_dynamic,
                  slot_refill_dynamic, &devframes_allocator, false);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }

    // XXX: The code below is confused about gen/l/paddrs.
    // Caps should be managed in genpaddr, while the bus mgmt must be in lpaddr.

    // Here we get a cnode cap, so we need to put it somewhere in the root cnode
    // As we already have a reserved slot for a phyaddr caps cnode, we put it there

    errval_t error_code;
    struct capref requested_caps;
    err = cl->vtbl.get_phyaddr_cap(cl, &requested_caps, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));
    physical_caps = requested_caps;

    struct capref pacn = {
        .cnode = cnode_root,
        .slot = ROOTCN_SLOT_PACN
    };
    // Move phyaddr cap to ROOTCN_SLOT_PACN to conform to 2 level cspace
    err = cap_copy(pacn, requested_caps);
    assert(err_is_ok(err));

    // Build the capref for the first physical address capability
    struct capref phys_cap;
    phys_cap.cnode = build_cnoderef(pacn, CNODE_TYPE_OTHER);
    phys_cap.slot = 0;

    struct cnoderef devcnode;
    err = cnode_create_l2(&my_devframes_cnode, &devcnode);
    if (err_is_fail(err)) { USER_PANIC_ERR(err, "cnode create"); }
    struct capref devframe;
    devframe.cnode = devcnode;
    devframe.slot = 0;

    if (bootinfo->regions_length > L2_CNODE_SLOTS) {
        USER_PANIC("boot info has more regions (%d) than fit into L2 CNode (%d)",
                bootinfo->regions_length, L2_CNODE_SLOTS);
    }

    for (int i = 0; i < bootinfo->regions_length; i++) {
        struct mem_region *mrp = &bootinfo->regions[i];
        if (mrp->mr_type == RegionType_Module) {
            skb_add_fact("memory_region(16'%" PRIxGENPADDR ",%u,%zu,%u,%tu).",
                         mrp->mr_base, 0, mrp->mrmod_size, mrp->mr_type,
                         mrp->mrmod_data);
        }
        else {
            skb_add_fact("memory_region(16'%" PRIxGENPADDR ",%u,%zu,%u,%tu).",
                        mrp->mr_base, 0, mrp->mr_bytes, mrp->mr_type,
                        mrp->mrmod_data);
        }

        if (mrp->mr_type == RegionType_PhyAddr ||
            mrp->mr_type == RegionType_PlatformData) {
            ACPI_DEBUG("Region %d: %"PRIxGENPADDR" - %"PRIxGENPADDR" (%lu) bytes %s\n",
                       i, mrp->mr_base, mrp->mr_base + mrp->mr_bytes, mrp->mr_bytes,
                       mrp->mr_type == RegionType_PhyAddr ?
                       "physical address" : "platform data");

            err = cap_retype(devframe, phys_cap, 0, ObjType_DevFrame, mrp->mr_bytes, 1);
            if (err_is_ok(err)) {
                err = mm_add_multi(&pci_mm_physaddr, devframe, mrp->mr_bytes,
                        mrp->mr_base);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "adding region %d FAILED\n", i);
                }
            } else {
                if (err_no(err) == SYS_ERR_REVOKE_FIRST) {
                    printf("cannot retype region %d: need to revoke first; ignoring it\n", i);
                } else {
                  USER_PANIC_ERR(err, "error in retype\n");
                }
            }
            devframe.slot++;
            phys_cap.slot++;
        }

        if (mrp->mr_type == RegionType_ACPI_TABLE) {
            debug_printf("FOUND ACPI TABLE: %lx\n", mrp->mr_base);
            AcpiOsSetRootPointer(mrp->mr_base);
        }

    }

    return SYS_ERR_OK;
}

static errval_t setup_skb_info(void)
{
    skb_execute("[pci_queries].");
    errval_t err = skb_read_error_code();
    if (err_is_fail(err)) {
        ACPI_DEBUG("\npcimain.c: Could not load pci_queries.pl.\n"
               "SKB returned: %s\nSKB error: %s\n",
                skb_get_output(), skb_get_error_output());
        return err;
    }

    skb_add_fact("mem_region_type(%d,ram).", RegionType_Empty);
    skb_add_fact("mem_region_type(%d,roottask).", RegionType_RootTask);
    skb_add_fact("mem_region_type(%d,phyaddr).", RegionType_PhyAddr);
    skb_add_fact("mem_region_type(%d,multiboot_module).", RegionType_Module);
    skb_add_fact("mem_region_type(%d,platform_data).", RegionType_PlatformData);
    skb_add_fact("mem_region_type(%d,apic).", RegionType_LocalAPIC);
    skb_add_fact("mem_region_type(%d,ioapic).", RegionType_IOAPIC);

    return err;
}

int main(int argc, char *argv[])
{
    errval_t err;

    // Parse CMD Arguments
    bool got_apic_id = false;
    for (int i = 1; i < argc; i++) {
        if(sscanf(argv[i], "apicid=%" PRIuPTR, &my_hw_id) == 1) {
            got_apic_id = true;
        } else {
            debug_printf("unkown argument: '%s'\n", argv[i]);
        }
    }

    if(got_apic_id == false) {
        fprintf(stderr, "Usage: %s APIC_ID\n", argv[0]);
        fprintf(stderr, "Wrong monitor version?\n");
        return EXIT_FAILURE;
    }

    err = oct_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Initialize dist");
    }

    //connect to the SKB
    ACPI_DEBUG("acpi: connecting to the SKB...\n");
    err = skb_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Connecting to SKB failed.");
    }

    skb_execute("[pci_queries].");

    err = setup_skb_info();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Populating SKB failed.");
    }

    err = init_allocators();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Init memory allocator");
    }

    int r = init_acpi();
    assert(r == 0);

   start_service();

    messages_handler_loop();
}
