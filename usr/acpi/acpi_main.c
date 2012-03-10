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

#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <stdlib.h>
#include <mm/mm.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#include <dist2/dist2.h>
#include <skb/skb.h>

#include "acpi_debug.h"
#include "acpi_shared.h"

/**
 * Number of slots in the cspace allocator.
 * Keep it as a power of two and not smaller than DEFAULT_CNODE_SLOTS.
 */
#define PCI_CNODE_SLOTS 2048

uintptr_t my_apic_id;

// XXX: this enum defines region types that must not overlap
// with the KPI-defined enum region_type.
enum user_region_type {
    RegionType_LocalAPIC = RegionType_Max,  ///< local APIC start address
    RegionType_IOAPIC                       ///< I/O APIC start address
};

// Memory allocator instance for physical address regions and platform memory
struct mm pci_mm_physaddr;

// BIOS Copy
struct capref biosmem;

static errval_t copy_bios_mem(void) {
    errval_t err = SYS_ERR_OK;

    // Get a copy of the VBE BIOS before ACPI touches it
    struct capref bioscap, biosframe;

    err = mm_alloc_range(&pci_mm_physaddr, BIOS_BITS, 0,
                       1UL << BIOS_BITS, &bioscap, NULL);
    assert(err_is_ok(err));

    err = devframe_type(&biosframe, bioscap, BIOS_BITS);
    //DEBUG_ERR(err, "devframe type\n");
    assert(err_is_ok(err));

    void *origbios;
    err = vspace_map_one_frame(&origbios, 1 << BIOS_BITS, biosframe,
                             NULL, NULL);
    assert(err_is_ok(err));

    err = frame_alloc(&biosmem, 1 << BIOS_BITS, NULL);
    assert(err_is_ok(err));

    void *newbios;
    err = vspace_map_one_frame(&newbios, 1 << BIOS_BITS, biosmem, NULL, NULL);
    assert(err_is_ok(err));

    memcpy(newbios, origbios, 1 << BIOS_BITS);

    // TODO: Unmap both vspace regions again

    err = cap_delete(biosframe);
    assert(err_is_ok(err));

    // TODO: Implement mm_free()

    return err;
}

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

    err = mm_init(&pci_mm_physaddr, ObjType_PhysAddr, 0, 48,
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

    // Build the capref for the first physical address capability
    struct capref phys_cap = {
        // XXX: not sure how to get size bit for this?
    	.cnode = build_cnoderef(requested_caps, 8),
    	.slot = 0,
    };

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

        if (mrp->mr_type == RegionType_PlatformData) {
            ACPI_DEBUG("Region %d: 0x%08lx - 0x%08lx platform data\n",
		      i, mrp->mr_base,
		      mrp->mr_base + (((size_t)1)<<mrp->mr_bits));
            err = mm_add(&pci_mm_physaddr, phys_cap,
                         mrp->mr_bits, mrp->mr_base);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "adding region %d FAILED\n", i);
            }
            phys_cap.slot++;
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

    bool do_video_init = false;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "video_init") == 0) {
            do_video_init = true;
        }
    }

    err = dist_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Initialize dist");
    }

    //connect to the SKB
    ACPI_DEBUG("acpi: connecting to the SKB...\n");
    skb_client_connect();
    skb_execute("[pci_queries].");


    err = setup_skb_info();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Populating SKB failed.");
    }

    err = init_allocators();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Init memory allocator");
    }

    err = copy_bios_mem();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Copy BIOS Memory");
    }

    int r = init_acpi();
    assert(r == 0);

    buttons_init();

    if (do_video_init) {
        video_init();
    }

    start_service();

    messages_handler_loop();
}
