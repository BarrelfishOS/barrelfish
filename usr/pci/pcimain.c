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
#include <barrelfish/nameservice_client.h>
#include <stdio.h>
#include <stdlib.h>
#include <mm/mm.h>
#include <if/monitor_blocking_rpcclient_defs.h>

#include <skb/skb.h>

#include "pci.h"
#include "pci_acpi.h"

#include "pci_debug.h"

/**
 * Number of slots in the cspace allocator.
 * Keep it as a power of two and not smaller than DEFAULT_CNODE_SLOTS.
 */
#define PCI_CNODE_SLOTS 2048

uintptr_t my_apic_id;

// cnoderef for the phyaddrcn
static struct cnoderef cnode_phyaddr = {
    .address = CPTR_PHYADDRCN_BASE,
    .address_bits = DEFAULT_CNODE_BITS,
    .size_bits = PAGE_CNODE_BITS,
    .guard_size = 0,
};

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

    // XXX: The code below is confused about gen/l/paddrs.
    // Caps should be managed in genpaddr, while the bus mgmt must be in lpaddr.
    struct capref mem_cap = {
        .cnode = cnode_phyaddr,
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
	} else {
	    skb_add_fact("memory_region(%" PRIuGENPADDR ",%u,%zu,%u,%tu).",
                    mrp->mr_base,
                    mrp->mr_bits,
                    ((size_t)1) << mrp->mr_bits,
                    mrp->mr_type,
                    mrp->mrmod_data);
	}
        if (mrp->mr_type == RegionType_PlatformData
            || mrp->mr_type == RegionType_PhyAddr) {
            PCI_DEBUG("Region %d: 0x%08lx - 0x%08lx %s\n",
		      i, mrp->mr_base,
		      mrp->mr_base + (((size_t)1)<<mrp->mr_bits),
		      mrp->mr_type == RegionType_PlatformData
		      ? "platform data" : "physical address range");
            err = mm_add(&pci_mm_physaddr, mem_cap,
                         mrp->mr_bits, mrp->mr_base);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "adding region %d FAILED\n", i);
            }
            mem_cap.slot++;
        }
    }

    return SYS_ERR_OK;
}


struct capref biosmem;

int main(int argc, char *argv[])
{
    errval_t err;

    bool do_video_init = false;
    bool got_apic_id = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "video_init") == 0) {
            do_video_init = true;
        }

        if(sscanf(argv[i], "apicid=%" PRIuPTR, &my_apic_id) == 1) {
            got_apic_id = true;
        }
    }

    if(got_apic_id == false) {
        fprintf(stderr, "Usage: %s APIC_ID\n", argv[0]);
        fprintf(stderr, "Wrong monitor version?\n");
        return EXIT_FAILURE;
    }

    //connect to the SKB
    PCI_DEBUG("pci: connecting to the SKB...\n");
    skb_client_connect();
    PCI_DEBUG("pci: connected.\n");

#if 0
    skb_execute("append([1,2,3],[6,7,8],L),write(output,L).");
    PCI_DEBUG("\npci: data sent\n");

    while (skb_read_error_code() == SKB_PROCESSING) messages_wait_and_handle_next();
    PCI_DEBUG("\nSKB returned: %s\n", skb_get_output());
    PCI_DEBUG("\nSKB error returned: %s\n", skb_get_error_output());
#endif
    skb_execute("[pci_queries].");

    int error_code = skb_read_error_code();
    if (error_code != 0) {
        printf("\npcimain.c: Could not load pci_queries.pl.\n"
               "SKB returned: %s\nSKB error: %s\n",
                skb_get_output(), skb_get_error_output());
    }


    skb_add_fact("mem_region_type(%d,ram).", RegionType_Empty);
    skb_add_fact("mem_region_type(%d,roottask).", RegionType_RootTask);
    skb_add_fact("mem_region_type(%d,phyaddr).", RegionType_PhyAddr);
    skb_add_fact("mem_region_type(%d,multiboot_module).", RegionType_Module);
    skb_add_fact("mem_region_type(%d,platform_data).", RegionType_PlatformData);

    skb_add_fact("mem_region_type(%d,apic).", RegionType_LocalAPIC);
    skb_add_fact("mem_region_type(%d,ioapic).", RegionType_IOAPIC);

    err = init_allocators();
    assert(err_is_ok(err));

    // Get a copy of the VBE BIOS before ACPI touches it
    {
        struct capref bioscap, biosframe;

        err = mm_alloc_range(&pci_mm_physaddr, BIOS_BITS, 0,
                           1UL << BIOS_BITS, &bioscap, NULL);
        assert(err_is_ok(err));

        err = devframe_type(&biosframe, bioscap, BIOS_BITS);
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
/*         err = mm_free(&AcpiGbl_PlatMemMgr, 0, BIOS_BITS); */
/*         assert(err_is_ok(err)); */
    }

    int r = init_acpi();
    assert(r == 0);

    buttons_init();

    pci_init_datastructures();
    pci_init();

    if (do_video_init) {
        video_init();
    }

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
/*
    printf("get listing.\n");
    skb_execute("listing.");
    while (skb_read_error_code() == SKB_PROCESSING) messages_wait_and_handle_next();
    printf("\nSKB returned: \n%s\n", skb_get_output());
*/
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
