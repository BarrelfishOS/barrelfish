/**
 * \file acpi_generic.c
 * \brief
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

// Memory allocator instance for physical address regions and platform memory

#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>
#include <if/monitor_blocking_defs.h>

#include <mm/mm.h>

#include <skb/skb.h>

#include "acpi_debug.h"
#include "acpi_shared.h"
#include "acpi_allocators.h"


struct mm pci_mm_physaddr;

struct capref physical_caps;
struct capref my_devframes_cnode;

// XXX should be from offests
#define PADDR_SPACE_SIZE_BITS   48
#define MAXCHILDBITS    4               ///< Max branching of BTree nodes

errval_t acpi_allocators_init(void)
{
    errval_t err, msgerr;

    ACPI_DEBUG("acpi: initializing allocators\n");

    struct monitor_blocking_binding *cl = get_monitor_blocking_binding();
    assert(cl != NULL);

    ACPI_DEBUG("acpi: obtaining boot info...\n");

    // Get the bootinfo and map it in.
    struct capref bootinfo_frame;
    size_t bootinfo_size;
    struct bootinfo *bootinfo;

    err = slot_alloc(&bootinfo_frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "slot_alloc for monitor->get_bootinfo");
    }

    msgerr = cl->rpc_tx_vtbl.get_bootinfo(cl, &err, &bootinfo_frame, &bootinfo_size);
    if (err_is_fail(msgerr) || err_is_fail(err)) {
        USER_PANIC_ERR(err_is_fail(msgerr) ? msgerr : err, "failed in get_bootinfo");
    }

    err = vspace_map_one_frame((void**)&bootinfo, bootinfo_size, bootinfo_frame,
                               NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "mapping of bootinfo frame failed\n");
        return err;
    }

    ACPI_DEBUG("acpi: boot info mapped [%p..%p]\n", bootinfo,
                bootinfo + bootinfo_size);


    ACPI_DEBUG("acpi: setup slot allocator with %" PRIu64 " slots\n", L2_CNODE_SLOTS);

    /* Initialize the memory allocator to handle PhysAddr caps */
    static struct range_slot_allocator devframes_allocator;
    err = range_slot_alloc_init(&devframes_allocator, L2_CNODE_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC_INIT);
    }

    err = range_slot_alloc_refill(&devframes_allocator, L2_CNODE_SLOTS);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC_INIT);
    }
    ACPI_DEBUG("acpi: setup memory manager for physaddr region\n");

    err = mm_init(&pci_mm_physaddr, ObjType_DevFrame, 0, 48,
                  /* This next parameter is important. It specifies the maximum
                   * amount that a cap may be "chunked" (i.e. broken up) at each
                   * level in the allocator. Setting it higher than 1 reduces the
                   * memory overhead of keeping all the intermediate caps around,
                   * but leads to problems if you chunk up a cap too small to be
                   * able to allocate a large subregion. This caused problems
                   * for me with a large framebuffer... -AB 20110810 */
                  1, /*was DEFAULT_CNODE_BITS,*/
                  slab_default_refill, slot_alloc_dynamic,
                  slot_refill_dynamic, &devframes_allocator, false);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }


    ACPI_DEBUG("acpi: REFILL=%p %p\n", slot_refill_dynamic, pci_mm_physaddr.slot_refill);

    struct capref requested_caps;


    // XXX: The code below is confused about gen/l/paddrs.
    // Caps should be managed in genpaddr, while the bus mgmt must be in lpaddr.

    ACPI_DEBUG("acpi: obtaining CNODE containing physaddr caps\n");

    // Here we get a cnode cap, so we need to put it somewhere in the root cnode
    // As we already have a reserved slot for a phyaddr caps cnode, we put it there
    err = slot_alloc(&requested_caps);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "slot_alloc for monitor->get_phyaddr_cap");
    }
    err = cl->rpc_tx_vtbl.get_phyaddr_cap(cl, &requested_caps, &msgerr);
    assert(err_is_ok(err) && err_is_ok(msgerr));
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

    ACPI_DEBUG("acpi: creating L2 cnode for device caps\n");

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

    ACPI_DEBUG("acpi: walking boot info to obtain the device regions\n");

    for (int i = 0; i < bootinfo->regions_length; i++) {
        struct mem_region *mrp = &bootinfo->regions[i];

        if (mrp->mr_type == RegionType_Module) {
//            ACPI_DEBUG("acpi: region[%u] base=0x%" PRIxGENPADDR ", size=%zu, type=%u\n",
//                               i, mrp->mr_base, mrp->mrmod_size, mrp->mr_type);

            skb_add_fact("memory_region(16'%" PRIxGENPADDR ",%u,%zu,%u,%tu).",
                         mrp->mr_base, 0, mrp->mrmod_size, mrp->mr_type,
                         mrp->mrmod_data);
        }
        else {
        //    ACPI_DEBUG("acpi: region[%u] base=0x%" PRIxGENPADDR ", size=%zu, type=%u\n",
        //                       i, mrp->mr_base, mrp->mr_bytes, mrp->mr_type);

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
    }

    return acpi_allocators_init_arch(bootinfo);
}
