/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>

#include <mm/mm.h>
#include <xeon_phi/xeon_phi.h>

#include "xeon_phi_internal.h"
#include "sysmem_caps.h"

/// the number of slots to allocate for the allocator
#define NUM_SLOTS 2048

#define NUM_CHILDREN 2

/*
 * XXX: This manager relies on the 1:1 mapping of the system memory
 *      in the system memory page tables!
 */

/// the memory manager for the system memory
static struct mm sysmem_manager;

/// offset to the base address
static lpaddr_t base_offset = 0;

/// the slot allocator
static struct range_slot_allocator sysmem_allocator;

/**
 * \brief Initializes the capability manager of the system memory range
 *
 * \return SYS_ERR_OK on success,
 */
errval_t sysmem_cap_manager_init(struct capref sysmem_cap)
{
    errval_t err;

    // initialize the memory allcator
    XSYSMEM_DEBUG("Initializing slot allocator of %i slots\n", NUM_SLOTS);
    err = range_slot_alloc_init(&sysmem_allocator, NUM_SLOTS, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC_INIT);
    }

    struct frame_identity ret;
    err = invoke_frame_identify(sysmem_cap, &ret);
    if (err_is_fail(err)) {
        return err;
    }

    base_offset = ret.base;

    XSYSMEM_DEBUG("Initializing memory manager\n");

    /*
     * initialize the memory manager.
     *
     * Important: the type has to be DevFrame, we do not want to zero out the
     *            host memory!
     */
    err = mm_init(&sysmem_manager, ObjType_DevFrame, ret.base, ret.bits,
                  NUM_CHILDREN, slab_default_refill, slot_alloc_dynamic,
                  &sysmem_allocator, false);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }

    XSYSMEM_DEBUG("Adding cap: [0x%016lx, %i]\n", ret.base, ret.bits);
    err = mm_add(&sysmem_manager, sysmem_cap, ret.bits, ret.base);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Returns a previously requested system memory capability to the
 *        cap manager
 */
errval_t sysmem_cap_return(struct capref frame)
{
    errval_t err;
    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    return mm_free(&sysmem_manager, frame, id.base, id.bits);
}

/**
 * \brief Requests a certain system memory capability based on the base and
 *        length requirements
 *
 * \param base  the base address of the system memory (host address)
 * \param bits  the size of the requested capability in bits
 * \param frame capability representing the system memory frame
 *
 * \retval SYS_ERR_OK on success
 *
 * Note: the caller must check the size and base of the frame...
 */
errval_t sysmem_cap_request(lpaddr_t base,
                            uint8_t bits,
                            struct capref *frame)
{
    errval_t err;

    XSYSMEM_DEBUG("Requesting cap for [0x%016lx, %i]\n", base, bits);
    // the size and base must not exceed the maximum range (512G)
    assert(bits < 40);
    assert(!(base & (BASE_PAGE_SIZE-1)));

    // align the base to the next 4k boundary
    //size += (base & (BASE_PAGE_SIZE-1));
    // base -= (base & (BASE_PAGE_SIZE-1));

    // size = (size+BASE_PAGE_SIZE-1) & ~(BASE_PAGE_SIZE - 1);

    // transform the address into the host memory range
    base += base_offset;

    err = mm_alloc_range(&sysmem_manager, bits, base, base + (1UL << bits), frame,
                         NULL);

    if (err_is_fail(err)) {
        XSYSMEM_DEBUG("Try reallocation for  [0x%016lx, %i]\n", base, bits);
        err = mm_realloc_range(&sysmem_manager, bits, base, frame);
        if (err_is_fail(err)) {
            return err;
        }
    }
    return SYS_ERR_OK;
}
