/**
 * \file
 * \brief Local descriptor table (LDT) management
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/syscalls.h>
#include <barrelfish/ldt.h>
#include <arch/ldt.h>
#include <target/x86_64/barrelfish_kpi/cpu_target.h> // segment_descriptor
#include <stdio.h>

#define LDT_NENTRIES    512     ///< Number of entries in the statically-sized LDT

/// Local segment descriptor table. Shared by all dispatchers in an address space.
// (XXX: coherence assumption)
static union segment_descriptor ldt[LDT_NENTRIES];

/// Spinlock protecting LDT in spanned domains
// (XXX: coherence assumption)
static spinlock_t ldt_spinlock;

/** \brief Initialise private (per-dispatcher) LDT */
void ldt_init_disabled(dispatcher_handle_t handle)
{
    errval_t err;

    struct dispatcher_shared_x86_64 *disp =
        get_dispatcher_shared_x86_64(handle);
    struct dispatcher_x86_64 *disp_priv = get_dispatcher_x86_64(handle);

    /* setup private (static) LDT, and get kernel to load it */
    disp->ldt_base = (lvaddr_t) ldt;
    // XXX: size may not be multiple of page size, but does it really matter?
    disp->ldt_npages = DIVIDE_ROUND_UP(sizeof(ldt), BASE_PAGE_SIZE);
    sys_x86_reload_ldt();

    /* XXX: kludge to maintain backwards compatibility.
     * Setup a single segment descriptor that we can use to locate the
     * current dispatcher (i.e. curdispatcher() always works). This
     * will be replaced once we switch onto a thread with a real FS segment.
     */
    disp_priv->dummyseg[0] = 0;
    disp_priv->dummyseg[1] = handle;
    err = ldt_alloc_segment_disabled(handle, disp_priv->dummyseg,
                                     &disp_priv->disp_seg_selector);
    if (err_is_fail(err)) {
        // XXX: can't call usual debug/panic code, as curdispatcher() won't work
        char buf[128];
        snprintf(buf, sizeof(buf),
                 "%.*s.%u: fatal error in ldt_init_disabled(). Aborted.\n",
                 DISP_NAME_LEN, disp->d.name, disp_priv->generic.core_id);
        sys_print(buf, sizeof(buf));
        while (1) {disp_yield_disabled(handle);}
    }

    /* load this segment to FS */
    __asm volatile("mov %%ax, %%fs"
                   : /* No outputs */
                   : "a" (disp_priv->disp_seg_selector));
}

/**
 * \brief Allocate and fill a segment descriptor in the LDT
 *
 * \param handle Dispatcher handle
 * \param segbase Base of segment
 * \param ret_selector On success, used to return selector for new segment
 */
errval_t ldt_alloc_segment_disabled(dispatcher_handle_t handle, void *segbase,
                                    uint16_t *ret_selector)
{
    // segment descriptors are limited to a 32-bit base address
    if ((lvaddr_t)segbase >= (1ul << 32)) {
        return LIB_ERR_SEGBASE_OVER_4G_LIMIT;
    }

    // construct descriptor
    union segment_descriptor desc = {
        .d = {
            .lo_base = ((lvaddr_t) segbase) & 0xffffff,
            .hi_base = (((lvaddr_t) segbase) >> 24) & 0xff,
            .type = 3, /* read/write data, accessed */
            .system_desc = 1, /* data */
            .privilege_level = 3, /* user mode */
            .present = 1,
            .long_mode = 0,
            .operation_size = 1,
        }
    };

    // find free LDT entry
    acquire_spinlock(&ldt_spinlock);
    for (int i = 0; i < LDT_NENTRIES; i++) {
        if (!ldt[i].d.present) {
            ldt[i] = desc;
            release_spinlock(&ldt_spinlock);
            assert_disabled(ret_selector != NULL);
            *ret_selector = X86_64_LDT_SELECTOR(i);
            return SYS_ERR_OK;
        }
    }
    release_spinlock(&ldt_spinlock);

    return LIB_ERR_LDT_FULL;
}

/**
 * \brief enabled version of ldt_alloc_segment_disabled()
 * 
 * Exposed for calls by special-case software that needs to play with segments.
 */
errval_t ldt_alloc_segment(void *segbase, uint16_t *ret_selector)
{
    dispatcher_handle_t handle = disp_disable();
    errval_t ret = ldt_alloc_segment_disabled(handle, segbase, ret_selector);
    disp_enable(handle);
    return ret;
}

/**
 * \brief Free a previously-allocated segment on a specific dispatcher
 *
 * \param handle Dispatcher handle
 * \param selector Segment selector
 */
errval_t ldt_free_segment_ondisp(dispatcher_handle_t handle, uint16_t selector)
{
    if ((selector & 0x7) != 7) { // XXX: user-priv LDT selector
        return LIB_ERR_LDT_SELECTOR_INVALID;
    }

    int index = X86_64_SELECTOR_IDX(selector);

    // check that this entry is occupied
    if (index >= LDT_NENTRIES || !ldt[index].d.present) {
        return LIB_ERR_LDT_SELECTOR_INVALID;
    }

    // mark entry as free
    ldt[index].raw = 0;
    return SYS_ERR_OK;
}

/**
 * \brief Free a previously-allocated segment on the current dispatcher
 *
 * \param selector Segment selector
 */
errval_t ldt_free_segment(uint16_t selector)
{
    // strictly speaking, we probably don't need to disable here
    dispatcher_handle_t handle = disp_disable();
    errval_t ret = ldt_free_segment_ondisp(handle, selector);
    disp_enable(handle);
    return ret;
}

/**
 * \brief Update the base address of a previously-allocated segment
 *
 * \param selector Segment selector
 * \param segbase New base of segment
 */
errval_t ldt_update_segment(uint16_t selector, void *segbase)
{
    if ((selector & 0x7) != 7) { // XXX: user-priv LDT selector
        return LIB_ERR_LDT_SELECTOR_INVALID;
    }

    int index = X86_64_SELECTOR_IDX(selector);

    // check that this entry is occupied
    if (index >= LDT_NENTRIES || !ldt[index].d.present) {
        return LIB_ERR_LDT_SELECTOR_INVALID;
    }

    // segment descriptors are limited to a 32-bit base address
    if ((lvaddr_t)segbase >= (1ul << 32)) {
        return LIB_ERR_SEGBASE_OVER_4G_LIMIT;
    }

    // update base address
    ldt[index].d.lo_base = ((lvaddr_t) segbase) & 0xffffff;
    ldt[index].d.hi_base = (((lvaddr_t) segbase) >> 24) & 0xff;

    return SYS_ERR_OK;
}
