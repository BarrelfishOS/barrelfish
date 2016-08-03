/**
 * \file
 * \brief Platform code for ARMv7-A VersatileExpress EMM board
 */

/*
 * Copyright (c) 2009-2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <bitmacros.h>
#include <a9_gt.h>
#include <a9_scu.h>
#include <boot_protocol.h>
#include <global.h>
#include <init.h>
#include <paging_kernel_arch.h>
#include <platform.h>
#include <serial.h>
#include <startup_arch.h>
#include <pl011.h>
#include <assert.h>
#include <errors/errno.h>
#include <a9mpcore_map.h>
#include <vexpress_map.h>
#include <dev/cortex_a9_pit_dev.h>
#include <gic.h>

/********************************************************************************
 *
 * Implementation of serial.h
 *
 *******************************************************************************/

errval_t serial_init(unsigned port, bool initialize_hw)
{
    lvaddr_t base = paging_map_device(uart_base[port], uart_size[port]);
    pl011_init(port, base, initialize_hw);
    return SYS_ERR_OK;
};

/*
 * Print system identification.   MMU is NOT yet enabled.
 * TODO - Use Mackerel to print the identification from the system
 * configuration block.
 */
void platform_print_id(void)
{
    assert(!paging_mmu_enabled());
    
    uint32_t id=
        *((uint32_t *)(VEXPRESS_MAP_SYSREG + VEXPRESS_SYS_ID));
    uint32_t procid0=
        *((uint32_t *)(VEXPRESS_MAP_SYSREG + VEXPRESS_SYS_PROCID0));
    uint32_t procid1=
        *((uint32_t *)(VEXPRESS_MAP_SYSREG + VEXPRESS_SYS_PROCID1));

    printf("Device: This is a VersatileExpress EMM board. "
           "ID=%08x PROCID0=%08x PROCID1=%08x\n",
           id, procid0, procid1);
}

void platform_get_info(struct platform_info *pi)
{
    pi->arch     = PI_ARCH_ARMV7A;
    pi->platform = PI_PLATFORM_VEXPRESS;
    armv7_get_info(&pi->arch_info.armv7);
}

/*
 * \brief Boot an ARM APP core
 *
 * \param core_id   MPID of the core to try booting
 * \param entry     Entry address for new kernel in the destination
 *                  architecture's lvaddr_t
 *
 * \returns Zero on successful boot, non-zero (error code) on failure
 */

int
platform_boot_aps(coreid_t core_id, genvaddr_t gen_entry) {
    assert(paging_mmu_enabled());

    /* XXX - we're abusing the gen_entry pointer here.  Change the interface
     * to make this arch-specific. */
    lpaddr_t new_core_data_ptr= (lpaddr_t)gen_entry;

    /* This mailbox is in the boot driver's BSS. */
    struct armv7_boot_record *br=
        (struct armv7_boot_record *)core_data->target_bootrecs;

    /* Acquire the lock on the boot record. */
    spinlock_acquire(&br->lock);

    /* Pass the core data pointer. */
    br->core_data= new_core_data_ptr;

    /* Clear the completion notification. */
    br->done= 0;

    /* Flag which core should boot. */
    /* XXX - this will only work for single-cluster systems, whose MPID fits
     * entirely within the low 8 bits.  Make core IDs bigger! */
    br->target_mpid= core_id;

    /* The boot driver will read this value with its MMU and caches disabled,
     * so we need to make sure it's visible. */
    dmb(); isb();
    clean_invalidate_to_poc(&br->core_data);
    clean_invalidate_to_poc(&br->done);
    clean_invalidate_to_poc(&br->target_mpid);

    /* We need to ensure that the clean has finished before we wake them. */
    dmb(); isb();

    /* Wake all sleeping cores. */
    sev();

    /* The target core will let us know that it's exited the boot driver by
     * setting done to one *with its MMU, and hence coherency, enabled*. */
    while(!br->done) wfe();

    /* Release the lock on the boot record. */
    spinlock_release(&br->lock);

    return 0;
}

void
platform_notify_bsp(void) {
    assert(paging_mmu_enabled());

    panic("Unimplemented.\n");
    /* XXX - implement me. */
}

uint32_t tsc_hz = 0;

void
a9_probe_tsc(void) {
    /* A15+ don't require probing, and the A9 FVP doesn't support it.  This
     * probe is only called on A9 platforms. */
    if(periphclk == 0) panic("No periphclk argument supplied.");
    tsc_hz= periphclk;
}
