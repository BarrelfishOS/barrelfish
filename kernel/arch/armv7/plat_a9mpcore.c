/**
 * \file
 * \brief Platform code for the Cortex-A9 MPCore.
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <maps/a9mpcore_map.h>
#include <a9_scu.h>
#include <a9_gt.h>
#include <gic.h>
#include <dev/cortex_a9_pit_dev.h>
#include <assert.h>
#include <cp15.h>
#include <kernel.h>
#include <init.h>
#include <paging_kernel_arch.h>
#include <arch/arm/platform.h>
#include <systime.h>
#include <arch/armv7/irq.h>

#define MSG(format, ...) \
    printk( LOG_NOTE, "CortexA9 platform: "format, ## __VA_ARGS__ )

/* These are called from the A9/A15 common GIC (interrupt controller) code. */

lpaddr_t platform_gic_cpu_interface_base = A9MPCORE_GIC_CPU_OFFSET;
lpaddr_t platform_gic_distributor_base = A9MPCORE_GIC_DIST_OFFSET;

/* These are for A9-specific devices, so are only used here. */

static lpaddr_t
platform_get_scu_address(void) {
    assert(paging_mmu_enabled());
    return platform_get_private_region() + A9MPCORE_SCU_OFFSET;
}

static lpaddr_t
platform_get_gt_address(void) {
    assert(paging_mmu_enabled());
    return platform_get_private_region() + A9MPCORE_TIMER_GBL_OFFSET;
}

static lpaddr_t
platform_get_lt_address(void) {
    assert(paging_mmu_enabled());
    return platform_get_private_region() + A9MPCORE_TIMER_LCL_OFFSET;
}

/* On the A9, we need to initialise the snoop control unit. */
void
platform_revision_init(void) {
    assert(paging_mmu_enabled());
    a9_scu_init(platform_get_scu_address());
    platform_gic_cpu_interface_base += platform_get_private_region();
    platform_gic_distributor_base += platform_get_private_region();
    if(platform_get_core_count() > 1) a9_scu_enable();
}

/*
 * Return the core count from the interrupt controller
 */
size_t
platform_get_core_count(void) {
    return gic_cpu_count();
}

static cortex_a9_pit_t tsc;

/* See TRM 4.3 */
#define GLOBAL_TIMER_IRQ 27

void platform_timer_init(int timeslice)
{
    errval_t err;
    /* Time slice counter: use the Cortex-A9 Local Timer
       (see Cortex-A9 MPCore TRM 4.1). */
    lvaddr_t lcl_base =
        paging_map_device(platform_get_lt_address(), A9MPCORE_TIMER_LCL_SIZE);
    cortex_a9_pit_initialize(&tsc, (mackerel_addr_t)lcl_base);

    /* Global timer: use the Cortex-A9 Global Timer
       (see Cortex-A9 MPCore TRM 4.3). */
    a9_gt_init(platform_get_gt_address());
    err = platform_enable_interrupt(GLOBAL_TIMER_IRQ, 0, 0, 0);
    if(err_is_fail(err)){
        printk(LOG_ERR,
                "Failed to enable timer interrupt. Will continue without...");
    }
    /* Discover the clock rate. */
    a9_probe_tsc();
    assert(systime_frequency != 0);

    MSG("System counter frequency is %lluHz.\n", systime_frequency);
    /* Set kernel timeslice value, timeslice is in ms. */
    kernel_timeslice = ns_to_systime(timeslice * 1000000);
    MSG("Timeslice interrupt every %llu system ticks (%dms).\n", kernel_timeslice, timeslice);
    systime_set_timer(kernel_timeslice);
}

bool platform_is_timer_interrupt(uint32_t irq)
{
    if (irq == GLOBAL_TIMER_IRQ) {
        a9_gt_ack_irq();
        return 1;
    }

    return 0;
}

systime_t systime_now(void)
{
    return a9_gt_read();
}

void systime_set_timeout(systime_t absolute_timeout)
{
    a9_gt_set_comparator(absolute_timeout);
}

void systime_set_timer(systime_t relative_timeout)
{
    systime_set_timeout(systime_now() + relative_timeout);
}
