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
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
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
#include <platform.h>
#include <systime.h>

#define MSG(format, ...) \
    printk( LOG_NOTE, "CortexA9 platform: "format, ## __VA_ARGS__ )

/* These are called from the A9/A15 common GIC (interrupt controller) code. */

lpaddr_t
platform_get_distributor_address(void) {
    assert(paging_mmu_enabled());
    return platform_get_private_region() + A9MPCORE_GIC_DIST_OFFSET;
}

lpaddr_t
platform_get_gic_cpu_address(void) {
    assert(paging_mmu_enabled());
    return platform_get_private_region() + A9MPCORE_GIC_CPU_OFFSET;
}

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

void
timers_init(int timeslice) {
    /* Time slice counter: use the Cortex-A9 Local Timer
       (see Cortex-A9 MPCore TRM 4.1). */
    lvaddr_t lcl_base =
        paging_map_device(platform_get_lt_address(), A9MPCORE_TIMER_LCL_SIZE);
    cortex_a9_pit_initialize(&tsc, (mackerel_addr_t)lcl_base);

    /* Global timer: use the Cortex-A9 Global Timer
       (see Cortex-A9 MPCore TRM 4.3). */
    a9_gt_init(platform_get_gt_address());
    gic_enable_interrupt(GLOBAL_TIMER_IRQ, 0, 0, 0, 0);
    /* Discover the clock rate. */
    a9_probe_tsc();
    assert(tsc_hz != 0);

    systime_frequency = tsc_hz;

    MSG("System counter frequency is %uHz.\n", tsc_hz);
    /* Set kernel timeslice value, timeslice is in ms. */
    kernel_timeslice = ns_to_systime(timeslice * 1000000);
    MSG("Timeslice interrupt every %llu system ticks (%dms).\n", kernel_timeslice, timeslice);
    systime_set_timeout(systime_now() + kernel_timeslice);
}

uint64_t
timestamp_read(void) {
    return a9_gt_read();
}

uint32_t
timestamp_freq(void) {
    return tsc_hz;
}

bool timer_interrupt(uint32_t irq) {
    if (irq == GLOBAL_TIMER_IRQ) {
#ifndef CONFIG_ONESHOT_TIMER
        // Set next trigger
        systime_set_timeout(systime_now() + kernel_timeslice);
#endif
        /* Ack the interrupt at the controller. */
        gic_ack_irq(irq);
        return 1;
    }

    return 0;
}

systime_t systime_now(void)
{
    return a9_gt_read();
}

void systime_set_timeout(systime_t timeout)
{
    a9_gt_set_comparator(timeout);
}
