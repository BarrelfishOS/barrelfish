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

#include <a9mpcore_map.h>
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

/* See TRM 4.2.3 */
#define LOCAL_TIMER_IRQ 29

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

    /* Discover the clock rate. */
    a9_probe_tsc();
    assert(tsc_hz != 0);

    /* Write counter reload value.  Divide by 1000, as timeslice is in ms. */
    uint32_t reload = (timeslice * tsc_hz) / 1000;
    MSG("System counter frequency is %uHz.\n", tsc_hz);
    MSG("Timeslice interrupt every %u ticks (%dms).\n", reload, timeslice);
    cortex_a9_pit_TimerLoad_wr(&tsc, reload);

    /* Prescaler value to 1 - run at PERIPHCLK to match the global timer. */
    cortex_a9_pit_TimerControl_prescale_wrf(&tsc, 0);
    /* Enable interrupt generation. */
    cortex_a9_pit_TimerControl_int_enable_wrf(&tsc, 1);
    /* Automatic reload. */
    cortex_a9_pit_TimerControl_auto_reload_wrf(&tsc, 1);
    /* Clear any pending event */
    cortex_a9_pit_TimerIntStat_event_flag_wrf(&tsc, 1);
    /* Enable the timeslice interrupt. */
    gic_enable_interrupt(LOCAL_TIMER_IRQ, 0, 0, 0, 0);
    /* Enable the timer. */
    cortex_a9_pit_TimerControl_timer_enable_wrf(&tsc, 1);
}

uint64_t
timestamp_read(void) {
    return a9_gt_read();
}

uint32_t
timestamp_freq(void) {
    return tsc_hz;
}

bool
timer_interrupt(uint32_t irq) {
    if(irq == LOCAL_TIMER_IRQ) {
        /* Clear the flag at the timer. */
        cortex_a9_pit_TimerIntStat_event_flag_wrf(&tsc, 1);

        /* Ack the interrupt at the controller. */
        gic_ack_irq(irq);
        return 1;
    }

    return 0;
}
