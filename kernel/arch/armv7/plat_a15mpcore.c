/**
 * \file
 * \brief Platform code for the Cortex-A15 MPCore.
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <a15_gt.h>
#include <a15mpcore_map.h>
#include <assert.h>
#include <cp15.h>
#include <gic.h>
#include <kernel.h>
#include <init.h>
#include <paging_kernel_arch.h>
#include <platform.h>

#define MSG(format, ...) \
    printk( LOG_NOTE, "CortexA15 platform: "format, ## __VA_ARGS__ )

/* These are called from the A9/A15 common GIC (interrupt controller) code. */

lpaddr_t
platform_get_distributor_address(void) {
    assert(paging_mmu_enabled());
    return platform_get_private_region() + A15MPCORE_GICD_OFFSET;
}

lpaddr_t
platform_get_gic_cpu_address(void) {
    assert(paging_mmu_enabled());
    return platform_get_private_region() + A15MPCORE_GICC_OFFSET;
}

/* A15 platforms don't need anything special done. */
void
platform_revision_init(void) {
}

/*
 * Return the core count from the interrupt controller
 */
size_t
platform_get_core_count(void) {
    return gic_cpu_count();
}

/* Timeslice counter uses the Non-secure Physical Timer. */

/* See TRM 8.2.3 */
/* This *should* be IRQ 30, for the non-secure timer, but GEM5 only
 * provides the secure timer, even in NS mode.
 * The timerirq parameter allows this to be overridden. */
#define DEFAULT_TIMER_IRQ 30

extern uint32_t timerirq;
static uint32_t timeslice_ticks;

void
timers_init(int timeslice) {
    /* The timeslice is in ms, so divide by 1000. */
    timeslice_ticks= timeslice * a15_gt_frequency() / 1000;

    MSG("System counter frequency is %uHz.\n", a15_gt_frequency());
    MSG("Timeslice interrupt every %u ticks (%dms).\n",
            timeslice_ticks, timeslice);

    a15_gt_init();

    if(timerirq == 0) timerirq= DEFAULT_TIMER_IRQ;
    MSG("Timer interrupt is %u\n", timerirq);

    /* Enable the interrupt. */
    gic_enable_interrupt(timerirq, 0, 0, 0, 0);

    /* Set the first timeout. */
    a15_gt_timeout(timeslice_ticks);

    /* We use the system counter for timestamps, which doesn't need any
     * further initialisation. */
}

uint64_t
timestamp_read(void) {
    return a15_gt_counter();
}

uint32_t
timestamp_freq(void) {
    return a15_gt_frequency();
}

bool
timer_interrupt(uint32_t irq) {
    if(irq == timerirq) {
        gic_ack_irq(irq);

        /* Reset the timeout. */
        a15_gt_timeout(timeslice_ticks);
        return 1;
    }

    return 0;
}
