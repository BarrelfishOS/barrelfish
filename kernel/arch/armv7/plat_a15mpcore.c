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
#include <maps/a15mpcore_map.h>
#include <assert.h>
#include <cp15.h>
#include <gic.h>
#include <kernel.h>
#include <init.h>
#include <paging_kernel_arch.h>
#include <platform.h>
#include <systime.h>

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
 
/// For now, use secure timer
#define DEFAULT_TIMER_IRQ 29

extern uint32_t timerirq;
extern uint32_t cntfrq;

void
timers_init(int timeslice) {
    /* If there was a cntfrq parameter passed, then overwrite the current
     * CNTFRQ register.  We need to do this if there was no bootloader to set
     * it for us, as on the FVP simulators. */
    if(cntfrq != 0) a15_gt_set_cntfrq(cntfrq);

    systime_frequency = a15_gt_frequency();

    /* The timeslice is in ms, so divide by 1000. */
    kernel_timeslice = ns_to_systime(timeslice * 1000000);

    MSG("System counter frequency is %uHz.\n", systime_frequency);
    MSG("Timeslice interrupt every %u ticks (%dms).\n",
            kernel_timeslice, timeslice);

    a15_gt_init();

    if(timerirq == 0) timerirq= DEFAULT_TIMER_IRQ;
    MSG("Timer interrupt is %u\n", timerirq);

    /* Enable the interrupt. */
    gic_enable_interrupt(timerirq, 0, 0, 0, 0);

    /* Set the first timeout. */
    systime_set_timeout(systime_now() + kernel_timeslice);

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

bool timer_interrupt(uint32_t irq)
{
    if (irq == timerirq) {
        gic_ack_irq(irq);
        a15_gt_mask_interrupt();

        /* Reset the timeout. */
        uint64_t now = systime_now();
        systime_set_timeout(now + kernel_timeslice);
        return 1;
    }

    return 0;
}

systime_t systime_now(void)
{
    return a15_gt_counter();
}

void systime_set_timeout(systime_t timeout)
{
    a15_gt_set_comparator(timeout);
}
