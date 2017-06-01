/**
 * \file timers.c
 * \brief Timer support for ARMv8
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <offsets.h>
#include <platform.h>
#include <serial.h>
#include <sysreg.h>
#include <arch/arm/gic.h>
#include <systime.h>
#include <timers.h>

cycles_t ticks_per_ms = 1;

/*
 * Timers
 */
void timers_init(int timeslice)
{
    printk(LOG_NOTE, "isr_el1=%p\n", sysreg_read_isr_el1());
    {
        armv8_generic_timer_kernel_ctrl_el1_t kctl;
        kctl = armv8_generic_timer_kernel_ctrl_el1_rd(NULL);

        /* don't trap access to CNTFRQ* and CNTFRQ* registers from EL0 to EL1 */
        kctl = armv8_generic_timer_kernel_ctrl_el1_EL0PCTEN_insert(kctl, 0x1);
        kctl = armv8_generic_timer_kernel_ctrl_el1_EL0VCTEN_insert(kctl, 0x1);

        /* trap access to CNTP_* and CNTV_* registers from EL0 to EL1 */
        kctl = armv8_generic_timer_kernel_ctrl_el1_EL0PTEN_insert(kctl, 0x0);
        kctl = armv8_generic_timer_kernel_ctrl_el1_EL0VTEN_insert(kctl, 0x0);

        armv8_generic_timer_kernel_ctrl_el1_wr(NULL, kctl);
    }

    /* enable the timer */
    armv8_generic_timer_ctrl_el0_IMASK_wrf(NULL, 0x0);
    armv8_generic_timer_ctrl_el0_ENABLE_wrf(NULL, 0x1);

    /* set the compare value */
    armv8_generic_timer_compare_val_el0_wr(NULL, 0xffffffffffffffff);


    /* systime_frequency is ticks per milisecond, while timer_get_frequency is in HZ */
    systime_frequency = timer_get_frequency() / 1000;

    /* The timeslice is in ms */
    kernel_timeslice = ns_to_systime(timeslice * 1000000);

    printf("System counter frequency is %uHz.\n", timer_get_frequency());
    printf("Timeslice interrupt every %u ticks (%dms).\n",
            kernel_timeslice, timeslice);

    // Wait for n time units, close to cycles
    armv8_generic_timer_timer_val_el0_wr(NULL, 100);

    while(timer_is_set())
        ;

    timer_reset(timeslice);

    uint32_t PMCR_EL0  = 0;

    PMCR_EL0 |= (1 << 0); /* All counters are enabled.*/
    PMCR_EL0 |= (1 << 1); /* reset all event counters */
    PMCR_EL0 |= (1 << 2); /* reset all clock counters */
    PMCR_EL0 |= (0 << 3); /* set counter to tick every clock cycle (1=ever 64th) */
    PMCR_EL0 |= (1 << 4); /* enable event support */
    PMCR_EL0 |= (0 << 5); /* don't disable cycle counte r*/

    PMCR_EL0 |= (6 << 11); /* Six counters */

    __asm volatile("msr PMCR_EL0, %[PMCR_EL0]" : : [PMCR_EL0] "r" (PMCR_EL0));


    uint32_t PMUSERENR_EL0  = 0;

    PMUSERENR_EL0 |= (1 << 0);  /* don't trap access to PM registers to EL 1 */
    PMUSERENR_EL0 |= (1 << 1);  /* don't trap software increment wrap to EL 1 */
    PMUSERENR_EL0 |= (1 << 2);  /* don't trap cycle counter to EL 1 */
    PMUSERENR_EL0 |= (1 << 3);  /* don't trap event counter read to EL 1*/

    __asm volatile("msr PMUSERENR_EL0, %[PMUSERENR_EL0]" : : [PMUSERENR_EL0] "r" (PMUSERENR_EL0));
}

/**
 *
 * @param ms
 */
void timer_reset(uint64_t ms)
{
    armv8_generic_timer_timer_val_el0_wr(NULL, ms * systime_frequency);
}


systime_t systime_now(void)
{
    return timer_get_timestamp();
}
