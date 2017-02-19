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

/*
 * Timers
 */
void timers_init(int timeslice)
{
    printk(LOG_NOTE, "isr_el1=%p\n", sysreg_read_isr_el1());
    {
        uint32_t CNTKCTL_EL1 = sysreg_read_cntkctl_el1();
        CNTKCTL_EL1 |= (0 << 9); // Trap access to CNTP_* to EL1
        CNTKCTL_EL1 |= (0 << 8); // Dont trap access to CNTV_* to EL1
        CNTKCTL_EL1 |= (1 << 1); // Dont trap access to CNTFRQ* to EL1
        CNTKCTL_EL1 |= (1 << 0); // Dont trap access to CNTFRQ* to EL1
        sysreg_write_cntkctl_el1(CNTKCTL_EL1);
    }

    {
        uint32_t cntp_ctl_el0 = sysreg_read_cntp_ctl_el0();

        // Enable the timer
        cntp_ctl_el0 |= (1 << 0);

        sysreg_write_cntp_ctl_el0(cntp_ctl_el0);
    }

    {
        uint64_t CNTP_CVAL_EL0 = (uint64_t) 0xffffffff | (uint64_t) 0xffffffff << 32;
        sysreg_write_cntp_cval_el0(CNTP_CVAL_EL0);
    }

    systime_frequency = timestamp_freq();
    /* The timeslice is in ms, so divide by 1000. */
    kernel_timeslice = ns_to_systime(timeslice * 1000000);

    printf("System counter frequency is %uHz.\n", timestamp_freq());
    printf("Timeslice interrupt every %u ticks (%dms).\n",
            kernel_timeslice, timeslice);

    {
        // Wait for n time units, close to cycles
        sysreg_write_cntp_tval_el0(100);
        uint32_t cntp_ctl_el0;
        do {
            cntp_ctl_el0 = sysreg_read_cntp_ctl_el0();
        } while ((cntp_ctl_el0 & 4) == 0);

    }

    {
        uint32_t cntp_tval_el0 = timeslice * timestamp_freq() / 1000 ;
        sysreg_write_cntp_tval_el0(cntp_tval_el0);
    }

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

uint64_t timestamp_read(void)
{
    return sysreg_read_cntpct_el0();
}

uint32_t timestamp_freq(void)
{
    return sysreg_read_cntfrq_el0();
}

bool timer_interrupt(uint32_t irq)
{
    printk(LOG_NOTE, "Got interrupt %d\n", irq);
    return 0;
}

void timer_timeout(uint32_t ms)
{
    uint32_t cntp_tval_el0 = ms * timestamp_freq() / 1000 ;
    sysreg_write_cntp_tval_el0(cntp_tval_el0);
    printk(LOG_NOTE, "CNTP_TVAL_EL0=%ld\n", sysreg_read_cntp_tval_el0());
}

systime_t systime_now(void)
{
    return timestamp_read();
}
