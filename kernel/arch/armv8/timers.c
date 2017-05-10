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
#include <dev/armv8_dev.h>

cycles_t ticks_per_ms = 1;

/*
 * Timers
 */
void timers_init(int timeslice)
{
    printk(LOG_NOTE, "isr_el1=%p\n", sysreg_read_isr_el1());
    {
        //armv8_CNTPS_CTL_EL1_t kctl;
        armv8_CNTKCTL_EL1_t kctl;
        kctl = armv8_CNTKCTL_EL1_rd(NULL);

        /* don't trap access to CNTFRQ* and CNTFRQ* registers from EL0 to EL1 */
        kctl = armv8_CNTKCTL_EL1_EL0PCTEN_insert(kctl, 0x1);
        kctl = armv8_CNTKCTL_EL1_EL0VCTEN_insert(kctl, 0x1);

        /* trap access to CNTP_* and CNTV_* registers from EL0 to EL1 */
        kctl = armv8_CNTKCTL_EL1_EL0PTEN_insert(kctl, 0x0);
        kctl = armv8_CNTKCTL_EL1_EL0VTEN_insert(kctl, 0x0);

        armv8_CNTKCTL_EL1_wr(NULL, kctl);
    }

    /* enable the timer */
    armv8_CNTP_CTL_EL0_IMASK_wrf(NULL, 0x0);
    armv8_CNTP_CTL_EL0_ENABLE_wrf(NULL, 0x1);

    /* set the compare value */
    armv8_CNTP_CVAL_EL0_wr(NULL, 0xffffffffffffffff);


    /* systime_frequency is ticks per milisecond, while timer_get_frequency is in HZ */
    systime_frequency = timer_get_frequency() / 1000;

    /* The timeslice is in ms */
    kernel_timeslice = ns_to_systime(timeslice * 1000000);

    printf("System counter frequency is %uHz.\n", timer_get_frequency());
    printf("Timeslice interrupt every %u ticks (%dms).\n",
            kernel_timeslice, timeslice);

    // Wait for n time units, close to cycles
    armv8_CNTP_TVAL_EL0_wr(NULL, 100);

    while(timer_is_set())
        ;

    timer_reset(timeslice);

    armv8_PMCR_EL0_t pmcr = 0;
    pmcr = armv8_PMCR_EL0_E_insert(pmcr, 1); /* All counters are enabled.*/
    pmcr = armv8_PMCR_EL0_P_insert(pmcr, 1); /* reset all event counters */
    pmcr = armv8_PMCR_EL0_C_insert(pmcr, 1); /* reset all clock counters */
    pmcr = armv8_PMCR_EL0_D_insert(pmcr, 0); /* set counter to tick every clock cycle (1=ever 64th) */
    pmcr = armv8_PMCR_EL0_X_insert(pmcr, 1); /* enable event support */
    pmcr = armv8_PMCR_EL0_DP_insert(pmcr, 0); /* don't disable cycle counter */
    //pmcr = armv8_PMCR_EL0_N_insert(pmcr, 6);  /* N is RO ? */
    armv8_PMCR_EL0_wr(NULL, pmcr);

    armv8_PMUSERENR_EL0_t pmu = 0;
    /* don't trap access to PM registers to EL 1 */
    pmu = armv8_PMUSERENR_EL0_EN_insert(pmu, 1);
    /* don't trap software increment wrap to EL 1 */
    pmu = armv8_PMUSERENR_EL0_SW_insert(pmu, 1);
    /* don't trap cycle counter to EL 1 */
    pmu = armv8_PMUSERENR_EL0_CR_insert(pmu, 1);
    /* don't trap event counter read to EL 1*/
    pmu = armv8_PMUSERENR_EL0_ER_insert(pmu, 1);
    armv8_PMUSERENR_EL0_wr(NULL, pmu);
}

/**
 *
 * @param ms
 */
void timer_reset(uint64_t ms)
{
    armv8_CNTP_TVAL_EL0_wr(NULL, ms * systime_frequency);
}


systime_t systime_now(void)
{
    return timer_get_timestamp();
}
