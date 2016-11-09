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
/*
 * Timers
 */
void     timers_init(void)
{
    uint64_t CNTKCTL_EL1;
     __asm volatile("mrs %[CNTKCTL_EL1], CNTKCTL_EL1" : [CNTKCTL_EL1] "=r" (CNTKCTL_EL1));
     CNTKCTL_EL1 |= (1 << 9); // Dont trap access to CNTP_* to EL1
     CNTKCTL_EL1 |= (1 << 8); // Dont trap access to CNTV_* to EL1
     CNTKCTL_EL1 |= (1 << 1); // Dont trap access to CNTFRQ* to EL1
     CNTKCTL_EL1 |= (1 << 0); // Dont trap access to CNTFRQ* to EL1
     __asm volatile("msr CNTKCTL_EL1, %[CNTKCTL_EL1]" : : [CNTKCTL_EL1] "r" (CNTKCTL_EL1));


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

    return 0;
}

uint32_t timestamp_freq(void)
{
    return 1;
}

bool     timer_interrupt(uint32_t irq)
{
    return 0;
}
