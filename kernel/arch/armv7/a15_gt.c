/**
 * \file
 * \brief ARM Cortex A15 Generic Timer driver.
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <a15_gt.h>

/* Initialise the timer. */
void
a15_gt_init(void) {
    /* CNTFRQ (counter frequency) must have already been set by code that
     * knows the correct frequency.  Note that CNTFRQ doesn't *set* the
     * frequency, it just records it. */

    /* CNTKCTL - PL0 timer access disabled, no event stream, counter
     * (timestamp) is accessible.  */
    uint32_t cntkctl= (1 << 1) /* PL0CVTEN - virtual counter at PL0 */
                    | (1 << 0) /* PL0PVTEN - physical counter at PL0 */;
    __asm volatile("mcr p15, 0, %0, c14, c1, 0" : : "r"(cntkctl));

    /* CNTV_CTL - virtual timer disabled, interrupt masked. */
    uint32_t cntv_ctl= (1 << 1) /* IMASK */;
    __asm volatile("mcr p15, 0, %0, c14, c3, 1" : : "r"(cntv_ctl));

    /* Set the compare value high, so that it doesn't trigger until somebody's
     * called a15_gt_timeout().  Note that the rollover period is guaranteed
     * to be at least 40 years.  See ARMv7 ARM B8.1.1. */
    uint32_t cval_low= 0xffffffff, cval_high= 0xffffffff;
    __asm volatile("mcrr p15, 2, %0, %1, c14" : :
            "r"(cval_low), "r"(cval_high));

    /* CNTP_CTL - physical timer enabled, interrupt unmasked. */
    uint32_t cntp_ctl= (1 << 0) /* ENABLE */;
    __asm volatile("mcr p15, 0, %0, c14, c2, 1" : : "r"(cntp_ctl));

    /* From this point, the current timestamp is available in CNTPCT at PL0 &
     * PL1, and we can trigger interrupts by writing CNTP_CVAL (absolute) or
     * CNTP_TVAL (relative). */
}
