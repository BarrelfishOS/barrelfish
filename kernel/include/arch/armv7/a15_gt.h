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

#include <kernel.h>

/* Return the current value of the CNTFRQ (count frequency) register, the rate
 * at which the system counter counts. */
static inline uint32_t
a15_gt_frequency(void) {
    uint32_t cntfrq;
    __asm volatile("mrc p15, 0, %0, c14, c0, 0" : "=r"(cntfrq));
    return cntfrq;
}

/* Return the current counter value. */
static inline uint64_t
a15_gt_counter(void) {
    uint32_t lo, hi;
    __asm volatile("mrrc p15, 0, %0, %1, c14" : "=r"(lo), "=r"(hi));
    return (((uint64_t)hi) << 32) | lo;
}

/* Trigger a timeout interrupt t counter ticks from now. */
static inline void
a15_gt_timeout(uint32_t t) {
    /* Write CNTP_TVAL, the physical counter timer value register. */
    __asm volatile("mcr p15, 0, %0, c14, c2, 0" : : "r"(t));
}

void a15_gt_init(void);
