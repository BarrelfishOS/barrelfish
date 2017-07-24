/**
 * \file
 * \brief Platform interface for ARMv7-A boards.
 *
 * This file defines the hardware abstraction layer for ARM targets. Each
 * board is expected to have an implementation that corresponds to this
 * interface.
 *
 * This interface is expected to change as new boards are added.
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARMv8_TIMER_H_
#define __ARMv8_TIMER_H_

#include <barrelfish/types.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/platform.h>

#include <sysreg.h>
#include <dev/armv8_dev.h>

/*
 * Timers
 */

/**
 * @brief initialize the timers
 */
void timers_init(int timeslice);


/**
 * @brief resets the timer to fire in $ms milliseconds
 *
 * @param ms millisecons
 */
void timer_reset(uint64_t ms);

/**
 * @brief obtains the current frequency
 *
 * @return returns the frequency in Hz
 */
static inline cycles_t timer_get_frequency(void)
{
    return armv8_CNTFRQ_EL0_rd(NULL);
}

/**
 * @brief get the current timer value
 *
 * @return returns the current timer value in clock cycles
 */
static inline cycles_t timer_get_timestamp(void)
{
    return armv8_CNTPCT_EL0_rd(NULL);
}

/**
 * @brief queries the timer if it is set
 *
 * @return TRUE if the timer has not fired yet
 */
static inline volatile bool timer_is_set(void)
{
    return (armv8_CNTP_CTL_EL0_ISTATUS_rdf(NULL) == 0);
}

#endif // __ARMv8_TIMER_H_
