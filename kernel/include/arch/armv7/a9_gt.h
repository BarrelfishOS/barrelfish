/**
 * \file
 * \brief ARM Cortex A9 Global Timer driver.
 */

/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef __A9_GT_H__
#define __A9_GT_H__

#include <barrelfish_kpi/types.h>

/*
 * Initialize the timer.  The MMU is on.
 */
void a9_gt_init(lpaddr_t addr);

/*
 * Read timer value
 */
uint64_t a9_gt_read(void);
uint32_t a9_gt_read_low(void);
uint32_t a9_gt_read_high(void);

/* The Cortex-A* private timers are clocked by PERIPHCLK, which is not always
 * discoverable at runtime, so it's an optional boot parameter. */
extern uint32_t periphclk;

/* This is either discovered by the platform boot code, or copied from
 * periphclk. */
extern uint32_t tsc_hz;

/* Platform-specific clock rate discovery. */
void a9_probe_tsc(void);

#endif // __A9_GT_H__
