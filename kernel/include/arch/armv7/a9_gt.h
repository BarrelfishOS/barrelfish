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
extern void a9_gt_init(lpaddr_t addr);

/*
 * Read timer value
 */
extern uint64_t a9_gt_read(void);
extern uint32_t a9_gt_read_low(void);
extern uint32_t a9_gt_read_high(void);

#endif // __A9_GT_H__
