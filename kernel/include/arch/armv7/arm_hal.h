/**
 * \file
 * \brief Hardware Abstraction Layer interface for ARM boards.
 *
 * This file defines the hardware abstraction layer for ARM targets. Each
 * board is expected to have an implementation that corresponds to this
 * interface.
 *
 * This interface is expected to change as new boards are added.
 */

/*
 * Copyright (c) 2007, 2009, 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARM_HAL_H__
#define __ARM_HAL_H__

#include <barrelfish_kpi/types.h>
#include <stdbool.h>

/*
 * Timer
 */
extern size_t pit_num_timers(void);
extern void pit_init(uint32_t timeslice, size_t timer);
extern void pit_start(size_t timer);
extern bool pit_handle_irq(uint32_t irq);
extern void pit_mask_irq(bool masked, size_t timer);

/*
 * Time-stamp counter
 */
void     tsc_init(int timeslice);
uint32_t tsc_read(void);
uint32_t tsc_get_hz(void);

/*
 * Cortex A9 Global Timer
 *
 * XXX: In our code, these are only defined for the pandaboard. Not sure what
 * is the case for other platforms.
 */
void     gt_init(void);
uint64_t gt_read(void);
uint32_t gt_read_low(void);
uint32_t gt_read_high(void);

#endif // __ARM_HAL_H__
