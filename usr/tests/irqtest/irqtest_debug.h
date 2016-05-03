/*
 * Copyright (c) 2008, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __IRQTEST_DEBUG_H__
#define __IRQTEST_DEBUG_H__

/*****************************************************************
 * Debug printer:
 *****************************************************************/
#define IRQTEST_DEBUG 1

#if defined(IRQTEST_DEBUG) || defined(GLOBAL_DEBUG)
#define IRQ_DEBUG(fmt, ...) printf("irq test: " fmt, ##__VA_ARGS__)
#else
#define IRQ_DEBUG(fmt, ...) ((void)0)
#endif

#endif // __IRQTEST_DEBUG_H__
