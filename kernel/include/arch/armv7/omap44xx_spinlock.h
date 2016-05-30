/**
 * \file
 * \brief kernel driver for the spinlock module, used for serial output
 * see OMAP4460 TRM chapter 21 for a functional description
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __OMAP44XX_SPINLOCK_H__
#define __OMAP44XX_SPINLOCK_H__

#include <kernel.h>

#define OMAP44XX_SPINLOCK_NUM_LOCKS 32

/**
 * \brief Map the OMAP44xx spinlock device into kernel virtual memory
 * and reset it. 
 */
extern errval_t omap44xx_spinlock_init(void);

/**
 * \brief acquire an OMAP44xx spinlock.  We repeatedly read the
 * register; 0 means we have the lock, 1 means we have to try again. 
 */
extern void omap44xx_spinlock_acquire( unsigned locknumber );

/**
 * \brief release an OMAP44xx spinlock.
 */
extern void omap44xx_spinlock_release( unsigned locknumber );

#endif  // __OMAP44XX_SPINLOCK_H__
