/**
 * \file
 * \brief ARM sp804 dual timer module kernel interface.
 */


/*
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __SP804_UART_H__
#define __SP804_UART_H__

#include <barrelfish_kpi/types.h>

/** 
 * \brief Configure the SP804 modules.
 *
 * We expose the timers individually, and simply trust that the rest
 * of the CPU driver knows that each pair of timers shares a single
 * interrupt line.   We also require that the MMU is turned on at this
 * point. 
 * 
 * \param n the number of modules (half the number of timers)
 * \param hz the clock frequency of the timers in Hertz
 * \param addrs an array of n physical addresses of sp804 devices
 * \param irqs an array of n irq numbers for the devices
 *
 * Each SP804 has two timers.  We number timers sequentially; the
 * number supplied to this function is the number of SP804s (i.e. half
 * the number of timers). 
 */
extern void sp804_configure(size_t n, 
			    uint32_t hz, 
			    const lpaddr_t pit_addrs[], 
			    const uint32_t irqs[] );

#endif // __SP804_UART_H__
