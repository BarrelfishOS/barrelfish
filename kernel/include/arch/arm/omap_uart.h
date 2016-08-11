/**
 * \file
 * \brief Kernel serial driver for the OMAP44xx UARTs.  
 */

/*
 * Copyright (c) 2012-2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __OMAP_UART_H__
#define __OMAP_UART_H__

#include <barrelfish_kpi/types.h>
#include <stdbool.h>

/*
 * Initialize UARTs before the MMU is on.
 */
extern void omap_uart_early_init(unsigned port, lpaddr_t base);

/*
 * Re-initialize UARTs after the MMU is on.
 */
extern void omap_uart_init(unsigned port, lvaddr_t base, bool initialize_hw);

/**
 * \brief Prints a single character to a serial port. 
 */
extern void omap_uart_putchar(unsigned port, char c);

/** 
 * \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
extern char omap_uart_getchar(unsigned port);

#endif // __OMAP_UART_H__

