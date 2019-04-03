/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __PL011_H__
#define __PL011_H__

#include <barrelfish_kpi/types.h>
#include <stdbool.h>

/*
 * We can handle up to 6 UARTS; should be enough for anyone...
 */
#define PL011_MAX_PORTS 6

/*
 * \briefConfigure a port.  
 *
 * This happens at system startup, and before the MMU is turned on.
 * The hardware is not initialized by this call. 
 * After this, the UART is (hopefully) usable, but after the MMU is
 * enabled the OS should then call pl011_init below. 
 */
extern void pl011_configure(unsigned port, lpaddr_t addr);

/*
 * \brief Initialize a UART, and a number to refer to it in the
 * future.
 *
 * \param port : Physical address of the UART.
 * \param hwinit : Also init the hardware itself if True
 */
extern void pl011_init(unsigned port, lvaddr_t base, bool hwinit);

/*
 * \brief Put a character to the port
 */
extern void pl011_putchar(unsigned port, char c);

/*
 * \brief Read a character from a port
 */
extern char pl011_getchar(unsigned port);

#endif // __PL011_H__
