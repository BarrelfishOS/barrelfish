/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ZYNQ_UART_H__
#define __ZYNQ_UART_H__

#include <barrelfish_kpi/types.h>
#include <stdbool.h>

#define ZYNQ_UART_MAX_PORTS 2

/*
 * \brief Configure a port.  
 *
 * This happens at system startup, and before the MMU is turned on.
 * The hardware is not initialized by this call. 
 * After this, the UART is (hopefully) usable, but after the MMU is
 * enabled the OS should then call zynq_uart_init below. 
 */
extern void zynq_uart_early_init(unsigned port, lpaddr_t addr);

/*
 * \brief Initialize a UART, and a number to refer to it in the
 * future.
 *
 * \param port : Physical address of the UART.
 * \param hwinit : Also init the hardware itself if True
 */
extern void zynq_uart_init(unsigned port, lvaddr_t base, bool hwinit);

/*
 * \brief Put a character to the port
 */
extern void zynq_uart_putchar(unsigned port, char c);

/*
 * \brief Read a character from a port
 */
extern char zynq_uart_getchar(unsigned port);

#endif // __ZYNQ_UART_H__
