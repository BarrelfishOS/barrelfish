/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __RPI3_MINIUART_H__
#define __RPI3_MINIUART_H__

#include <barrelfish_kpi/types.h>
#include <stdbool.h>

/*
 * \briefConfigure a port.
 *
 * This happens at system startup, and before the MMU is turned on.
 * The hardware is not initialized by this call.
 * After this, the UART is (hopefully) usable, but after the MMU is
 * enabled the OS should then call rpi3_miniuart_init below.
 */
extern void rpi3_minuart_configure(lpaddr_t addr);

/*
 * \brief Initialize a UART, and a number to refer to it in the
 * future.
 *
 * \param port : Physical address of the UART.
 * \param hwinit : Also init the hardware itself if True
 */
extern void rpi3_miniuart_init(lvaddr_t base, bool hwinit);

#endif // __PL011_H__
