/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OMAP_UART_H
#define OMAP_UART_H

void omap_uart_init(omap_uart_t *uart, lvaddr_t base);
void omap_putchar(omap_uart_t *uart, char c);
char omap_getchar(omap_uart_t *uart);

#endif
