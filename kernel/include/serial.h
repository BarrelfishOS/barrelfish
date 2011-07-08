/*
 * Copyright (c) 2007, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __SERIAL_H
#define __SERIAL_H

/* Need to include this for errval_t */
#include <errors/errno.h>

extern int serial_portbase;

errval_t serial_console_init(uint8_t port_ordinal);
void serial_console_putchar(char c);
char serial_console_getchar(void);

errval_t serial_debug_init(uint8_t port_ordinal);
void serial_debug_putchar(char c);
char serial_debug_getchar(void);

#endif //__SERIAL_H
