/*
 * Kernel-level serial driver.  Implements the interface in
 * /kernel/include/serial.h
 *
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaestr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#include <kernel.h>
#include <offsets.h>
#include <serial.h>
#include <dev/pl011_uart_dev.h>
#include <arch/arm/pl011_uart.h>

#define NUM_PORTS 4
unsigned int serial_console_port = 0;
unsigned int serial_debug_port = 1;
const unsigned int serial_num_physical_ports = NUM_PORTS;

#define UART_BASE (0x1c090000 + KERNEL_OFFSET)
#define UART_STEP 0x00010000

pl011_uart_t ports[NUM_PORTS];

errval_t
serial_init(unsigned int port, bool hwinit) {
    if(port >= NUM_PORTS) return SYS_ERR_SERIAL_PORT_INVALID;
    return SYS_ERR_OK;
}

errval_t
serial_early_init(unsigned int port) {
    if(port >= NUM_PORTS) return SYS_ERR_SERIAL_PORT_INVALID;
    pl011_uart_init(&ports[port], UART_BASE + port*UART_STEP);
    return SYS_ERR_OK;
}

void
serial_putchar(unsigned int port, char c) {
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    pl011_putchar(&ports[port], c);
}

char
serial_getchar(unsigned int port) {
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    return pl011_getchar(&ports[port]);
}
