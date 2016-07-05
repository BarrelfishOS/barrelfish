/**
 * \file
 * \brief Kernel serial driver for the Xilinx Zynq7000-series UART
 */

/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <arm.h>
#include <dev/zynq7/zynq_uart_dev.h>
#include <paging_kernel_arch.h>
#include <zynq_uart.h>
#include <zynq7_map.h>

/* Serial console and debugger interfaces. */
static zynq_uart_t ports[ZYNQ_UART_MAX_PORTS];
static lpaddr_t port_addrs[ZYNQ_UART_MAX_PORTS];
static bool port_inited[ZYNQ_UART_MAX_PORTS];

static void zynq_uart_hw_init(zynq_uart_t *uart);

#define MSG(port, format, ...) \
    printk( LOG_NOTE, "ZYNQ serial[%d]: "format, port, ## __VA_ARGS__ )

void
zynq_uart_early_init(unsigned port, lpaddr_t base) {
    assert(port < ZYNQ_UART_MAX_PORTS);
    assert(ports[port].base == 0);

    port_addrs[port] = base;
    zynq_uart_initialize(&ports[port], (mackerel_addr_t)base);
}

void
zynq_uart_init(unsigned port, bool initialize_hw) {
    assert(port < ZYNQ_UART_MAX_PORTS);
    /* Ensure port has already been through early_init. */
    assert(port_addrs[port] != 0);

    /* All devices seem to be 4k-aligned, which is nice. */
    lvaddr_t base = paging_map_device(port_addrs[port], 0x1000);

    MSG(port, "base = 0x%"PRIxLVADDR"\n", base);
    zynq_uart_initialize(&ports[port], (mackerel_addr_t) base);
    if(initialize_hw && !port_inited[port]) {
        zynq_uart_hw_init(&ports[port]);
        port_inited[port] = true;
    }
    MSG(port,"done.\n");
}

/*
 * Initialise Zynq UART
 * Zynq TRM S19.3.1
 */
static void
zynq_uart_hw_init(zynq_uart_t *uart) {
    panic("Unimplemented\n");
}

/**
 * \brief Prints a single character to a serial port. 
 */
void
zynq_uart_putchar(unsigned port, char c) {
    assert(port <= ZYNQ_UART_MAX_PORTS);
    zynq_uart_t *uart = &ports[port];

    /* Wait until FIFO can hold more characters. */
    while(zynq_uart_SR_TXFULL_rdf(uart));

    /* Write character. */
    zynq_uart_FIFO_FIFO_wrf(uart, c);
}

/** 
 * \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char
zynq_uart_getchar(unsigned port) {
    assert(port <= ZYNQ_UART_MAX_PORTS);
    zynq_uart_t *uart = &ports[port];

    /* Wait until there is at least one character in the FIFO. */
    while(zynq_uart_SR_RXEMPTY_rdf(uart));

    /* Return the character. */
    return zynq_uart_FIFO_FIFO_rdf(uart);
}
