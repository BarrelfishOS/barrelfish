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
#include <platform.h>
#include <serial.h>
#include <zynq_uart.h>
#include <zynq7_map.h>

/* Serial console and debugger interfaces. */
static zynq_uart_t ports[ZYNQ_UART_MAX_PORTS];

static void zynq_uart_hw_init(zynq_uart_t *uart);

#define MSG(port, format, ...) \
    printk( LOG_NOTE, "ZYNQ serial[%d]: "format, port, ## __VA_ARGS__ )

/* XXX - rename this. */
errval_t
serial_early_init(unsigned port) {
    assert(port < ZYNQ_UART_MAX_PORTS);

    zynq_uart_initialize(&ports[port], (mackerel_addr_t)uart_base[port]);

    /* Ensure the transmitter is enabled. */
    zynq_uart_CR_tx_dis_wrf(&ports[port], 0);
    zynq_uart_CR_tx_en_wrf(&ports[port], 1);

    return SYS_ERR_OK;
}

void
zynq_uart_init(unsigned port, lvaddr_t base, bool initialize_hw) {
    assert(port < ZYNQ_UART_MAX_PORTS);
    zynq_uart_initialize(&ports[port], (mackerel_addr_t) base);
    if(initialize_hw) zynq_uart_hw_init(&ports[port]);
}

/*
 * Initialise Zynq UART
 * Zynq TRM S19.3.1
 */
static void
zynq_uart_hw_init(zynq_uart_t *uart) {
    /* Disable all interrupts. */
    zynq_uart_IDR_rawwr(uart, 0);

    /* Clear all interrupts. */
    zynq_uart_ISR_rawwr(uart, 0xffffffff);

    /* Trigger an interrupt on a single byte. */
    zynq_uart_RXWM_RTRIG_wrf(uart, 1);

    /* Enable RX trigger interrupt. */
    zynq_uart_IER_rtrig_wrf(uart, 1);

    /* Enable receiver. */
    zynq_uart_CR_rx_dis_wrf(uart, 0);
    zynq_uart_CR_rx_en_wrf(uart, 1);
}

/**
 * \brief Prints a single character to a serial port. 
 */
void
serial_putchar(unsigned port, char c) {
    assert(port <= ZYNQ_UART_MAX_PORTS);
    zynq_uart_t *uart = &ports[port];

    /* Wait until FIFO can hold more characters. */
    while(zynq_uart_SR_TXFULL_rdf(uart));

    /* Write character. */
    zynq_uart_FIFO_FIFO_wrf(uart, c);
}

/** 
 * \brief Reads a single character from the default serial port.
 */
char
serial_getchar(unsigned port) {
    assert(port <= ZYNQ_UART_MAX_PORTS);
    zynq_uart_t *uart = &ports[port];

    /* Drain the FIFO. */
    char c= zynq_uart_FIFO_FIFO_rdf(uart);
    while(!zynq_uart_SR_RXEMPTY_rdf(uart)) {
        c= zynq_uart_FIFO_FIFO_rdf(uart);
    }

    /* Clear the RXTRIG interrupt. */
    zynq_uart_ISR_rtrig_wrf(uart, 1);

    /* Return the character. */
    return c;
}
