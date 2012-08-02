/**
 * \file
 * \brief The world's simplest serial driver.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arm.h>

#include <omap_uart_dev.h>
#include <omap_uart.h>

void omap_uart_init(omap_uart_t *uart, lvaddr_t base)
{
    omap_uart_initialize(uart, (mackerel_addr_t) base);

    // Disable all interrupts
    omap_uart_IER_t ier = omap_uart_IER_default;
    omap_uart_IER_wr(uart, ier);

    // Enable FIFOs and select highest trigger levels
    omap_uart_FCR_t fcr = omap_uart_FCR_default;
    fcr = omap_uart_FCR_rx_fifo_trig_insert(fcr, 3);
    fcr = omap_uart_FCR_tx_fifo_trig_insert(fcr, 3);
    fcr = omap_uart_FCR_fifo_en_insert(fcr, 1);
    omap_uart_FCR_wr(uart, fcr);

    // Set line to 8N1
    omap_uart_LCR_t lcr = omap_uart_LCR_default;
    lcr = omap_uart_LCR_parity_en_insert(lcr, 0);       // No parity
    lcr = omap_uart_LCR_nb_stop_insert(lcr, 0);         // 1 stop bit
    lcr = omap_uart_LCR_char_length_insert(lcr, omap_uart_wl_8bits);      // 8 data bits
    omap_uart_LCR_wr(uart, lcr);
}

/** \brief Prints a single character to the default serial port. */
void omap_putchar(omap_uart_t *uart, char c)
{
    // Wait until FIFO can hold more characters
    while(!omap_uart_LSR_tx_fifo_e_rdf(uart));
    // Write character
    omap_uart_THR_thr_wrf(uart, c);
}

/** \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char omap_getchar(omap_uart_t *uart)
{
    return omap_uart_RHR_rhr_rdf(uart);
}
