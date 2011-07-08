/**
 * \file
 * \brief The world's simplest serial driver.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arm.h>

#include <pl011_uart_dev.h>
#include <pl011_uart.h>

void pl011_uart_init(PL011_UART_t *uart, lvaddr_t base)
{
    PL011_UART_LCR_H_t lcr = {
        .brk = 0, .pen = 0, .eps  = 0, .stp2 = 0, .fen  = 1,
        .wlen = PL011_UART_bits8, .sps  = 0
    };

    PL011_UART_initialize(uart, (mackerel_addr_t) base);

    // Mask all interrupts
    PL011_UART_IMSC_wr_raw(uart, ~0ul);

    // Configure port to 38400 baud, 8 data, no parity, 1 stop (8-N-1)
    //
    // (This is a mild scam as system is running in QEMU)
    //
    // Note baud rate changes not committed in h/w until lcr_h written.

    PL011_UART_IBRD_wr_raw(uart, 0xc);     // Assuming UARTCLK is 7.3728MHz
    PL011_UART_FBRD_wr_raw(uart, 0);

    PL011_UART_LCR_H_wr(uart, lcr);
}

/** \brief Prints a single character to the default serial port. */
void pl011_putchar(PL011_UART_t *uart, char c)
{
    PL011_UART_DR_un dr_un;

    while (PL011_UART_FR_rd(uart).txff == 1)
        ;

    dr_un.raw  = 0;
    dr_un.val.data = (uint8_t)c;
    PL011_UART_DR_wr(uart, dr_un.val);
}

/** \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char pl011_getchar(PL011_UART_t *uart)
{
    while (PL011_UART_FR_rd(uart).rxfe == 1)
        ;

    return (char) PL011_UART_DR_rd(uart).data;
}
