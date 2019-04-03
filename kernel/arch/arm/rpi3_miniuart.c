/**
 * \file
 * \brief ARM pl011 UART kernel-level driver.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <arch/arm/rpi3_miniuart.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <arch/arm/arm.h>
#include <arch/arm/platform.h>
#include <serial.h>
#include <dev/rpi3_miniuart_dev.h>

static rpi3_miniuart_t uart;
#define MSG(format, ...) printk( LOG_NOTE, "rpi3_miniuart: "format, ## __VA_ARGS__ )

/*
 * \brief Initialize a serial port.  The MMU is turned on.
 */
void rpi3_miniuart_init(lvaddr_t base, bool hwinit)
{
    // assert(paging_mmu_enabled());
    // assert(port < serial_num_physical_ports);
    //
    // pl011_uart_t *u = &uarts[port];
    //
    // // [Re]initialize the Mackerel state for the UART
    rpi3_miniuart_initialize(&uart, (mackerel_addr_t) base);
    //
    // if (hwinit) {
    //     // Mask all interrupts: set all bits to zero
    //     pl011_uart_IMSC_rawwr(u, INTERRUPTS_MASK);
    //
    //     // Disable the UART before reconfiguring it.
    //     pl011_uart_CR_uarten_wrf(u, 0);
    //
    //     // Clear and enable the receive interrupt.
    //     pl011_uart_ICR_rxic_wrf(u, 1);
    //     pl011_uart_IMSC_rxim_wrf(u, 1);
    //
    //     // Configure port to 38400 baud, 8 data, no parity, 1 stop (8-N-1)
    //     //
    //     // (This is a mild scam as system is running in QEMU)
    //     //
    //     // Note baud rate changes not committed in h/w until lcr_h
    //     // written.
    //     pl011_uart_IBRD_divint_wrf(u, 0xc); // Assuming UARTCLK is 7.3728MHz
    //     pl011_uart_FBRD_divfrac_wrf(u, 0);
    //
    //     /* Configure the line control register. */
    //     pl011_uart_LCR_H_t lcr= (pl011_uart_LCR_H_t)0;
    //     /* Disable FIFOs.  There's no way to get an interrupt when a single
    //      * character arrives with FIFOs, so it's useless as a console. */
    //     lcr= pl011_uart_LCR_H_fen_insert(lcr, 0);
    //     /* Eight data bits. */
    //     lcr= pl011_uart_LCR_H_wlen_insert(lcr, pl011_uart_bits8);
    //     /* No parity. */
    //     lcr= pl011_uart_LCR_H_pen_insert(lcr, 0);
    //     /* One stop bit. */
    //     lcr= pl011_uart_LCR_H_stp2_insert(lcr, 0);
    //     pl011_uart_LCR_H_wr(u, lcr);
    //
    //     /* Configure the main control register. */
    //     pl011_uart_CR_t cr = (pl011_uart_CR_t)0;
    //     /* No flow control. */
    //     cr= pl011_uart_CR_ctsen_insert(cr, 0);
    //     cr= pl011_uart_CR_rtsen_insert(cr, 0);
    //     /* Enable transmit and receive. */
    //     cr= pl011_uart_CR_txe_insert(cr, 1);
    //     cr= pl011_uart_CR_rxe_insert(cr, 1);
    //     /* Enable UART. */
    //     cr= pl011_uart_CR_uarten_insert(cr, 1);
    //     pl011_uart_CR_wr(u, cr);
    // }
}

/*
 * \brief Put a character to the port
 */
void serial_putchar(unsigned port, char c)
{
    while(rpi3_miniuart_STAT_txff_rdf(&uart) == 1) ;
    rpi3_miniuart_DR_rawwr(&uart, c);
    // assert(port < MAX_PORTS);
    // pl011_uart_t *u = &uarts[port];
    // assert(u->base != 0);
    //
    // while(pl011_uart_FR_txff_rdf(u) == 1) ;
    // pl011_uart_DR_rawwr(u, c);
}

/*
 * \brief Read a character from a port
 */
char serial_getchar(unsigned port)
{
    // assert(port < MAX_PORTS);
    // pl011_uart_t *u = &uarts[port];
    // assert(u->base != 0);
    //
    // /* Wait for data. */
    // while(pl011_uart_FR_rxfe_rdf(u) == 1);
    //
    // /* Acknowledge any interrupt. */
    // pl011_uart_ICR_rxic_wrf(u, 1);
    //
    // return (char)pl011_uart_DR_data_rdf(u);
    return 0;
}
