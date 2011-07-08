/**
 * \file
 * \brief The world's simplest serial driver.
 */

/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arm.h>

#include <ixp2800_uart_dev.h>
#include <ixp2800_uart.h>

/** \brief Initialize uart  */
void ixp2800_uart_init(IXP2800_UART_t *uart, lvaddr_t base)
{
    IXP2800_UART_initialize(uart, (mackerel_addr_t) base);

    IXP2800_UART_LCR_t lcr = IXP2800_UART_LCR_default;
    
    lcr = IXP2800_UART_LCR_sb_insert(lcr, 0);
    lcr = IXP2800_UART_LCR_eps_insert(lcr, 0);
    lcr = IXP2800_UART_LCR_pen_insert(lcr, 0);
    lcr = IXP2800_UART_LCR_wls_insert(lcr, IXP2800_UART_bits8);
    lcr = IXP2800_UART_LCR_stkyp_insert(lcr, 0);
    
    IXP2800_UART_LCR_wr(uart, lcr);


    //Mask all interrupts
    IXP2800_UART_IER_t ier = IXP2800_UART_IER_default;
    
    ier = IXP2800_UART_IER_ravie_insert(ier, 0);
    ier = IXP2800_UART_IER_tie_insert(ier, 0);
    ier = IXP2800_UART_IER_rlse_insert(ier, 0);
    ier = IXP2800_UART_IER_rtoie_insert(ier, 0);
    ier = IXP2800_UART_IER_nrze_insert(ier, 0);
    ier = IXP2800_UART_IER_uue_insert(ier, 1);

    IXP2800_UART_IER_wr(uart, ier);


    //Check current baud rate setting
    
    //Set DLAB to high to access the divisor
    IXP2800_UART_LCR_dlab_wrf(uart, 1);
    
    //Reset DLAB to low
    IXP2800_UART_LCR_dlab_wrf(uart, 0);

}

/** \brief Prints a single character to the default serial port. */
void ixp2800_putchar(IXP2800_UART_t *uart, char c)
{
    while(IXP2800_UART_LSR_tdrq_rdf(uart) == 0){ } //Spin until data transmission request
    
    IXP2800_UART_THR_thr_wrf(uart, c);
}

/** \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char ixp2800_getchar(IXP2800_UART_t *uart)
{
    char c;

    while (IXP2800_UART_LSR_dr_rdf(uart) == 0){ } //Spin until character is ready

    c = (char) IXP2800_UART_RBR_rbr_rdf(uart);

    return c;

}
