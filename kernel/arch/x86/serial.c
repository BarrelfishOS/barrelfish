/**
 * \file
 * \brief The world's simplest serial driver.
 *
 * AB's quick-and-nasty serial driver.
 * FIXME: make platform independent
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <x86.h>
#include <serial.h>
#include "pc16550d_dev.h"

int serial_portbase = 0x3f8; // COM1 default, can be changed via command-line arg

#define NIBBLE 4
#define HEX2ASCII 0x30
#define HEXLETTER 0x3a
#define HEXCORRECTION 0x7

static pc16550d_t uart;

/** \brief Initialise the serial driver. */
errval_t serial_console_init(uint8_t ordinal)
{
    assert(ordinal == 0); // multiple ports NYI

    pc16550d_initialize(&uart, serial_portbase);

    // XXX: if non-BSP core, assume HW is already initialised
    if (!arch_core_is_bsp()) {
        return SYS_ERR_OK;
    }

    // Initialize UART
    // disable interrupt
    pc16550d_ier_t ier = pc16550d_ier_default;
    ier = pc16550d_ier_erbfi_insert(ier, 0);
    pc16550d_ier_wr(&uart, ier);

    // enable FIFOs
    pc16550d_fcr_t fcr = pc16550d_fcr_default;
    fcr = pc16550d_fcr_fifoe_insert(fcr, 1);
    // FIFOs hold 14 bytes
    fcr = pc16550d_fcr_rtrigger_insert(fcr, pc16550d_bytes14);
    pc16550d_fcr_wr(&uart, fcr);

    pc16550d_lcr_t lcr = pc16550d_lcr_default;
    lcr = pc16550d_lcr_wls_insert(lcr, pc16550d_bits8); // 8 data bits
    lcr = pc16550d_lcr_stb_insert(lcr, 1); // 1 stop bit
    lcr = pc16550d_lcr_pen_insert(lcr, 0); // no parity
    pc16550d_lcr_wr(&uart, lcr);

    // set data terminal ready
    pc16550d_mcr_t mcr = pc16550d_mcr_default;
    mcr = pc16550d_mcr_dtr_insert(mcr, 1);
    mcr = pc16550d_mcr_out_insert(mcr, 2);
    pc16550d_mcr_wr(&uart, mcr);

    // Set baudrate (XXX: hard-coded to 115200)
    if (!CPU_IS_M5_SIMULATOR) {
        pc16550d_lcr_dlab_wrf(&uart, 1);
        pc16550d_dl_wr(&uart, pc16550d_baud115200);
        pc16550d_lcr_dlab_wrf(&uart, 0);
    }

    return SYS_ERR_OK;
}

/** \brief Prints a single character to the default serial port. */
void serial_console_putchar(char c)
{
    // Wait until FIFO can hold more characters
    while(!pc16550d_lsr_thre_rdf(&uart));
    // Write character
    pc16550d_thr_wr(&uart, c);
}

/** \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char serial_console_getchar(void)
{
    // Read as many characters as possible from FIFO
    while( !pc16550d_lsr_dr_rdf(&uart));
    return pc16550d_rbr_rd(&uart);
}

errval_t serial_debug_init(uint8_t ordinal)
{
    // NOP - for now uses serial console port
    return SYS_ERR_OK;
}

void serial_debug_putchar(char c)
{
    serial_console_putchar(c);
}

char serial_debug_getchar(void)
{
    return serial_console_getchar();
}
