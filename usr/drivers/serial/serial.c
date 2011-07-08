/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <pci/pci.h>
#include "serial.h"
#include "pc16550d_uart_dev.h"

static struct PC16550D_UART_t uart;
static uint16_t portbase;

static void serial_interrupt(void *arg)
{
    PC16550D_UART_iir_t iir = PC16550D_UART_iir_rd(&uart);

    // Assert no error happened
    assert(PC16550D_UART_iir_iid_extract(iir) != PC16550D_UART_rls
           && PC16550D_UART_iir_iid_extract(iir) != PC16550D_UART_ms);

    // Read serial port just like with polling
    serial_poll();
}

static void real_init(void)
{
    // Initialize Mackerel with base port
    PC16550D_UART_initialize(&uart, portbase);

    // enable interrupt
    PC16550D_UART_ier_t ier = PC16550D_UART_ier_default;
    ier = PC16550D_UART_ier_erbfi_insert(ier, 1);
    PC16550D_UART_ier_wr(&uart, ier);

    // enable FIFOs
    PC16550D_UART_fcr_t fcr = PC16550D_UART_fcr_default;
    fcr = PC16550D_UART_fcr_fifoe_insert(fcr, 1);
    // FIFOs hold 14 bytes
    fcr = PC16550D_UART_fcr_rtrigger_insert(fcr, PC16550D_UART_bytes14);
    PC16550D_UART_fcr_wr(&uart, fcr);

    PC16550D_UART_lcr_t lcr = PC16550D_UART_lcr_default;
    lcr = PC16550D_UART_lcr_wls_insert(lcr, PC16550D_UART_bits8); // 8 data bits
    lcr = PC16550D_UART_lcr_stb_insert(lcr, 1); // 1 stop bit
    lcr = PC16550D_UART_lcr_pen_insert(lcr, 0); // no parity
    PC16550D_UART_lcr_wr(&uart, lcr);

    // set data terminal ready
    PC16550D_UART_mcr_t mcr = PC16550D_UART_mcr_default;
    mcr = PC16550D_UART_mcr_dtr_insert(mcr, 1);
    mcr = PC16550D_UART_mcr_out_insert(mcr, 2);
    PC16550D_UART_mcr_wr(&uart, mcr);

    // Set baudrate (XXX: hard-coded to 115200)
    PC16550D_UART_lcr_dlab_wrf(&uart, 1);
    PC16550D_UART_dl_wr(&uart, PC16550D_UART_baud115200);
    PC16550D_UART_lcr_dlab_wrf(&uart, 0);

    // offer service now we're up
    start_service();
}

int serial_init(uint16_t portbase_arg, uint8_t irq)
{
    int r = pci_client_connect();
    assert(r == 0); // XXX

    portbase = portbase_arg;

    return pci_register_legacy_driver_irq(real_init, portbase, portbase+8,
                                          irq, serial_interrupt, NULL);
}

static void serial_putc(char c)
{
    // Wait until FIFO can hold more characters
    while(!PC16550D_UART_lsr_thre_rdf(&uart));
    // Write character
    PC16550D_UART_thr_wr(&uart, c);
}

void serial_write(char *c, size_t len)
{
    for (int i = 0; i < len; i++) {
        serial_putc(c[i]);
    }
}

void serial_poll(void)
{
    // Read as many characters as possible from FIFO
    while(PC16550D_UART_lsr_dr_rdf(&uart)) {
        char c = PC16550D_UART_rbr_rd(&uart);
        serial_input(&c, 1);
    }
}
