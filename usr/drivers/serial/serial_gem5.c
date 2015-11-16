/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <pci/pci.h>
#include <barrelfish/inthandler.h>
#include <driverkit/driverkit.h>
#include "serial.h"
#include <dev/pl011_uart_dev.h>

static struct pl011_uart_t uart;
//static uint16_t portbase;

#define INTERRUPTS_MASK		0x0070

#define NUM_PORTS 2
unsigned serial_console_port = 0;
unsigned serial_debug_port = 0;
const unsigned serial_num_physical_ports = NUM_PORTS;


#define UART0_VBASE            0x1c090000
#define UART0_SECTION_OFFSET   0x90000

#define UART_DEVICE_BYTES	0x4c
#define UART_MAPPING_DIFF	0x1000

static void serial_poll(void)
{
    // Read as many characters as possible from FIFO
    while (pl011_uart_FR_rd(&uart).rxfe == 1)
        ;

    char c = (char) pl011_uart_DR_rd(&uart).data;

    serial_input(&c, 1);
}

static void serial_interrupt(void *arg)
{
    //pc16550d_iir_t iir = pc16550d_iir_rd(&uart);

    // Assert no error happened
    //assert(pc16550d_iir_iid_extract(iir) != pc16550d_rls
    //       && pc16550d_iir_iid_extract(iir) != pc16550d_ms);

    // Read serial port just like with polling
    serial_poll();
}

static void real_init(lvaddr_t base)
{
    pl011_uart_LCR_H_t lcr = {
        .brk = 0, .pen = 0, .eps  = 0, .stp2 = 0, .fen  = 1,
        .wlen = pl011_uart_bits8, .sps  = 0
    };

    pl011_uart_initialize(&uart, (mackerel_addr_t) base);

    // Mask all interrupts
    // gem5 implementation of pl011 only supports
    // RXIM, TXIM, RTIM interrupts, so we only mask them
    pl011_uart_IMSC_wr_raw(&uart, INTERRUPTS_MASK);

    // Configure port to 38400 baud, 8 data, no parity, 1 stop (8-N-1)
    //
    // (This is a mild scam as system is running in QEMU)
    //
    // Note baud rate changes not committed in h/w until lcr_h written.

    pl011_uart_IBRD_wr_raw(&uart, 0xc);     // Assuming UARTCLK is 7.3728MHz
    pl011_uart_FBRD_wr_raw(&uart, 0);

    pl011_uart_LCR_H_wr(&uart, lcr);

    // offer service now we're up
    //start_service();
}

errval_t serial_init(uint16_t lportbase, uint8_t irq)
{

	lportbase = 0;

    if (lportbase < NUM_PORTS) {

	    // XXX: TODO: figure this out --> kaluga magic?
    	errval_t err;
	    lvaddr_t base;
    	err = map_device_register(UART0_VBASE + lportbase*UART_MAPPING_DIFF, UART_DEVICE_BYTES, &base);
	    if (err_is_fail(err)) {
    	    USER_PANIC_ERR(err, "map_device_register failed\n");
        	return err;
	    }

    	assert(base);

        //real_init(base + UART0_SECTION_OFFSET + lportbase*UART_MAPPING_DIFF);
		real_init(base + lportbase*UART_MAPPING_DIFF);

    	// register interrupt
	    uint16_t *pb = malloc(sizeof(uint16_t));
    	*pb = lportbase;
	    err = inthandler_setup_arm(serial_interrupt, pb, 44);
    	if (err_is_fail(err)) {
        	USER_PANIC_ERR(err, "interrupt setup failed.");
	    }

    	// offer service now we're up
	    start_service();
    	return SYS_ERR_OK;
		
    } else {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }

}

static void serial_putc(char c)
{
    pl011_uart_DR_un dr_un;

    while (pl011_uart_FR_rd(&uart).txff == 1)
        ;

    dr_un.raw  = 0;
    dr_un.val.data = (uint8_t)c;
    pl011_uart_DR_wr(&uart, dr_un.val);
}

void serial_write(char *c, size_t len)
{
    for (int i = 0; i < len; i++) {
        serial_putc(c[i]);
    }
}
