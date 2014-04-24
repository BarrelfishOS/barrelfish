/**
 * \file
 * \brief A basic (virtual) serial output for the Xeon Phi
 *
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <x86.h>
#include <serial.h>

#include <dev/xeon_phi/xeon_phi_serial_dev.h>

#define SBOX_BASE               0x08007D0000ULL
#define SBOX_SCRATCH14          0x0000AB58
#define SBOX_SCRATCH15          0x0000AB5C
#define SBOX_SCRATCH8           0x0000AB40

static xeon_phi_serial_t mmio_serial;

/* todo: get rid of those */
int serial_portbase;


/** \brief Initialise the serial driver. */
errval_t serial_init(lvaddr_t base) {

    xeon_phi_serial_initialize(&mmio_serial, (mackerel_addr_t)base);

	// XXX: if non-BSP core, assume HW is already initialised
	if (!arch_core_is_bsp()) {
		return SYS_ERR_OK;
	}

	return SYS_ERR_OK;
}

errval_t serial_early_init(void) {

    xeon_phi_serial_initialize(&mmio_serial, (mackerel_addr_t)SBOX_BASE);

    xeon_phi_serial_ctrl_rawwr(&mmio_serial, xeon_phi_serial_reset);

	return SYS_ERR_OK;
}

xeon_phi_serial_ctrl_t sctrl = xeon_phi_serial_reset;
xeon_phi_serial_data_t sdata = xeon_phi_serial_reset;


uint32_t didx = 0;

#define SERIAL_TIMEOUT 0xFFFFFF

/** \brief Prints a single character to the default serial port. */
void serial_putchar(char c) {
	switch (didx) {
        case 0:
            sctrl = xeon_phi_serial_ctrl_value0_insert(sctrl, xeon_phi_serial_data);
            sdata = xeon_phi_serial_data_value0_insert(sdata, c);
            break;
        case 1:
            sctrl = xeon_phi_serial_ctrl_value1_insert(sctrl, xeon_phi_serial_data);
            sdata = xeon_phi_serial_data_value1_insert(sdata, c);
            break;
        case 2:
            sctrl = xeon_phi_serial_ctrl_value2_insert(sctrl, xeon_phi_serial_data);
            sdata = xeon_phi_serial_data_value2_insert(sdata, c);
            break;
        case 3:
            sctrl = xeon_phi_serial_ctrl_value3_insert(sctrl, xeon_phi_serial_data);
            sdata = xeon_phi_serial_data_value3_insert(sdata, c);
            break;
	};

	++didx;

	if (c == '\n' || didx == 4) {
		// write
	    volatile uint32_t timeout = SERIAL_TIMEOUT;
	    while ((xeon_phi_serial_ctrl_rd(&mmio_serial)) && (timeout--))
	            ;

	    xeon_phi_serial_data_rawwr(&mmio_serial, sdata);
	    xeon_phi_serial_ctrl_rawwr(&mmio_serial, sctrl);

		sdata = xeon_phi_serial_data_initial;
		sctrl = xeon_phi_serial_ctrl_initial;
		didx = 0;
	}
}

/** \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char serial_getchar(void) {
	return '\0';
}
