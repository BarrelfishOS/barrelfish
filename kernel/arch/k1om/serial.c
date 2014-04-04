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

#define SBOX_BASE               0x08007D0000ULL
#define SBOX_SCRATCH14          0x0000AB58
#define SBOX_SCRATCH15          0x0000AB5C
#define SBOX_SCRATCH8           0x0000AB40

//static uint8_t *sbox_base = (uint8_t *)SBOX_BASE;

/* todo: get rid of those */
unsigned serial_console_port;
unsigned serial_debug_port;
int serial_portbase;

union xeon_phi_message {
	uint32_t val;
	char c[4];
};

/** \brief Initialise the serial driver. */
errval_t serial_init(unsigned port) {

	// XXX: if non-BSP core, assume HW is already initialised
	if (!arch_core_is_bsp()) {
		return SYS_ERR_OK;
	}

	return SYS_ERR_OK;
}

errval_t serial_early_init(unsigned port) {

	/* clear out the control register */
	uint32_t *signal = (uint32_t*) ((void *) SBOX_BASE + SBOX_SCRATCH8);
	*signal = 0;
	return SYS_ERR_OK;
}

union xeon_phi_message data_buf;
union xeon_phi_message data_ctrl;
uint32_t didx = 0;


/** \brief Prints a single character to the default serial port. */
void serial_putchar(unsigned port, char c) {
	volatile uint32_t *ctrl = ((uint32_t *) (SBOX_BASE + SBOX_SCRATCH8));
	volatile uint32_t *data = ((uint32_t *) (SBOX_BASE + SBOX_SCRATCH15));

	data_buf.c[didx] = c;
	data_ctrl.c[didx] = 0x7A;
	++didx;

	if (c == '\n' || didx == 4) {
		// write
		while ((*ctrl))
			;

		*data = data_buf.val;
		(*ctrl) = data_ctrl.val;
		data_buf.val = 0;
		data_ctrl.val = 0;
		didx = 0;
	}
}

/** \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char serial_getchar(unsigned port) {
	assert(!"Not possible to get input from Host");
	return '\0';
}
