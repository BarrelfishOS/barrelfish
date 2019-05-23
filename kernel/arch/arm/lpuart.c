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

#include <arch/arm/lpuart.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <arch/arm/arm.h>
#include <arch/arm/platform.h>
#include <serial.h>
#include <dev/lpuart_dev.h>

static lpuart_t uarts[LPUART_MAX_PORTS];

// Mask all interrupts in the IMSC register
#define INTERRUPTS_MASK		0

#define MSG(format, ...) printk( LOG_NOTE, "lpuart: "format, ## __VA_ARGS__ )

/**
 * \brief Configure the serial interface, from a caller that knows
 * that this is a bunch of PL011s, and furthermore where they are in
 * the physical address space.
 */
errval_t serial_early_init(unsigned n)
{
    assert(!paging_mmu_enabled());
    assert(n < serial_num_physical_ports);

    lpuart_initialize(&uarts[n],
        (mackerel_addr_t)local_phys_to_mem(platform_uart_base[n]));

    // Make sure that the UART is enabled and transmitting - not all platforms
    // do this for us.
    lpuart_baud_rawwr(&uarts[n], 0x402008b);
    lpuart_ctrl_te_wrf(&uarts[n], 1);
    lpuart_ctrl_re_wrf(&uarts[n], 1);

    return SYS_ERR_OK;
}

/**
 * \brief Configure the serial interface, from a caller that knows
 * that this is a bunch of PL011s, and furthermore where they are in
 * the physical address space.
 */
errval_t serial_early_init_mmu_enabled(unsigned n)
{
    assert(paging_mmu_enabled());
    assert(n < serial_num_physical_ports);

    lpuart_initialize(&uarts[n],
        (mackerel_addr_t)local_phys_to_mem(platform_uart_base[n]));

    // Make sure that the UART is enabled and transmitting - not all platforms
    // do this for us.
    lpuart_baud_rawwr(&uarts[n], 0x402008b);
    lpuart_ctrl_te_wrf(&uarts[n], 1);
    lpuart_ctrl_re_wrf(&uarts[n], 1);

    return SYS_ERR_OK;
}
/*
 * \brief Initialize a serial port.  The MMU is turned on.
 */
void lpuart_init(unsigned port, lvaddr_t base, bool hwinit)
{
    assert(paging_mmu_enabled());
    assert(port < serial_num_physical_ports);

    lpuart_t *u = &uarts[port];

    // [Re]initialize the Mackerel state for the UART
    lpuart_initialize(u, (mackerel_addr_t) base);

    if (hwinit) {
        lpuart_baud_rawwr(u, 0x402008b);
        lpuart_ctrl_t ctrl = (lpuart_ctrl_t)0;
        ctrl = lpuart_ctrl_te_insert(ctrl, 1);
        ctrl = lpuart_ctrl_re_insert(ctrl, 1);
        lpuart_ctrl_rawwr(u, ctrl);

        //TODO: Figure out more advanced configuration (FIFO etc.)
    }
}

/*
 * \brief Put a character to the port
 */
void serial_putchar(unsigned port, char c)
{
    assert(port < LPUART_MAX_PORTS);
    lpuart_t *u = &uarts[port];
    assert(u->base != 0);

    while(lpuart_stat_tdre_rdf(u) == 0);
    lpuart_data_buf_wrf(u, c);
}

/*
 * \brief Read a character from a port
 */
char serial_getchar(unsigned port)
{
    assert(port < LPUART_MAX_PORTS);
    lpuart_t *u = &uarts[port];
    assert(u->base != 0);

    /* Wait for data. */
    while(lpuart_stat_rdrf_rdf(u) == 0);

    return (char)lpuart_data_buf_rdf(u);
}
