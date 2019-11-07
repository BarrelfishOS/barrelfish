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

static void lpuart_hw_init(lpuart_t *uart);

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
    lpuart_hw_init(&uarts[n]);

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
    lpuart_hw_init(&uarts[n]);

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
        lpuart_hw_init(u);
    }
}

static void lpuart_hw_init(lpuart_t *uart)
{
    // Disable transceiver
    lpuart_ctrl_t ctrl = lpuart_ctrl_rawrd(uart);
    ctrl = lpuart_ctrl_te_insert(ctrl, 0);
    ctrl = lpuart_ctrl_re_insert(ctrl, 0);
    lpuart_ctrl_rawwr(uart, ctrl);

    //TODO: Figure out more advanced configuration (FIFO etc.)

    // Set baudrate
    // baudrate = clock rate / (over sampling rate * SBR)
    // TODO: Currently we assume UART clock is set to 8MHz
    lpuart_baud_t baud = lpuart_baud_default;
    baud = lpuart_baud_osr_insert(baud, lpuart_ratio5);
    // OSR of 5 needs bothedge set
    baud = lpuart_baud_bothedge_insert(baud, 1);
    baud = lpuart_baud_sbr_insert(baud, 139);
    lpuart_baud_rawwr(uart, baud);

    // Enable transceiver
    ctrl = lpuart_ctrl_default;
    ctrl = lpuart_ctrl_te_insert(ctrl, 1);
    ctrl = lpuart_ctrl_re_insert(ctrl, 1);
    lpuart_ctrl_rawwr(uart, ctrl);
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
    lpuart_txdata_wr(u,c);
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

    return (char)lpuart_rxdata_buf_rdf(u);
}
