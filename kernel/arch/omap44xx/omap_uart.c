/**
 * \file
 * \brief Kernel serial driver for the OMAP44xx UARTs.  Implements the
 * interface found in /kernel/include/serial.h
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arm.h>
#include <paging_kernel_arch.h>

#include <serial.h>
#include <dev/omap/omap_uart_dev.h>
#include <omap44xx_map.h>

//
// Serial console and debugger interfaces
//
#define NUM_PORTS 4
unsigned int serial_console_port = 2;
unsigned int serial_debug_port = 2;
unsigned const int serial_num_physical_ports = NUM_PORTS;

static omap_uart_t ports[NUM_PORTS];

static lpaddr_t uart_base[NUM_PORTS] = {
    OMAP44XX_MAP_L4_PER_UART1,
    OMAP44XX_MAP_L4_PER_UART2,
    OMAP44XX_MAP_L4_PER_UART3,
    OMAP44XX_MAP_L4_PER_UART4
};

static size_t uart_size[NUM_PORTS] = {
    OMAP44XX_MAP_L4_PER_UART1_SIZE,
    OMAP44XX_MAP_L4_PER_UART2_SIZE,
    OMAP44XX_MAP_L4_PER_UART3_SIZE,
    OMAP44XX_MAP_L4_PER_UART4_SIZE
};

static bool uart_initialized[NUM_PORTS];

/*
 * Initialzie OMAP UART
 * UART TRM 23.3
 */
static void omap_uart_init(omap_uart_t *uart, lvaddr_t base)
{
    omap_uart_initialize(uart, (mackerel_addr_t) base);

    // Disable all interrupts
    omap_uart_IER_t ier = omap_uart_IER_default;
    omap_uart_IER_wr(uart, ier);

    // Enable FIFOs and select highest trigger levels
    omap_uart_FCR_t fcr = omap_uart_FCR_default;
    fcr = omap_uart_FCR_rx_fifo_trig_insert(fcr, 3);
    fcr = omap_uart_FCR_tx_fifo_trig_insert(fcr, 3);
    fcr = omap_uart_FCR_fifo_en_insert(fcr, 1);
    omap_uart_FCR_wr(uart, fcr);

    // Set line to 8N1
    omap_uart_LCR_t lcr = omap_uart_LCR_default;
    lcr = omap_uart_LCR_parity_en_insert(lcr, 0);       // No parity
    lcr = omap_uart_LCR_nb_stop_insert(lcr, 0);         // 1 stop bit
    lcr = omap_uart_LCR_char_length_insert(lcr, omap_uart_wl_8bits); // 8 data bits
    omap_uart_LCR_wr(uart, lcr);
}

errval_t serial_init(unsigned port)
{
    if (port >= NUM_PORTS) { 
        return SYS_ERR_SERIAL_PORT_INVALID;
    }
    if (uart_initialized[port]) {
	printf("omap serial_init[%d]: already initialized; skipping.\n", port);
	return SYS_ERR_OK;
    }
    
    lvaddr_t base = paging_map_device(uart_base[port],uart_size[port]);
    // paging_map_device returns an address pointing to the beginning of
    // a section, need to add the offset for within the section again
    uint32_t offset = (uart_base[port] & ARM_L1_SECTION_MASK);
    printf("omap serial_init[%d]: base = 0x%"PRIxLVADDR" 0x%"PRIxLVADDR"\n",
	   port, base, base + offset);
    omap_uart_init(&ports[port], base + offset);
    uart_initialized[port] = true;
    printf("omap serial_init[%d]: done.\n", port);
    return SYS_ERR_OK;
}

errval_t serial_early_init(unsigned port)
{
    if (port < NUM_PORTS) {
	// Bug to call twice on a port
        assert(ports[port].base == 0);
        omap_uart_init(&ports[port], uart_base[port]);
        return SYS_ERR_OK;
    } else {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }
}


/**
 * \brief Prints a single character to a serial port. 
 */
void serial_putchar(unsigned port, char c)
{
    assert(port <= NUM_PORTS);
    omap_uart_t *uart = &ports[port];

    // Wait until FIFO can hold more characters
    while(!omap_uart_LSR_tx_fifo_e_rdf(uart));

    // Write character
    omap_uart_THR_thr_wrf(uart, c);
}

/** 
 * \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char serial_getchar(unsigned port)
{
    assert(port <= NUM_PORTS);
    omap_uart_t *uart = &ports[port];

    // Wait until there is at least one character in the FIFO
    while(!omap_uart_LSR_rx_fifo_e_rdf(uart));

    // Return the character
    return omap_uart_RHR_rhr_rdf(uart);
}
