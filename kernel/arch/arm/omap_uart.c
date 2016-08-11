/**
 * \file
 * \brief Kernel serial driver for the OMAP44xx UARTs.  
 */

/*
 * Copyright (c) 2012-2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <omap_uart.h>
#include <dev/omap/omap44xx_uart3_dev.h>
#include <kernel.h>
#include <arm.h>
#include <paging_kernel_arch.h>
#include <platform.h>
#include <serial.h>

//
// Serial console and debugger interfaces
//
#define NUM_PORTS 4
static omap44xx_uart3_t ports[NUM_PORTS];

static void omap_uart_hw_init(omap44xx_uart3_t *uart);

#define MSG(port, format, ...) \
    printk( LOG_NOTE, "OMAP serial[%d]: "format, port, ## __VA_ARGS__ )

/*
 * Initialize UARTs before the MMU is on.
 */
errval_t serial_early_init(unsigned port)
{
    assert(!paging_mmu_enabled());
    assert(port < serial_num_physical_ports);
    omap44xx_uart3_initialize(&ports[port], (mackerel_addr_t)uart_base[port]);
    omap_uart_hw_init(&ports[port]);
    return SYS_ERR_OK;
}

/*
 * Re-initialize UARTs after the MMU is on.
 */
void omap_uart_init(unsigned port, lvaddr_t base, bool initialize_hw)
{
    assert(paging_mmu_enabled());
    assert(port < serial_num_physical_ports);
    omap44xx_uart3_initialize(&ports[port], (mackerel_addr_t)base);
    if (initialize_hw) omap_uart_hw_init(&ports[port]);
}

static void
uart_reset(omap44xx_uart3_t *uart) {
    /* Do soft reset */
    omap44xx_uart3_sysc_softreset_wrf(uart, 1);
    while(!omap44xx_uart3_syss_resetdone_rdf(uart)); // Poll for completion
}

static void
init_fifos(omap44xx_uart3_t *uart) {
    /* Configure FIFOs and DMA. TRM S23.3.5.1.1.2. */
    omap44xx_uart3_lcr_t lcr_reset= omap44xx_uart3_lcr_rd(uart);

    /* Set configuration mode B. */
    omap44xx_uart3_lcr_wr(uart, (omap44xx_uart3_lcr_t)0x00BF); // Step 1(b)

    /* Enable register submode TCR_TLR (part 1). */
    int enhanced_en_reset= omap44xx_uart3_efr_enhanced_en_rdf(uart);
    omap44xx_uart3_efr_enhanced_en_wrf(uart, 1);

    /* Set configuration mode A. */
    omap44xx_uart3_lcr_wr(uart, (omap44xx_uart3_lcr_t)0x0080); // Step 3

    /* Enable register submode TCR_TLR (part 2). */
    int tcr_tlr_reset= omap44xx_uart3_mcr_tcr_tlr_rdf(uart);
    omap44xx_uart3_mcr_tcr_tlr_wrf(uart, 1);

    /* Disable FIFOs, and set send and receive triggers to one byte. */
    omap44xx_uart3_fcr_rx_fifo_trig_wrf(uart, 0x1); // Low 2 bits
    omap44xx_uart3_fcr_tx_fifo_trig_wrf(uart, 0x1);
    omap44xx_uart3_fcr_dma_mode_wrf(uart, 0);       // Ignored
    omap44xx_uart3_fcr_fifo_en_wrf(uart, 0);

    /* Reset Tx and Rx FIFOs. */
    omap44xx_uart3_fcr_tx_fifo_clear_wrf(uart, 1);
    omap44xx_uart3_fcr_rx_fifo_clear_wrf(uart, 1);

    /* Set configuration mode B. */
    omap44xx_uart3_lcr_wr(uart, (omap44xx_uart3_lcr_t)0x00BF); // Step 6

    /* Set the upper 4 bits of the trigger levels to zero. */
    omap44xx_uart3_tlr_rx_fifo_trig_dma_wrf(uart, 0x0);
    omap44xx_uart3_tlr_tx_fifo_trig_dma_wrf(uart, 0x0);

    /* Disable DMA, and set trigger levels using both tlr_rx_fifo_trig_dma
     * (high 4 bits), and fcr_rx_fifo_trig (low 2 bits).  */
    omap44xx_uart3_scr_rx_trig_granu1_wrf(uart, 1);
    omap44xx_uart3_scr_tx_trig_granu1_wrf(uart, 1);
    omap44xx_uart3_scr_dma_mode_2_wrf(uart, 0);
    omap44xx_uart3_scr_dma_mode_ctl_wrf(uart, 1);

    /* Restore enhanced_en. */
    omap44xx_uart3_efr_enhanced_en_wrf(uart, enhanced_en_reset);

    /* Set configuration mode A. */
    omap44xx_uart3_lcr_wr(uart, (omap44xx_uart3_lcr_t)0x0080); // Step 10

    /* Restore tcr_tlr. */
    omap44xx_uart3_mcr_tcr_tlr_wrf(uart, tcr_tlr_reset);

    /* Restore LCR. */
    omap44xx_uart3_lcr_wr(uart, lcr_reset);
}

static void
init_protocol(omap44xx_uart3_t *uart) {
    /* Configure protocol, baud rate and interrupts. TRM S13.3.5.1.1.4. */

    /* Disable UART, to access DLL and DLH. */
    omap44xx_uart3_mdr1_mode_select_wrf(uart, omap44xx_uart3_MODE_SELECT_7);

    /* Set configuration mode B. */
    omap44xx_uart3_lcr_wr(uart, (omap44xx_uart3_lcr_t)0x00BF);

    /* Enable access to IER[7:4] bit field. */
    int enhanced_en_reset= omap44xx_uart3_efr_enhanced_en_rdf(uart);
    omap44xx_uart3_efr_enhanced_en_wrf(uart, 1);

    /* Switch to register operational mode. */
    omap44xx_uart3_lcr_wr(uart, (omap44xx_uart3_lcr_t)0x0000); // Step 4

    /* Clear the interrupt enable register. */
    omap44xx_uart3_ier_wr(uart, (omap44xx_uart3_ier_t)0x0000); // Step 5

    /* Set configuration mode B. */
    omap44xx_uart3_lcr_wr(uart, (omap44xx_uart3_lcr_t)0x00BF);

    /* Set the Baud rate divisor for ~115200bps, from a 48MHz clock.
     * We'll set 16x mode, so 115385 ~ 48MHz / (16 * 26). */
    omap44xx_uart3_dlh_clock_msb_wrf(uart, 0);
    omap44xx_uart3_dll_clock_lsb_wrf(uart, 26);

    /* Switch to register operational mode. */
    omap44xx_uart3_lcr_wr(uart, (omap44xx_uart3_lcr_t)0x0000);

    /* Enable the receive interrupt. */
    omap44xx_uart3_ier_rhr_it_wrf(uart, 1);

    /* Set configuration mode B. */
    omap44xx_uart3_lcr_wr(uart, (omap44xx_uart3_lcr_t)0x00BF);

    /* Restore enhanced_en. */
    omap44xx_uart3_efr_enhanced_en_wrf(uart, enhanced_en_reset);

    /* Set protocol formatting (8n1). */
    omap44xx_uart3_lcr_div_en_wrf(uart, 0);
    omap44xx_uart3_lcr_break_en_wrf(uart, 0);
    omap44xx_uart3_lcr_parity_en_wrf(uart, 0);
    omap44xx_uart3_lcr_nb_stop_wrf(uart, omap44xx_uart3_NB_STOP_0);
    omap44xx_uart3_lcr_char_length_wrf(uart, omap44xx_uart3_cl8);

    /* Set UART to 16x mode. */
    omap44xx_uart3_mdr1_mode_select_wrf(uart, 0);
}

/*
 * Initialise OMAP UART hardware
 * UART TRM 23.3
 */
static void omap_uart_hw_init(omap44xx_uart3_t *uart)
{
    uart_reset(uart);
    init_fifos(uart);
    init_protocol(uart);
}

/**
 * \brief Prints a single character to a serial port. 
 */
void serial_putchar(unsigned port, char c)
{
    assert(port <= NUM_PORTS);
    omap44xx_uart3_t *uart = &ports[port];

    // Wait until FIFO can hold more characters
    while(!omap44xx_uart3_lsr_tx_fifo_e_rdf(uart));

    // Write character
    omap44xx_uart3_thr_thr_wrf(uart, c);
}

/** 
 * \brief Reads a single character from the default serial port.
 */
char serial_getchar(unsigned port)
{
    assert(port <= NUM_PORTS);
    omap44xx_uart3_t *uart = &ports[port];

    /* Read until the interrupt is deasserted. */
    char c= '\0';
    while(omap44xx_uart3_iir_it_pending_rdf(uart) == 0) {
        c= omap44xx_uart3_rhr_rhr_rdf(uart);
    }

    return c;
}
