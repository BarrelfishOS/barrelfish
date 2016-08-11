/**
 * \file
 * \brief Platform code for the Cortex-A9 processors on TI OMAP44xx SoCs.
 */

/*
 * Copyright (c) 2009-2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <omap44xx_map.h>

/* RAM starts at 2G (2 ** 31) on the Pandaboard */
/* XXX - MMAP */
lpaddr_t phys_memory_start= GEN_ADDR(31);

#define NUM_UARTS 4
unsigned int serial_console_port = 2;
unsigned int serial_debug_port = 2;
unsigned int serial_num_physical_ports = NUM_UARTS;

const lpaddr_t uart_base[NUM_UARTS] = {
    OMAP44XX_MAP_L4_PER_UART1,
    OMAP44XX_MAP_L4_PER_UART2,
    OMAP44XX_MAP_L4_PER_UART3,
    OMAP44XX_MAP_L4_PER_UART4
};

const size_t uart_size[NUM_UARTS] = {
    OMAP44XX_MAP_L4_PER_UART1_SIZE,
    OMAP44XX_MAP_L4_PER_UART2_SIZE,
    OMAP44XX_MAP_L4_PER_UART3_SIZE,
    OMAP44XX_MAP_L4_PER_UART4_SIZE
};
