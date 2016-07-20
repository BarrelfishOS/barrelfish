/**
 * \file
 * \brief Platform code for ARMv7-A VersatileExpress EMM board
 */

/*
 * Copyright (c) 2009-2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <vexpress_map.h>

/* XXX - initialise this from the MMAP. */
lpaddr_t phys_memory_start= GEN_ADDR(31);

#define NUM_UARTS 5
unsigned serial_console_port = 0;
unsigned serial_debug_port = 0;
unsigned serial_num_physical_ports = NUM_UARTS;

const lpaddr_t uart_base[] = { 
    VEXPRESS_MAP_UART0, 
    VEXPRESS_MAP_UART1, 
    VEXPRESS_MAP_UART2, 
    VEXPRESS_MAP_UART3, 
    VEXPRESS_MAP_UART4
};

const size_t uart_size[] = { 
    VEXPRESS_MAP_UART0_SIZE, 
    VEXPRESS_MAP_UART1_SIZE, 
    VEXPRESS_MAP_UART2_SIZE, 
    VEXPRESS_MAP_UART3_SIZE, 
    VEXPRESS_MAP_UART4_SIZE
};
