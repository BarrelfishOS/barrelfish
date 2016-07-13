/**
 * \file
 * \brief Platform code for the Xilinx Zynq7000-series SoCs
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <zynq7_map.h>

/* RAM starts at 0 on the Zynq */
/* XXX - MMAP */
lpaddr_t phys_memory_start= 0;

#define NUM_UARTS 2
unsigned int serial_console_port = 1;
unsigned int serial_debug_port = 1;
unsigned int serial_num_physical_ports = NUM_UARTS;

const lpaddr_t
uart_base[NUM_UARTS]= {
    ZINQ7_UART0_BASEADDR,
    ZINQ7_UART1_BASEADDR
};

const size_t
uart_size[NUM_UARTS]= {
    (1<<12),
    (1<<12)
};
