/**
 * \file plat_arm_vm_consts.c
 * \brief
 */


/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

/* RAM starts at 0, provided by the MMAP */
lpaddr_t phys_memory_start= 0;

/*
 * ----------------------------------------------------------------------------
 * GIC
 * ----------------------------------------------------------------------------
 */

// These values are from the APM88xx manual
//lpaddr_t platform_gic_cpu_interface_base  = 0x00000000781e2000;
//lpaddr_t platform_gic_distributor_base = 0x0000000078121000;

// These values are from linux devicetree file
lpaddr_t platform_gic_cpu_interface_base = 0x0000000078020000;
lpaddr_t platform_gic_distributor_base = 0x0000000078010000;

/*
 * ----------------------------------------------------------------------------
 * UART
 * ----------------------------------------------------------------------------
 */

/* the maximum number of UARTS supported */
#define MAX_NUM_UARTS 4

/* the serial console port */
unsigned int serial_console_port = 0;

/* the debug console port */
unsigned int serial_debug_port = 0;

/* the number of physical ports */
unsigned serial_num_physical_ports = 4;

/* uart bases */
const lpaddr_t
platform_uart_base[MAX_NUM_UARTS]= {
        0x1C020000, 0x1C021000, 0x1C02200, 0x1C023000
};

/* uart sizes */
const size_t
platform_uart_size[MAX_NUM_UARTS]= {
    4096, 4096, 4096, 4096
};
