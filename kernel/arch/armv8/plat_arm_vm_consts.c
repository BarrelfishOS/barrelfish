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
#include <arch/arm/platform.h>

/* RAM starts at 0, provided by the MMAP */
lpaddr_t phys_memory_start= 0;

/*
 * ----------------------------------------------------------------------------
 * GIC
 * ----------------------------------------------------------------------------
 */

lpaddr_t platform_gic_distributor_base = 0x2f000000;
lpaddr_t platform_gic_redistributor_base = 0x2f100000;

/*
 * ----------------------------------------------------------------------------
 * UART
 * ----------------------------------------------------------------------------
 */

/* the maximum number of UARTS supported */
#define MAX_NUM_UARTS 1

/* the serial console port */
unsigned int serial_console_port = 0;

/* the debug console port */
unsigned int serial_debug_port = 0;

/* the number of physical ports */
unsigned serial_num_physical_ports = 1;

/* uart bases */
lpaddr_t platform_uart_base[MAX_NUM_UARTS]= {
        0x9000000
};

/* uart sizes */
size_t platform_uart_size[MAX_NUM_UARTS]= {
    4096
};
