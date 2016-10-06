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

lpaddr_t platform_gic_cpu_base = 0x8000000;
lpaddr_t platform_gic_dist_base = 0x8010000;

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
const lpaddr_t
uart_base[MAX_NUM_UARTS]= {
        0x9000000
};

/* uart sizes */
const size_t
uart_size[MAX_NUM_UARTS]= {
    4096
};
