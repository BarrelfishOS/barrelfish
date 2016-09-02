/**
 * \file
 * \brief Platform interface for ARMv7-A boards.
 *
 * This file defines the hardware abstraction layer for ARM targets. Each
 * board is expected to have an implementation that corresponds to this
 * interface.
 *
 * This interface is expected to change as new boards are added.
 */

/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARM_PLATFORM_H__
#define __ARM_PLATFORM_H__

#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/platform.h>

/*
 * Return the addresses of the GIC blocks.
 */
lpaddr_t platform_get_distributor_address(void);
lpaddr_t platform_get_gic_cpu_address(void);

/*
 * Return the base address of the private peripheral region.
 */
lpaddr_t platform_get_private_region(void);

/*
 * Do any extra initialisation for this particular CPU (e.g. A9/A15).
 */
void platform_revision_init(void);

/*
 * Return the core count
 */
size_t platform_get_core_count(void);

/*
 * Print system identification. MMU is NOT yet enabled.
 */
void platform_print_id(void);

/*
 * Fill out provided `struct platform_info`
 */
void platform_get_info(struct platform_info *pi);
void armv7_get_info(struct arch_info_armv7 *ai);

/*
 * Figure out how much RAM we have
 */
size_t platform_get_ram_size(void);

/*
 * Boot secondary processors
 */
int platform_boot_aps(coreid_t core_id, genvaddr_t gen_entry);
void platform_notify_bsp(uint32_t *mailbox);

/*
 * Timers
 */
void     timers_init(int timeslice);
uint64_t timestamp_read(void);
uint32_t timestamp_freq(void);
bool     timer_interrupt(uint32_t irq);

/*
 * UART locations
 */
extern const lpaddr_t uart_base[];
extern const size_t uart_size[];

#endif // __ARM_PLATFORM_H__
