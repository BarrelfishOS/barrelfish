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

/*
 * Return the addresses of the GIC Distributor and CPU blocks
 */
extern lpaddr_t platform_get_distributor_address(void);
extern lpaddr_t platform_get_gic_cpu_address(void);

/*
 * Initialize and enable the Snoop Control Unit
 */
extern void platform_init_scu(void);

/*
 * Return the core count
 */
extern size_t platform_get_core_count(void);

/*
 * Print system identification.   MMU is NOT yet enabled.
 */
extern void platform_print_id(void);

/*
 * Figure out how much RAM we have
 */
extern size_t platform_get_ram_size(void);

/*
 * Boot secondary processors
 */
extern int platform_boot_aps(coreid_t core_id, genvaddr_t gen_entry);
extern void platform_notify_bsp(void);


#endif // __ARM_PLATFORM_H__

