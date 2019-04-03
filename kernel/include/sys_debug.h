/**
 * \file
 * \brief Arch-generic system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_SYS_DEBUG_H
#define KERNEL_SYS_DEBUG_H

#include <kernel.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/dispatcher_shared_target.h>
#include <barrelfish_kpi/types.h>
#include <capabilities.h>

/*
 * System calls for debug output.
 */
struct sysret
sys_debug_print_capabilities(void);

errval_t
debug_print_cababilities(struct dcb *dispatcher);

errval_t irq_debug_create_src_cap(uint8_t dcn_level, capaddr_t dcn,
        capaddr_t out_cap_addr, uint64_t start, uint64_t end);

/* fetch info about mapping database */
errval_t debug_get_mdb_size(size_t *retval);

/* print mapping database operation counters */
errval_t debug_print_mdb_counters(void);

#endif
