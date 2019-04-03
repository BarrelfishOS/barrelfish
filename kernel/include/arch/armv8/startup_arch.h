/**
 * \file
 * \brief Startup prototypes.
 */

/*
 * Copyright (c) 2007-2010, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _STARTUP_ARCH_H_
#define _STARTUP_ARCH_H_

#include <startup.h>
#include <offsets.h>


#define INIT_PERM_RO          (VMSAv8_64_L3_CACHEABLE  | \
                               VMSAv8_64_L3_BUFFERABLE | \
                               VMSAv8_64_L3_USR_RO)

#define INIT_PERM_RW          (VMSAv8_64_L3_CACHEABLE  | \
                               VMSAv8_64_L3_BUFFERABLE | \
                               VMSAv8_64_L3_USR_RW)

#define INIT_L0_SIZE          ARMv8_L0_ENTRIES(ARMV8_INIT_SPACE_LIMIT)
#define INIT_L1_SIZE          ARMv8_L1_ENTRIES(ARMV8_INIT_SPACE_LIMIT)
#define INIT_L2_SIZE          ARMv8_L2_ENTRIES(ARMV8_INIT_SPACE_LIMIT)
#define INIT_L3_SIZE          ARMv8_L2_ENTRIES(ARMV8_INIT_SPACE_LIMIT)

void create_module_caps(struct spawn_state *st);

struct dcb *spawn_bsp_init(const char *name);

struct dcb *spawn_app_init(struct armv8_core_data *core_data, const char *name);

extern struct armv8_core_data *armv8_glbl_core_data;

#endif
