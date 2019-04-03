/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef COREBOOT_H
#define COREBOOT_H

#include <assert.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/cpu.h>

typedef uintptr_t (*coreboot_start_fn_t)(hwid_t target, genpaddr_t entry, genpaddr_t context);

void coreboot_set_spawn_handler(enum cpu_type type, coreboot_start_fn_t handler);
coreboot_start_fn_t coreboot_get_spawn_handler(enum cpu_type);

#endif // COREBOOT_H
