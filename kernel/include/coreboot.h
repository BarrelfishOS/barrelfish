/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef COREBOOT_H
#define COREBOOT_H

#include <assert.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/cpu.h>

typedef int(*start_core_fn)(coreid_t coreid, genvaddr_t entry);

void coreboot_set_spawn_handler(enum cpu_type type, start_core_fn handler);
start_core_fn coreboot_get_spawn_handler(enum cpu_type);

#endif // COREBOOT_H