/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_RAM_ALLOC_H
#define MONITOR_RAM_ALLOC_H

#include <barrelfish/types.h>
#include <if/intermon_defs.h>

errval_t mon_ram_alloc_init(coreid_t core_id, struct intermon_binding *b);
errval_t mon_ram_alloc_serve(void);
errval_t mon_ram_free(struct capability *cap_raw, genpaddr_t base, uint8_t bits);

#endif
