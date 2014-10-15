/**
 * \file
 * \brief Registering for handler functions to manage cores
 */
/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <coreboot.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/cpu.h>

static start_core_fn spawn_core_handlers[CPU_TYPE_NUM];

/**
 * Register spawn core handler function for specific cpu type.
 * 
 * \param type CPU type
 * \param handler Handler functions
 */
void coreboot_set_spawn_handler(enum cpu_type type, start_core_fn handler) 
{
    assert(type < CPU_TYPE_NUM);
    assert(handler != NULL);
    assert(spawn_core_handlers[type] == NULL);

    if (type < CPU_TYPE_NUM) {
        spawn_core_handlers[type] = handler;
    }
}

/**
 * \param  cpu_type Get handler for specific cpu type
 * \return Core boot handler function or NULL in case none was registered
 * for that type
 */
start_core_fn coreboot_get_spawn_handler(enum cpu_type type) {
    assert(type < CPU_TYPE_NUM);
    
    if (type >= CPU_TYPE_NUM) {
        return NULL;
    }
    return spawn_core_handlers[type];
}