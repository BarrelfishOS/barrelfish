/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"

struct intern {
    struct intermon_binding *closure;
    bool initialized;
};

extern struct intern intern[];
extern struct intern multicast_route[];
extern int multicast_route_coreid[];
extern uint64_t multicast_route_coremask[];
extern struct intern unicast_route[];
extern int unicast_route_coreid[];

static inline errval_t
intern_set(struct intermon_binding *closure, bool flag, uint8_t index)
{
    intern[index].closure = closure;
    intern[index].initialized = flag;

    return SYS_ERR_OK;
}

static inline errval_t
intern_set_initialize(uint8_t index, bool flag)
{
    if (index >= MAX_CPUS) {
        return MON_ERR_INVALID_CORE_ID;
    }

    intern[index].initialized = flag;
    return SYS_ERR_OK;
}

static inline errval_t
intern_get_initialize(uint8_t index, bool *flag)
{
    if (index >= MAX_CPUS) {
        return MON_ERR_INVALID_CORE_ID;
    }

    *flag = intern[index].initialized;
    return SYS_ERR_OK;
}

static inline errval_t
intern_get_closure(uint8_t index, struct intermon_binding** closure)
{
    errval_t err = SYS_ERR_OK;
    if (index >= MAX_CPUS) {
        *closure = NULL;
        err = MON_ERR_INVALID_CORE_ID;
        goto ret;
    }

    if (intern[index].closure == NULL) {
        *closure = NULL;
        err = MON_ERR_NO_MONITOR_FOR_CORE;
        goto ret;
    }

    *closure = intern[index].closure;
 ret:
    return err;
}

static inline errval_t
intern_get_core_id(struct intermon_binding* closure, uint8_t *core_id)
{
    for (int i = 0; i < MAX_CPUS; i++) {
        if (closure == intern[i].closure) {
            *core_id = i;
            return SYS_ERR_OK;
        }
    }

    return MON_ERR_INVALID_MON_ID;
}
