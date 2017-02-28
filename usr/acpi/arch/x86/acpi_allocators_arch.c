/**
 * \file acpi_generic.c
 * \brief
 */


/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/capabilities.h>
#include <if/monitor_blocking_defs.h>

#include "acpi_debug.h"
#include "acpi_shared.h"
#include "acpi_allocators.h"

errval_t acpi_allocators_init_arch(struct bootinfo *bootinfo)
{
    errval_t err;

    struct monitor_blocking_binding *cl = get_monitor_blocking_binding();
    assert(cl != NULL);

    // Request I/O Cap
    struct capref requested_caps;
    errval_t error_code;
    err = slot_alloc(&requested_caps);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "slot_alloc for monitor->get_io_cap");
    }
    err = cl->rpc_tx_vtbl.get_io_cap(cl, &requested_caps, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));
    // Copy into correct slot
    struct capref caps_io = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_IO
    };
    return cap_copy(caps_io, requested_caps);
}
