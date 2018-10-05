/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "intel_vtd.h"
#include "intel_vtd_commands.h"
#include "intel_vtd_interrupts.h"


errval_t vtd_interrupt_remapping_init(struct vtd *vtd)
{
    debug_printf("XXXX: INTERRUPT NOT YET IMPLEMENTED.\n");
    return SYS_ERR_OK;
}