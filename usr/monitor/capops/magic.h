/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

// XXX: prototypes and other decls that need to be moved to suitable locations
#ifndef CAPOPS_MAGIC_H
#define CAPOPS_MAGIC_H

#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/distcaps.h>
#include <barrelfish/caddr.h>
#include <errors/errno.h>

/*
 * Magic NYI functions
 */

// delete all local copies of capref
errval_t monitor_delete_copies(struct capref);

#endif
