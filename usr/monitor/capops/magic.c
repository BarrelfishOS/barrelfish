/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "magic.h"
#include <barrelfish/debug.h>

// delete all local copies of capref
errval_t monitor_delete_copies(struct capref cap)
{
    USER_PANIC("NYI");
    return ERR_NOTIMP;
}

// mark cap as in-delete
errval_t monitor_set_cap_deleted(struct capref cap)
{
    USER_PANIC("NYI");
    return ERR_NOTIMP;
}

// return SYS_ERR_OK if descendants exists, CAP_ERR_NOTFOUND otherwise
errval_t monitor_has_local_descendants(struct capability *cap)
{
    USER_PANIC("NYI");
    return ERR_NOTIMP;
}

