/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DOM_INVOCATIONS_H
#define DOM_INVOCATIONS_H

#include <barrelfish/invocations_arch.h>
#include "domcap.h"

static inline errval_t dom_cnode_get_state(struct domcapref cap, distcap_state_t *ret)
{
    return invoke_cnode_get_state(cap.croot, cap.cptr, cap.bits, ret);
}
static inline errval_t dom_cnode_delete(struct domcapref cap)
{
    return invoke_cnode_delete(cap.croot, cap.cptr, cap.bits);
}

#endif
