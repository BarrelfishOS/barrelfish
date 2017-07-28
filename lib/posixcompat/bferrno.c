/*
 * Copyright (c) 2017 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <errors/errno.h>

#include <posixcompat.h>

#include "posixcompat.h"

static errval_t bf_errno = SYS_ERR_OK;

errval_t posixcompat_get_bf_error(void)
{
    return bf_errno;
}

void posixcompat_set_bf_error(errval_t err)
{
    bf_errno = err;
}
