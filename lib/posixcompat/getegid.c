/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <unistd.h>

#include "posixcompat.h"

/**
 * \brief Get the effective group ID.
 */
gid_t getegid(void)
{
    assert(!"NYI");
    return 0;
}

/**
 * \brief Get the real group ID.
 */
gid_t getgid(void)
{
    assert(!"NYI");
    return 0;
}

/**
 * \brief Set the effective group ID.
 */
int setegid(gid_t gid)
{
    assert(!"NYI");
    return -1;
}

/**
 * \brief Set-group-ID.
 */
int setgid(gid_t gid)
{
    assert(!"NYI");
    return -1;
}
