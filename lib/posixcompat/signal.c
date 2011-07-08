/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <signal.h>
#include <barrelfish/barrelfish.h>

signalhandler_t signal(int signum, signalhandler_t handler)
{
    USER_PANIC("signal() NYI");
    return SIG_ERR;
}
