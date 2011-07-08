/**
 * \file
 * \brief Dispatcher architecture-specific code
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_BARRELFISH_CURDISPATCHER_H
#define ARCH_BEEHIVE_BARRELFISH_CURDISPATCHER_H

/**
 * \brief Returns pointer to current dispatcher, using thread register
 */
static inline dispatcher_handle_t curdispatcher(void)
{
    dispatcher_handle_t ret;

    // We keep the dispatcher pointer in the platform usage register.
    // This register is not used by the compiler.
    __asm("ld %0, p1" : "=r" (ret) : );

    return ret;
}

#endif // ARCH_BEEHIVE_BARRELFISH_CURDISPATCHER_H
