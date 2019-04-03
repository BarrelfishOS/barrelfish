/**
 * \file
 * \brief Dispatcher architecture-specific code
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_AARCH64_BARRELFISH_CURDISPATCHER_H
#define ARCH_AARCH64_BARRELFISH_CURDISPATCHER_H

#include <barrelfish_kpi/dispatcher_handle.h>

//
// Helpers for pasting #defined values into inline assembler.
//
#define STR(x) #x
#define XTR(x) STR(x)

/**
 * \brief Returns pointer to current dispatcher, using thread register
 */
static inline dispatcher_handle_t curdispatcher(void) {
    dispatcher_handle_t ret = 0;
    /* The CPU driver maintains the user-space pointer to the shared
     * dispatcher structure in the read-only thread ID register. */
    __asm("mrs %[ret], tpidrro_el0" : [ret] "=r" (ret));
    return ret;
}

#endif // ARCH_AARCH64_BARRELFISH_CURDISPATCHER_H
