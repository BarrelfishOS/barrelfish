/* [2009-07-30 ohodson] TODO: implement! */

/**
 * \file
 * \brief Miscellaneous architecture-specific functions
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_MISC_H
#define ARCH_MISC_H

//
// Helpers for pasting #defined values into inline assembler.
//
#define STR(x) #x
#define XTR(x) STR(x)

#include <arch/armv7/cp15.h>

/**
 * \brief Set thread-local-storage register.
 */
static inline void arch_set_thread_register(uintptr_t value)
{
    cp15_write_tpidruro(value);
}

static inline uintptr_t arch_get_thread_register(void)
{
    return cp15_read_tpidruro();
}

#endif /* ARCH_MISC_H */
