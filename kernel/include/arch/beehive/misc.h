/* [2009-07-30 ohodson] TODO: implement! */

/**
 * \file
 * \brief Miscellaneous architecture-specific functions
 */

/*
 * Copyright (c) 2008, 2009, 2010 ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_MISC_H
#define ARCH_MISC_H

/**
 * \brief Set thread-local-storage register.
 */

/* 
 * The name of the function is somewhat misleading. we need an unused
 * user register that always stores the pointer to the current
 * dispatcher. most ABIs define a register for thread-local storage,
 * and we have been abusing that on x64 for the dispatcher pointer
 * --arch_set_thread_ register sets this pointer.  Obviously this
 * needs to change to support thread-local storage using a standard
 * ABI, so we will have to figure out how to get to the dispatcher
 * from something like a thread-local variable.  The reason that this
 * is in the switch path and not in resume/execute is that on x86_64
 * loading the thread register (fs) is stupidly expensive, so we avoid
 * doing it unless we switch contexts -- presumably that could be a
 * local optimisation in the x86_64 dispatch paths rather than the
 * generic context_switch path/
 */

static inline void arch_set_thread_register(uintptr_t val)
{
    extern uintptr_t x86_fs_compatability_register;
    x86_fs_compatability_register = val;
}

#endif /* ARCH_MISC_H */
