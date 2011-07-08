/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <lwip/sys.h>
#include <barrelfish/barrelfish.h>

#if SYS_LIGHTWEIGHT_PROT
static struct thread_mutex lock = THREAD_MUTEX_INITIALIZER;

sys_prot_t sys_arch_protect(void)
{
    thread_mutex_lock(&lock);
    return 0;
}

void sys_arch_unprotect(sys_prot_t pval)
{
    // this is always true (sys_prot_t is uint8_t!) -AB
    //if(pval >= 0) {
        thread_mutex_unlock(&lock);
    //}
}
#endif // SYS_LIGHTWEIGHT_PROT
