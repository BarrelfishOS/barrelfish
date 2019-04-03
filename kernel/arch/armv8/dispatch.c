/*
 * Architecture-specific context switch
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <paging_kernel_arch.h>
#include <sysreg.h>

/**
 * \brief Switch context to 'dcb'.
 *
 * Switch to the dispatcher pointed to by 'dcb'. Sets 'dcb_current'.
 *
 * \param dcb        Pointer to dispatcher to which to switch context.
 */
void
context_switch(struct dcb *dcb) {
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(dcb->disp);

    assert(dcb != NULL);
    assert(dcb->vspace != 0);

    // VM guests do not have a user space dispatcher
    if (!dcb->is_vm_guest) {
        assert(dcb->disp != 0);
    }

    paging_context_switch(dcb->vspace);
    context_switch_counter++;

    if (!dcb->is_vm_guest) {
        assert(dcb->disp_cte.cap.type == ObjType_Frame);

        /* We set the read-only thread ID register (TPIDRRO_EL0) to the user's
         * pointer to the dispatcher shared area, so that code in
         * libbarrelfish can efficiently locate its dispatcher. The read-write
         * version (TPIDR_EL0) is free to implement TLS. */
        sysreg_write_tpidrro_el0((uint64_t)disp->udisp);

    }

    /* The EL1 thread ID register points to the same structure in the
     * kernel window, and is used by trap code.  This is inaccessible to
     * user-level code. */
    sysreg_write_tpidr_el1((uint64_t)disp);
}
