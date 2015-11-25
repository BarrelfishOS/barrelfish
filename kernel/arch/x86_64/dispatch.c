/*
 * Architecture-specific context switch
 */

/*
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <paging_kernel_arch.h>

/**
 * \brief Switch context to 'dcb'.
 *
 * Switch to the dispatcher pointed to by 'dcb'. Sets 'dcb_current'.
 *
 * \param dcb        Pointer to dispatcher to which to switch context.
 */
void
context_switch(struct dcb *dcb) {
    assert(dcb != NULL);
    assert(dcb->vspace != 0);

    // VM guests do not have a user space dispatcher
    if (!dcb->is_vm_guest) {
        assert(dcb->disp != 0);
    }

    fpu_lazy_top(dcb);

    paging_context_switch(dcb->vspace);
    context_switch_counter++;

    if (!dcb->is_vm_guest) {
        assert(dcb->disp_cte.cap.type == ObjType_Frame);

        maybe_reload_ldt(dcb, false);
    }
}
