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
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(dcb->disp);

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

        fpu_lazy_bottom(dcb);

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
        arch_set_thread_register(disp->udisp);
    }
}
