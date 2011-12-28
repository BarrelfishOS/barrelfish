/**
 * \file
 * \brief Dispatcher architecture-specific implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/syscalls.h>
#include <threads.h>

/* entry points defined in assembler code */
extern void run_entry(void);
extern void pagefault_entry(void);
extern void disabled_pagefault_entry(void);
extern void trap_entry(void);

/* helper functions defined in assembler code */
extern void _disp_resume(union registers_beehive *regs,
			 uint32_t *disabledptr) __attribute__((noreturn));

// The assembler uses DISP_DISABLED from asmoffsets.c
// so make sure asmoffsets.c is also using dispatcher_shared_generic
extern void _disp_switch(struct dispatcher_shared_generic *disp, 
			 union registers_beehive *from_regs,
			 union registers_beehive *to_regs);

extern int _disp_save(union registers_beehive *regs) __attribute__((returns_twice));

#if 0
void __attribute__ ((visibility ("hidden"))) disp_resume_end(void);
#endif

/**
 * \brief Architecture-specific dispatcher initialisation
 */
void disp_arch_init(dispatcher_handle_t handle)
{
    struct dispatcher_shared_beehive *disp =
        get_dispatcher_shared_beehive(handle);

    disp->d.dispatcher_run = (lvaddr_t)run_entry;
    disp->d.dispatcher_pagefault = (lvaddr_t)pagefault_entry;
    disp->d.dispatcher_pagefault_disabled = (lvaddr_t)disabled_pagefault_entry;
    disp->d.dispatcher_trap = (lvaddr_t)trap_entry;

#if 0
    disp->crit_pc_low = (lvaddr_t)disp_resume;
    disp->crit_pc_high = (lvaddr_t)disp_resume_end;
#endif
}

/**
 * \brief Resume execution of a given register state
 *
 * This function resumes the execution of the given register state on the
 * current dispatcher. It may only be called while the dispatcher is disabled.
 *
 * \param disp Current dispatcher pointer
 * \param regs Register state snapshot
 */
void disp_resume(dispatcher_handle_t handle, arch_registers_state_t *archregs)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);

    assert_disabled(disp != NULL);
    assert_disabled(archregs != NULL);

    assert_disabled(curdispatcher() == handle);
    assert_disabled(disp->disabled);
    assert_disabled(disp->haswork);

    /* sanity check user state */
    assert_disabled(archregs->named.count <= 64);
    assert_disabled(archregs->named.p1 != 0);
    assert_disabled(archregs->named.stack != 0);
    assert_disabled(archregs->named.pc != 0);

#ifdef CONFIG_DEBUG_DEADLOCKS
    ((struct disp_priv *)disp)->yieldcount = 0;
#endif
    
    // The assembler does the rest, including atomically
    // clearing the disable flag with restoring the context.
    _disp_resume(archregs, &disp->disabled);
}


/**
 * \brief Switch execution between two register states
 *
 * This function saves as much as necessary of the current register state
 * (which, when resumed will return to the caller), and switches execution
 * by resuming the given register state.  It may only be called while the
 * dispatcher is disabled.
 *
 * \param disp Current dispatcher pointer
 * \param from_regs Location to save current register state
 * \param to_regs Location from which to resume new register state
 */
void disp_switch(dispatcher_handle_t handle, arch_registers_state_t *from_state,
                 arch_registers_state_t *to_state)
{
    assert_disabled(curdispatcher() == handle);
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled);
    assert_disabled(disp->haswork);

    //struct disp_priv *disp_priv = (struct disp_priv *)disp;
    assert_disabled(to_state != NULL);

    // The assembler does all the rest
    _disp_switch(disp, from_state, to_state);
}


/**
 * \brief Save the current register state and optionally yield the CPU
 *
 * This function saves as much as necessary of the current register state
 * (which, when resumed will return to the caller), and then either
 * re-enters the thread scheduler or yields the CPU.
 * It may only be called while the dispatcher is disabled.
 *
 * \param disp Current dispatcher pointer
 * \param regs Location to save current register state
 * \param yield If true, yield CPU to kernel; otherwise re-run thread scheduler
 * \param yield_to Endpoint capability for dispatcher to which we want to yield
 */
void disp_save(dispatcher_handle_t handle, arch_registers_state_t *state,
               bool yield, capaddr_t yield_to)
{
    assert_disabled(curdispatcher() == handle);
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    assert_disabled(disp->disabled);

    // Save resume IP, stack and control registers
    // See disp_switch above for details

    if (_disp_save(state) != 0) {
	return;
    }

    if (yield) {
        sys_yield(yield_to);
        // may fail if target doesn't exist; if so, just fall through
    }
    // this code won't run if the yield succeeded

    // enter thread scheduler again
    // this doesn't return, and will call disp_yield if there's nothing to do
    thread_run_disabled(handle);
#if 0
    __asm volatile ("save_resume:");
#endif
}
