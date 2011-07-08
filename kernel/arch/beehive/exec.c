/**
 * \file
 * \brief x86-64 execution and miscellany
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <gdb_stub.h>
#include <exec.h>
#include <stdio.h> // printf
#include <dispatch.h>
#include <barrelfish_kpi/registers_arch.h> // NUM_REGS
#include <trace/trace.h>
#include <simctrl.h>
#include <corearea.h>
#include <trace/trace.h>

#include "beekernel.h"
#include "bmp.h"

/**
 * \brief Triggers a debugger breakpoint.
 */
void breakpoint(void)
{
    gdb_stub_entry(5/*???*/, "breakpoint"); 
}

void halt(void)
{
    panic("halt: called");
}

void reboot(void)
{
    panic("reboot: called");
}

/*
 * On beehive we have SAS, so just check that this is null
 */
extern void paging_context_switch(void* ptbase);
void paging_context_switch(void* ptbase)
{
    assert(ptbase == (void*)0xBAD23200);
}


extern void __attribute__ ((noreturn)) _resume(union registers_beehive *regs);

extern void __attribute__ ((noreturn)) _execute(uint32_t entry, struct dispatcher_shared_generic *disp);


// XXX See kernel/arch/include/misc.h code for arch_set_thread_register
uintptr_t x86_fs_compatability_register;

/**
 * \brief Go to user-space at entry point 'entry'.
 *
 * This function goes to user-space and starts executing the program at
 * its entry point at virtual address 'entry'.
 *
 * \param entry Entry point address of program to execute.
 */

void __attribute__ ((noreturn)) execute(lvaddr_t entry)
{
    if (entry == 0)
	panic("execute: jumping at 0!\n\n");

    struct dispatcher_shared_generic *disp = (void*)x86_fs_compatability_register;
    if (disp == NULL)
	panic("execute: disp == NULL");

    _execute(entry, disp);
}

/**
 * \brief Resume the given user-space snapshot.
 *
 * This function resumes user-space execution by restoring the CPU
 * registers with the ones given in the array, pointed to by 'regs'.
 */
void __attribute__ ((noreturn)) resume(arch_registers_state_t *state)
{
    if (((uintptr_t)state & (sizeof(uintptr_t) -1)) != 0)
	panic("execute: state=%p\n", state);

    if (state->named.count > 64)
	panic("resume: invalid rq count (%u) indicated\n", state->named.count);
    if (state->named.pc == 0)
	panic("resume: jumping at 0!\n\n");
    if (state->named.p1 == 0) {
	if (x86_fs_compatability_register == 0)
	    panic("resume: no p1\n");
	else {
	    printf("fixing up missing thread register from x86 segment register\n");
	    state->named.p1 = x86_fs_compatability_register;
	}
    }
    _resume(state);
}

/**
 * \brief Halt processor until an interrupt arrives
 *
 * For use in the idle loop when nothing is runnable.
 */
void __attribute__ ((noreturn)) wait_for_interrupt(void)
{
#ifdef TRACE_CSWITCH
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_CSWITCH, 0);
#endif
    //int core = arch_get_core_id();
    //printf("Core %d yields\n", core);
    struct corearea *corearea = my_corearea();
    for(;;) {
	bmp_pump_until_timer(corearea);
	if (corearea->kernel_pending) {
	    printf("wfi: pending %u\n", corearea->kernel_pending);
	    /* XXX RACE */
	    kernel_now += corearea->kernel_pending;
	    corearea->kernel_pending = 0;
	    /* XXX END RACE */
	}
	struct dcb *next = schedule();
	if (next != NULL) {
	    //printf("Core %d dispatches %p\n", core, next);
	    dispatch(next);
	} else {
	    printf("Core %d still had nothing to do!\n", arch_get_core_id());
	}
    }
}


void __attribute__((noreturn)) handle_timer(
    union registers_beehive *saved, int how);

/* If the how indicates EXECUTE then the saved pointer is NULL */

void __attribute__((noreturn)) handle_timer(
    union registers_beehive *saved, int how)
{
    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_TIMER, kernel_now);

    struct corearea *corearea = my_corearea();
    struct dcb *dcb = dcb_current;

    assert(dcb != NULL);

    struct dispatcher_shared_beehive *disp;

    disp = get_dispatcher_shared_beehive(dcb_current->disp);
    assert(dcb->disabled == disp->d.disabled);
    if (how == HANDLE_TIMER_HOW_EXECUTE) {
	assert(saved == NULL);
	printf("handle_timer: core %u \"%.*s\" (%p,%p) at activation\n",
	       arch_get_core_id(),
	       DISP_NAME_LEN, disp->d.name, dcb, disp);
    } else {
	assert(saved == (dcb->disabled ? &disp->disabled_save_area : &disp->enabled_save_area));
	printf("handle_timer: core %u \"%.*s\" (%p,%p) at %#x %s how %u\n",
	       arch_get_core_id(),
	       DISP_NAME_LEN, disp->d.name, dcb, disp, saved->named.pc,
	       (dcb->disabled ? "disabled" : "enabled"), how);
    }

    if (corearea->kernel_pending > (how == HANDLE_TIMER_HOW_SYNC ? 0 : 1))
	printf("handle_timer: core %u *** pending = %u\n",
	       arch_get_core_id(), corearea->kernel_pending);

    /* XXX RACE */
    kernel_now += corearea->kernel_pending + (how == HANDLE_TIMER_HOW_SYNC ? 1 : 0);
    corearea->kernel_pending = 0;
    /* XXX END RACE */

    dispatch(schedule());
    panic("dispatch() returned");
}
