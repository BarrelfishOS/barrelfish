/*
 * Copyright (c) 2009-2013 ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <systime.h>
#include <arm_hal.h>
#include <sysreg.h>
#include <exceptions.h>
#include <exec.h>
#include <misc.h>
#include <stdio.h>
#include <wakeup.h>
#include <irq.h>
#include <arch/arm/arm.h>
#include <arch/arm/gic.h>
#include <arch/arm/platform.h>
#include <dev/armv8_dev.h>

void handle_user_page_fault(lvaddr_t                fault_address,
                            arch_registers_state_t* save_area,
                            union registers_aarch64 *resume_area)
{
    lvaddr_t handler;
    struct dispatcher_shared_aarch64 *disp =
        get_dispatcher_shared_aarch64(dcb_current->disp);
    uintptr_t saved_pc = save_area->named.pc;

    disp->d.disabled = dispatcher_is_disabled_ip(dcb_current->disp, saved_pc);
    bool disabled = (disp->d.disabled != 0);

    assert(dcb_current->disp_cte.cap.type == ObjType_Frame);

    printk(LOG_DEBUG, "user page fault%s in '%.*s': addr %"PRIxLVADDR
                      " IP %"PRIxPTR"\n",
           disabled ? " WHILE DISABLED" : "", DISP_NAME_LEN,
           disp->d.name, fault_address, saved_pc);

    if (disabled) {
        assert(save_area == &disp->trap_save_area);
        handler = disp->d.dispatcher_pagefault_disabled;
        dcb_current->faults_taken++;
    }
    else {
        assert(save_area == &disp->enabled_save_area);
        handler = disp->d.dispatcher_pagefault;
    }

    if (dcb_current->faults_taken > 2) {
        printk(LOG_WARN, "handle_user_page_fault: too many faults, "
               "making domain unrunnable\n");
        dcb_current->faults_taken = 0; // just in case it gets restarted
        scheduler_remove(dcb_current);
        dispatch(schedule());
    }
    else {
        //
        // Upcall to dispatcher
        //
        // NB System might be cleaner with a prototype
        // dispatch context that has R0-R3 to be overwritten
        // plus initial stack, thread, and gic registers. Could do
        // a faster resume_for_upcall().
        //

        struct dispatcher_shared_generic *disp_gen =
            get_dispatcher_shared_generic(dcb_current->disp);

        /* XXX - This code leaks the contents of the kernel stack to the
         * user-level fault handler. */

        resume_area->named.x0   = disp_gen->udisp;
        resume_area->named.x1   = fault_address;
        resume_area->named.x2   = armv8_ESR_EL1_rd(NULL);
        resume_area->named.x3   = saved_pc;
        /* Why does the kernel do this? */
        resume_area->named.x10  = disp->got_base;
        resume_area->named.pc   = handler;
        resume_area->named.spsr = CPSR_F_MASK | AARCH64_MODE_USR;

        // SP is set by handler routine.

        // Upcall user to save area
        disp->d.disabled = true;
    }
}

void handle_user_undef(lvaddr_t fault_address, enum aarch64_exception_class cause,
                       arch_registers_state_t* save_area,
                       union registers_aarch64 *resume_area)
{
    struct dispatcher_shared_aarch64 *disp =
        get_dispatcher_shared_aarch64(dcb_current->disp);

    bool disabled =
        dispatcher_is_disabled_ip(dcb_current->disp, save_area->named.pc);
    disp->d.disabled = disabled;

    assert(dcb_current->disp_cte.cap.type == ObjType_Frame);
    if (disabled) {
        //        assert(save_area == &disp->trap_save_area);
    }
    else {
        assert(save_area == &disp->enabled_save_area);
    }

    printk(LOG_WARN, "user undef fault (0x%lx)%s in '%.*s': IP 0x%lx x29:%lx x30:%lx sp:%lx\n",
           cause, disabled ? " WHILE DISABLED" : "", DISP_NAME_LEN,
           disp->d.name, fault_address, save_area->named.x29, save_area->named.x30, save_area->named.stack);

    struct dispatcher_shared_generic *disp_gen =
        get_dispatcher_shared_generic(dcb_current->disp);

    resume_area->named.x0   = disp_gen->udisp;
    resume_area->named.x1   = AARCH64_EVECTOR_UNDEF;
    resume_area->named.x2   = 0;
    resume_area->named.x3   = fault_address;
    /* Why does the kernel do this? */
    resume_area->named.x10  = disp->got_base;
    resume_area->named.pc   = disp->d.dispatcher_trap;
    resume_area->named.spsr = CPSR_F_MASK | AARCH64_MODE_USR;

    // Upcall user to save area
    disp->d.disabled = true;
}

void handle_user_fault(lvaddr_t fault_address, uintptr_t cause,
                       arch_registers_state_t* save_area)
{
    union registers_aarch64 resume_area;

    switch(cause) {
        case aarch64_ec_unknown :
        case aarch64_ec_wfi :
        case aarch64_ec_mcr_cp15 :
        case aarch64_ec_mcrr_cp15 :
        case aarch64_ec_mcr_cp14 :
        case aarch64_ec_ldc_cp14 :
        case aarch64_ec_fpen :
        case aarch64_ec_mcr_cp10 :
        case aarch64_ec_mcrr_cp14 :
        case aarch64_ec_il :
            handle_user_undef(fault_address, cause, save_area, &resume_area);
            break;
        case aarch64_ec_svc_aa32 :
        case aarch64_ec_hvc_aa32 :
        case aarch64_ec_smc_aa32 :
        case aarch64_ec_svc_aa64 :
        case aarch64_ec_hvc_aa64 :
        case aarch64_ec_smc_aa64 :
            panic("syscall ended up in exception handler ? Yuck.");
            break;
        case aarch64_ec_mrs :
        case aarch64_ec_impl :
            handle_user_undef(fault_address, cause, save_area, &resume_area);
            break;
        case aarch64_ec_iabt_low  :
            handle_user_page_fault(fault_address, save_area, &resume_area);
            break;
        case aarch64_ec_iabt_high :
            panic("pagefault while in kernel? Yuck.");
            break;
        case aarch64_ec_pc_align :
            handle_user_undef(fault_address, cause, save_area, &resume_area);
            break;
        case aarch64_ec_dabt_low :
            handle_user_page_fault(fault_address, save_area, &resume_area);
            break;
        case aarch64_ec_dabt_high :
            panic("pagefault while in kernel? Yuck.");
            break;
        case aarch64_ec_sp_align :
        case aarch64_ec_fpu_aa32 :
        case aarch64_ec_fpu_aa64 :
        case aarch64_ec_serror :
        case aarch64_ec_bkpt_low :
        case aarch64_ec_bkpt_high :
        case aarch64_ec_step_low :
        case aarch64_ec_step_high :
        case aarch64_ec_wpt_low :
        case aarch64_ec_wpt_high :
        case aarch64_ec_bkpt_soft :
        case aarch64_ec_bkpt_el2 :
        case aarch64_ec_brk :
            handle_user_undef(fault_address, cause, save_area, &resume_area);
            break;
        default:
            panic("Unknown exception syndrome: %u", cause);
        break;
    }

    resume(&resume_area);
}

void nosave_handle_irq(void) 
{
    uint32_t irq = 0;
    irq = platform_get_active_irq();

    debug(SUBSYS_DISPATCH, "IRQ %"PRIu32" while %s\n", irq,
      dcb_current ? (dcb_current->disabled ? "disabled": "enabled") :
                    "in kernel");

    static int first_timer_interrupt_fired = 0;
    // Offer it to the timer
    if (platform_is_timer_interrupt(irq)) {
        if(!first_timer_interrupt_fired) {
            printk(LOG_NOTE, "ARMv8-A: Timer interrupt received!\n");
            first_timer_interrupt_fired = 1;
        }
        platform_acknowledge_irq(irq);
#ifndef CONFIG_ONESHOT_TIMER
        // Set next trigger
        systime_set_timer(kernel_timeslice);
#endif
        wakeup_check(systime_now());
        dispatch(schedule());
    } else {
        platform_acknowledge_irq(irq);
        send_user_interrupt(irq);
        panic("Unhandled IRQ %"PRIu32"\n", irq);
    }
}

void save_handle_irq(arch_registers_state_t* save_area, uintptr_t fault_pc,
                uint64_t x0, uint64_t x1, uint64_t x2, uint64_t x3)
{

    /* Save the FPU registers */
    __asm volatile(
        "   stp q0, q1, [%x0, #0]\n\t"
        "   stp q2, q3, [%x0, #0x20]\n\t"
        "   stp q4, q5, [%x0, #0x40]\n\t"
        "   stp q6, q7, [%x0, #0x60]\n\t"
        "   stp q8, q9, [%x0, #0x80]\n\t"
        "   stp q10, q11, [%x0, #0xa0]\n\t"
        "   stp q12, q13, [%x0, #0xc0]\n\t"
        "   stp q14, q15, [%x0, #0xe0]\n\t"
        "   stp q16, q17, [%x0, #0x100]\n\t"
        "   stp q18, q19, [%x0, #0x120]\n\t"
        "   stp q20, q21, [%x0, #0x140]\n\t"
        "   stp q22, q23, [%x0, #0x160]\n\t"
        "   stp q24, q25, [%x0, #0x180]\n\t"
        "   stp q26, q27, [%x0, #0x1a0]\n\t"
        "   stp q28, q29, [%x0, #0x1c0]\n\t"
        "   stp q30, q31, [%x0, #0x1e0]\n\t"
         :: "r" (&save_area->named.v));

    /* The assembly stub leaves the first 4 registers, the stack pointer,
     * the exception PC, and the SPSR for us to save, as it's run out of room for
     * the necessary instructions. */
    save_area->named.x0    = x0;
    save_area->named.x1    = x1;
    save_area->named.x2    = x2;
    save_area->named.x3    = x3;
    save_area->named.stack = armv8_SP_EL0_rd(NULL);
    save_area->named.spsr  = armv8_SPSR_EL1_rd(NULL);
    save_area->named.pc    = fault_pc;

    if (dcb_current != NULL) {
        dispatcher_handle_t handle = dcb_current->disp;
        if (save_area == dispatcher_get_disabled_save_area(handle)) {
            assert(dispatcher_is_disabled_ip(handle, fault_pc));
            dcb_current->disabled = true;
        } else {
            assert(save_area == dispatcher_get_enabled_save_area(handle));
            assert(!dispatcher_is_disabled_ip(handle, fault_pc));
            dcb_current->disabled = false;
        }
    }

    nosave_handle_irq();
}

#define STACK_DUMP_LIMIT 32

/* For unhandled faults, we print a register dump and panic. */
void fatal_kernel_fault(lvaddr_t epc, uint64_t spsr, uint64_t esr,
                        uint64_t vector, arch_registers_state_t* save_area)
{
    size_t i;
    enum aarch64_exception_class exception_class = FIELD(26,6,esr);
    /* int instruction_length = FIELD(25,1,esr); */
    int iss                = FIELD(0,25,esr);

    /* Save the FPU registers */
    __asm volatile(
        "   stp q0, q1, [%x0, #0]\n\t"
        "   stp q2, q3, [%x0, #0x20]\n\t"
        "   stp q4, q5, [%x0, #0x40]\n\t"
        "   stp q6, q7, [%x0, #0x60]\n\t"
        "   stp q8, q9, [%x0, #0x80]\n\t"
        "   stp q10, q11, [%x0, #0xa0]\n\t"
        "   stp q12, q13, [%x0, #0xc0]\n\t"
        "   stp q14, q15, [%x0, #0xe0]\n\t"
        "   stp q16, q17, [%x0, #0x100]\n\t"
        "   stp q18, q19, [%x0, #0x120]\n\t"
        "   stp q20, q21, [%x0, #0x140]\n\t"
        "   stp q22, q23, [%x0, #0x160]\n\t"
        "   stp q24, q25, [%x0, #0x180]\n\t"
        "   stp q26, q27, [%x0, #0x1a0]\n\t"
        "   stp q28, q29, [%x0, #0x1c0]\n\t"
        "   stp q30, q31, [%x0, #0x1e0]\n\t"
         :: "r" (&save_area->named.v));

    printk(LOG_PANIC, "Fatal (unexpected) fault at 0x%"PRIx64 " (%#" PRIx64 ")\n\n", epc, epc - (uintptr_t)&kernel_first_byte);
    printk(LOG_PANIC, "Register context saved at: %p\n", save_area);
    printk(LOG_PANIC, "Vector: ");
    switch(vector) {
        case AARCH64_EVECTOR_UNDEF:
            printk(LOG_PANIC, "UNDEF\n");
            break;
        case AARCH64_EVECTOR_EL0_SYNC:
            printk(LOG_PANIC, "EL0_SYNC\n");
            break;
        case AARCH64_EVECTOR_EL0_IRQ:
            printk(LOG_PANIC, "EL0_IRQ\n");
            break;
        case AARCH64_EVECTOR_EL0_FIQ:
            printk(LOG_PANIC, "EL0_FIQ\n");
            break;
        case AARCH64_EVECTOR_EL0_SERROR:
            printk(LOG_PANIC, "EL0_SERROR\n");
            break;
        case AARCH64_EVECTOR_EL1_SYNC:
            printk(LOG_PANIC, "EL1_SYNC\n");
            break;
        case AARCH64_EVECTOR_EL1_IRQ:
            printk(LOG_PANIC, "EL1_IRQ\n");
            break;
        case AARCH64_EVECTOR_EL1_FIQ:
            printk(LOG_PANIC, "EL1_FIQ\n");
            break;
        case AARCH64_EVECTOR_EL1_SERROR:
            printk(LOG_PANIC, "EL1_SERROR\n");
            break;
        case AARCH64_EVECTOR_EL2_SYNC:
            printk(LOG_PANIC, "EL2_SYNC\n");
            break;
        case AARCH64_EVECTOR_EL2_IRQ:
            printk(LOG_PANIC, "EL2_IRQ\n");
            break;
        case AARCH64_EVECTOR_EL2_FIQ:
            printk(LOG_PANIC, "EL2_FIQ\n");
            break;
        case AARCH64_EVECTOR_EL2_SERROR:
            printk(LOG_PANIC, "EL2_SERROR\n");
            break;
        case AARCH32_EVECTOR_EL0_SYNC:
            printk(LOG_PANIC, "AARCH32_EL0_SYNC\n");
            break;
        case AARCH32_EVECTOR_EL0_IRQ:
            printk(LOG_PANIC, "AARCH32_EL0_IRQ\n");
            break;
        case AARCH32_EVECTOR_EL0_FIQ:
            printk(LOG_PANIC, "AARCH32_EL0_FIQ\n");
            break;
        case AARCH32_EVECTOR_EL0_SERROR:
            printk(LOG_PANIC, "AARCH32_EL0_SERROR\n");
            break;
    }

    for (i = 0; i < 31; i++) {
        uint64_t reg = save_area->regs[i];
        if (reg >= (uintptr_t)&kernel_first_byte && reg <= (uintptr_t)&kernel_text_final_byte) {
            printk(LOG_PANIC, "x%d\t%"PRIx64" (%#" PRIx64 ")\n", i, reg, reg - (uintptr_t)&kernel_first_byte);
        } else {
            printk(LOG_PANIC, "x%d\t%"PRIx64"\n", i, reg);
        }
    }

    printk(LOG_PANIC, "sp\t%"PRIx64"\n", save_area->regs[SP_REG]);
    printk(LOG_PANIC, "pc\t%"PRIx64"\n", epc);
    printk(LOG_PANIC, "spsr\t%"PRIx64"\n", spsr);
    printk(LOG_PANIC, "instruction-specific syndrome\t%x\n", iss);

    /* Skip the trap frame to dump the prior stack. */
    uint64_t *kstack_base= (void *)save_area + (NUM_REGS * 8);

    if((((uintptr_t)kstack_base) & MASK(3)) != 0) {
        kstack_base= (uint64_t *)((uint64_t)kstack_base & ~MASK(3));
        printk(LOG_PANIC,
               "Kernel stack is misaligned, dumping from %p\n",
               kstack_base);
    }

    uint64_t kstack_len =
        (((uint64_t)kernel_stack + KERNEL_STACK_SIZE) -
         (uint64_t)kstack_base) /
        sizeof(uint64_t);

    printk(LOG_PANIC,
           "Kernel stack (0x%p - 0x%p):\n",
           kstack_base,
           (void *)kernel_stack + KERNEL_STACK_SIZE);

    for(i= 0; i < kstack_len-2; i+=2) {
        if(i > STACK_DUMP_LIMIT) {
            printk(LOG_PANIC, "...\n");
            break;
        }

        printk(LOG_PANIC,
               "%016"PRIx64"  %016"PRIx64"  %016"PRIx64"\n",
               (uint64_t)(kstack_base + i),
               kstack_base[i],
               kstack_base[i+1]);
    }

    switch(exception_class) {
        case aarch64_ec_unknown:
            panic("Unknown reason/instruction.\n");

        case aarch64_ec_wfi:
            panic("Trapped WFI/WFI.\n");

        case aarch64_ec_mcr_cp15:
        case aarch64_ec_mcrr_cp15:
            panic("CP15 abort.\n");

        case aarch64_ec_mcr_cp14:
        case aarch64_ec_ldc_cp14:
        case aarch64_ec_mcrr_cp14:
            panic("CP14 abort.\n");

        case aarch64_ec_fpen:
        case aarch64_ec_fpu_aa32:
        case aarch64_ec_fpu_aa64:
            panic("FPU abort.\n");

        case aarch64_ec_mcr_cp10:
            panic("CP10 abort.\n");

        case aarch64_ec_il:
            panic("PSTATE.IL == 1.\n");

        case aarch64_ec_svc_aa32:
        case aarch64_ec_hvc_aa32:
        case aarch64_ec_svc_aa64:
        case aarch64_ec_hvc_aa64:
        case aarch64_ec_smc_aa64:
            panic("Unhandled system/hypervisor/monitor call.\n");

        case aarch64_ec_mrs:
            panic("Exception caused by MSR/MRS.\n");

        case aarch64_ec_impl:
            panic("Implementation-specific exception.\n");

        case aarch64_ec_iabt_low:
            panic("Instruction abort at user level.\n");

        case aarch64_ec_iabt_high:
            panic("Instruction abort in the kernel.\n");

        case aarch64_ec_pc_align:
            panic("Misaligned PC @0x%"PRIx64".\n",
                  sysreg_read_far());

        case aarch64_ec_dabt_low:
            panic("Data abort at user level @0x%"PRIx64".\n",
                  sysreg_read_far());

        case aarch64_ec_dabt_high:
            printk(LOG_PANIC,
                   "Data abort in the kernel @0x%"PRIx64".\n",
                   sysreg_read_far());
            printk(LOG_PANIC, "Abort type: ");
            switch(iss) {
                case aarch64_dsfc_size_l0:
                    printk(LOG_PANIC, "address size fault, L0/TTBR\n");
                    break;
                case aarch64_dsfc_size_l1:
                    printk(LOG_PANIC, "address size fault, L1\n");
                    break;
                case aarch64_dsfc_size_l2:
                    printk(LOG_PANIC, "address size fault, L2\n");
                    break;
                case aarch64_dsfc_size_l3:
                    printk(LOG_PANIC, "address size fault, L3\n");
                    break;
                case aarch64_dsfc_trans_l0:
                    printk(LOG_PANIC, "translation fault, L0/TTBR\n");
                    break;
                case aarch64_dsfc_trans_l1:
                    printk(LOG_PANIC, "translation fault, L1\n");
                    break;
                case aarch64_dsfc_trans_l2:
                    printk(LOG_PANIC, "translation fault, L2\n");
                    break;
                case aarch64_dsfc_trans_l3:
                    printk(LOG_PANIC, "translation fault, L3\n");
                    break;
                case aarch64_dsfc_flag_l1:
                    printk(LOG_PANIC, "access flag fault, L1\n");
                    break;
                case aarch64_dsfc_flag_l2:
                    printk(LOG_PANIC, "access flag fault, L2\n");
                    break;
                case aarch64_dsfc_flag_l3:
                    printk(LOG_PANIC, "access flag fault, L3\n");
                    break;
                case aarch64_dsfc_perm_l1:
                    printk(LOG_PANIC, "permission fault, L1\n");
                    break;
                case aarch64_dsfc_perm_l2:
                    printk(LOG_PANIC, "permission fault, L2\n");
                    break;
                case aarch64_dsfc_perm_l3:
                    printk(LOG_PANIC, "permission fault, L3\n");
                    break;
                case aarch64_dsfc_external:
                    printk(LOG_PANIC, "external abort\n");
                    break;
                case aarch64_dsfc_external_l0:
                    printk(LOG_PANIC, "external abort on walk, L0/TTBR\n");
                    break;
                case aarch64_dsfc_external_l1:
                    printk(LOG_PANIC, "external abort on walk, L1\n");
                    break;
                case aarch64_dsfc_external_l2:
                    printk(LOG_PANIC, "external abort on walk, L2\n");
                    break;
                case aarch64_dsfc_external_l3:
                    printk(LOG_PANIC, "external abort on walk, L3\n");
                    break;
                case aarch64_dsfc_parity:
                    printk(LOG_PANIC, "parity error\n");
                    break;
                case aarch64_dsfc_parity_l0:
                    printk(LOG_PANIC, "parity error on walk, L0/TTBR\n");
                    break;
                case aarch64_dsfc_parity_l1:
                    printk(LOG_PANIC, "parity error on walk, L1\n");
                    break;
                case aarch64_dsfc_parity_l2:
                    printk(LOG_PANIC, "parity error on walk, L2\n");
                    break;
                case aarch64_dsfc_parity_l3:
                    printk(LOG_PANIC, "parity error on walk, L3\n");
                    break;
                case aarch64_dsfc_alighment:
                    printk(LOG_PANIC, "alignment fault\n");
                    break;
                case aarch64_dsfc_tlb_confl:
                    printk(LOG_PANIC, "TLB conflict\n");
                    break;
                case aarch64_dsfc_impl1:
                    printk(LOG_PANIC, "implementation-defined fault 1\n");
                    break;
                case aarch64_dsfc_impl2:
                    printk(LOG_PANIC, "implementation-defined fault 2\n");
                    break;
                case aarch64_dsfc_sect_dom:
                    printk(LOG_PANIC, "domain fault on section\n");
                    break;
                case aarch64_dsfc_page_dom:
                    printk(LOG_PANIC, "domain fault on page\n");
                    break;
                default:
                    printk(LOG_PANIC, "unknown\n");
                    break;
            }
            panic("halting.\n");

        case aarch64_ec_sp_align:
            panic("Misaligned SP.\n");

        case aarch64_ec_serror:
            panic("Delayed memory abort.\n");

        case aarch64_ec_bkpt_low:
            panic("HW Breakpoint in user code.\n");

        case aarch64_ec_bkpt_high:
            panic("HW Breakpoint in the kernel.\n");

        case aarch64_ec_step_low:
            panic("Single step in user code.\n");

        case aarch64_ec_step_high:
            panic("Single step in the kernel.\n");

        case aarch64_ec_wpt_low:
            panic("HW Watchpoint in user code @0x%"PRIx64".\n",
                  sysreg_read_far());

        case aarch64_ec_wpt_high:
            panic("HW Watchpoint in the kernel @0x%"PRIx64".\n",
                  sysreg_read_far());

        case aarch64_ec_bkpt_soft:
            panic("AArch32 soft breakpoint.\n");

        case aarch64_ec_bkpt_el2:
            panic("AArch32 Breakpoint trapped to EL2.\n");

        case aarch64_ec_brk:
            panic("AArch64 soft breakpoint.\n");

        default:
            panic("Unrecognised exception.\n");
    }
}
