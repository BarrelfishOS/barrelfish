/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "vmkitmon.h"
#include <x86emu.h>
#include "realmode.h"
#ifdef CONFIG_SVM
#include "svm.h"
#endif
#include "x86.h"

static struct guest *env = NULL;
static bool valid_exit = false;

#ifndef CONFIG_SVM
// Global variables are used, since the corresponding fields are read-only 
// in the VMCS. 
uint16_t saved_exit_reason;
uint64_t saved_exit_qual, saved_rip;
#endif

#ifdef CONFIG_SVM
static inline void
set_vmcb_exit(amd_vmcb_t *vmcb, uint64_t code, uint64_t info1, uint64_t info2)
{
    amd_vmcb_exitcode_wr(vmcb, code);
    amd_vmcb_exitinfo1_wr(vmcb, info1);
    amd_vmcb_exitinfo2_wr(vmcb, info2);
}
#endif
/* real mode interface functions */

static uint8_t
io_inb (uint16_t port)
{
    assert(!"inb not implemented");
    return 0;
}

static uint16_t
io_inw (uint16_t port)
{
    assert(!"inw not implemented");
    return 0;
}

static uint32_t
io_inl (uint16_t port)
{
    assert(!"inl not implemented");
    return 0;
}

static void
io_outb (uint16_t port, uint8_t val)
{
#ifdef CONFIG_SVM
    uint32_t info1;

    info1 = X86_IO_ACCESS_SZ8 | X86_IO_ACCESS_A16;
    info1 |= port << 16;
    set_vmcb_exit(&env->vmcb, SVM_VMEXIT_IOIO, info1, M.x86.R_EIP);
#else
    saved_exit_qual = 0x1 | (port << 16);
    saved_rip = M.x86.R_EIP;
    saved_exit_reason = VMX_EXIT_REASON_INOUT;
#endif
    valid_exit = true;

    // move EIP back to the start of the instruction
    M.x86.R_EIP -= 2;

    HALT_SYS();
}

static void
io_outw (uint16_t port, uint16_t val)
{
    assert(!"outw not implemented");
}

static void
io_outl (uint16_t port, uint32_t val)
{
    assert(!"outl not implemented");
}

static void
int_handler (int num)
{
    // check whether the interrupt corresponds to an exception
    // in real-mode everything from 10 is not an ecxeption
    if (num < 10) {
        // exception raised
        // check whether this exception should be intercepted
#ifdef CONFIG_SVM
        if (amd_vmcb_exceptions_rd_raw(&env->vmcb) & (1 << num)) {
#else
	uint64_t excp_bmp;
	errval_t err = invoke_dispatcher_vmread(env->dcb_cap, VMX_EXCP_BMP, &excp_bmp);
	assert(err_is_ok(err));
	if (excp_bmp & (1 << num)) {
#endif
            assert(!"Intercepted exception raised");
        } else {
            assert(!"Realmode raised an exception which is not captured");
        }
    } else {
        // software interrupt raised
        // check whether we are interessted in SW interrupts
#ifdef CONFIG_SVM
        if (amd_vmcb_intercepts_rd(&env->vmcb).intn == 1) {
            set_vmcb_exit(&env->vmcb, SVM_VMEXIT_SWINT, 0, 0);
            valid_exit = true;
        } else {
            assert(!"SWINT occured but not intercepted by the VMM");
        }
#else
	saved_exit_reason = VMX_EXIT_REASON_SWINT;
	valid_exit = true;
#endif
    }

    // move EIP back to the start of the instruction
    M.x86.R_EIP -= 2;

    HALT_SYS();
}

/**
 * \brief Initializes this module.
 *
 * Needs to be called before any other call to realmode functionality.
 *
 * \return Zero on success, non-zero on failure
 */
errval_t
realmode_init (void)
{
    // initialize the io hooks
    X86EMU_pioFuncs io_hooks = {
        .inb = io_inb,
        .inw = io_inw,
        .inl = io_inl,
        .outb = io_outb,
        .outw = io_outw,
        .outl = io_outl
    };
    X86EMU_setupPioFuncs(&io_hooks);

    // initialize interrupt handers
    X86EMU_intrFuncs int_hooks[256];
    for (int i = 0; i < 256; i++) {
        int_hooks[i] = int_handler;
    }
    X86EMU_setupIntrFuncs(int_hooks);

    return SYS_ERR_OK;
}

void
realmode_switch_to (struct guest *g)
{
    assert(g != NULL);
    assert(env == NULL);

    env = g;

    // sanity check
    assert(g->mem_low_va == 0 && g->mem_high_va >= 0x100000);

    // copy the registers
#ifdef CONFIG_SVM
    M.x86.R_ESP = amd_vmcb_rsp_rd(&g->vmcb);
    M.x86.R_EIP = amd_vmcb_rip_rd(&g->vmcb);
    M.x86.R_EFLG = amd_vmcb_rflags_rd_raw(&g->vmcb);
    M.x86.R_EAX = amd_vmcb_rax_rd(&g->vmcb);
#else
    uint64_t guest_rsp;
    errval_t err = invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_RSP, &guest_rsp);
    M.x86.R_ESP = guest_rsp;
    
    uint64_t guest_rip;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_RIP, &guest_rip);
    M.x86.R_EIP = guest_rip;
    
    uint64_t guest_rflags;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_RFLAGS, &guest_rflags);
    M.x86.R_EFLG = guest_rflags;
    M.x86.R_EAX = g->ctrl->regs.rax;
#endif
    M.x86.R_EBX = g->ctrl->regs.rbx;
    M.x86.R_ECX = g->ctrl->regs.rcx;
    M.x86.R_EDX = g->ctrl->regs.rdx;

    M.x86.R_EBP = g->ctrl->regs.rbp;
    M.x86.R_ESI = g->ctrl->regs.rsi;
    M.x86.R_EDI = g->ctrl->regs.rdi;

    // calculate the segment selector from the supplied base because the stored
    // selector might not point to the correct RM segment
#ifdef CONFIG_SVM
    M.x86.R_CS = amd_vmcb_cs_base_rd(&g->vmcb) >> 4;
    M.x86.R_DS = amd_vmcb_ds_base_rd(&g->vmcb) >> 4;
    M.x86.R_ES = amd_vmcb_es_base_rd(&g->vmcb) >> 4;
    M.x86.R_FS = amd_vmcb_fs_base_rd(&g->vmcb) >> 4;
    M.x86.R_GS = amd_vmcb_gs_base_rd(&g->vmcb) >> 4;
    M.x86.R_SS = amd_vmcb_ss_base_rd(&g->vmcb) >> 4;
#else
    uint64_t guest_cs_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_CS_BASE, &guest_cs_base);
    M.x86.R_CS = guest_cs_base >> 4;
    
    uint64_t guest_ds_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_DS_BASE, &guest_ds_base);
    M.x86.R_DS = guest_ds_base >> 4;
    
    uint64_t guest_es_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_ES_BASE, &guest_es_base);
    M.x86.R_ES = guest_es_base >> 4;
    
    uint64_t guest_fs_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_FS_BASE, &guest_fs_base);
    M.x86.R_FS = guest_fs_base >> 4;
    
    uint64_t guest_gs_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_GS_BASE, &guest_gs_base);
    M.x86.R_GS = guest_gs_base >> 4;
    
    uint64_t guest_ss_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_SS_BASE, &guest_ss_base);
    M.x86.R_SS = guest_ss_base >> 4;

    assert(err_is_ok(err));
#endif
    // copy memory location
    M.mem_base = guest_to_host(g->mem_low_va);
    if (g->a20_gate_enabled) {
        // add 1024 byte at the end if the a20 gate is enabled
        M.mem_size = 0x100400;
    } else {
        // without a20 gate we are suppoed to have 1MB of memory
        M.mem_size = 0x100000;
    }
}

void
realmode_switch_from (struct guest *g)
{
    // save all state
#ifdef CONFIG_SVM
    amd_vmcb_rsp_wr(&g->vmcb, M.x86.R_ESP);
    amd_vmcb_rip_wr(&g->vmcb, M.x86.R_EIP);
    amd_vmcb_rflags_wr_raw(&g->vmcb, M.x86.R_EFLG);
    amd_vmcb_rax_wr(&g->vmcb, M.x86.R_EAX);
#else
    errval_t err = invoke_dispatcher_vmwrite(g->dcb_cap, VMX_GUEST_RSP, M.x86.R_ESP);
    err += invoke_dispatcher_vmwrite(g->dcb_cap, VMX_GUEST_RIP, M.x86.R_EIP);
    err += invoke_dispatcher_vmwrite(g->dcb_cap, VMX_GUEST_RFLAGS, M.x86.R_EFLG);
    g->ctrl->regs.rax = M.x86.R_EAX;
#endif
    g->ctrl->regs.rbx = M.x86.R_EBX;
    g->ctrl->regs.rcx = M.x86.R_ECX;
    g->ctrl->regs.rdx = M.x86.R_EDX;

    g->ctrl->regs.rbp = M.x86.R_EBP;
    g->ctrl->regs.rsi = M.x86.R_ESI;
    g->ctrl->regs.rdi = M.x86.R_EDI;

    // only copy the segments back if they were changed during execution
    // take the base as reference value because the selector might be invalid
    // (this happens e.g. in a switch from protected mode to real-mode, where
    // real-mode segment is read from the GDT)
    // FIXME: this is not 100% save: If the code changes the seg selectors to
    //        same value as the initial one then it wont be captured here
#ifdef CONFIG_SVM
    if ((amd_vmcb_cs_base_rd(&g->vmcb) >> 4) != M.x86.R_CS) {
        VMCB_WRITE_SEGREG_REALMODE(&g->vmcb, cs, M.x86.R_CS);
    }
    if ((amd_vmcb_ds_base_rd(&g->vmcb) >> 4) != M.x86.R_DS) {
        VMCB_WRITE_SEGREG_REALMODE(&g->vmcb, ds, M.x86.R_DS);
    }
    if ((amd_vmcb_es_base_rd(&g->vmcb) >> 4) != M.x86.R_ES) {
        VMCB_WRITE_SEGREG_REALMODE(&g->vmcb, es, M.x86.R_ES);
    }
    if ((amd_vmcb_fs_base_rd(&g->vmcb) >> 4) != M.x86.R_FS) {
        VMCB_WRITE_SEGREG_REALMODE(&g->vmcb, fs, M.x86.R_FS);
    }
    if ((amd_vmcb_gs_base_rd(&g->vmcb) >> 4) != M.x86.R_GS) {
        VMCB_WRITE_SEGREG_REALMODE(&g->vmcb, gs, M.x86.R_GS);
    }
    if ((amd_vmcb_ss_base_rd(&g->vmcb) >> 4) != M.x86.R_SS) {
        VMCB_WRITE_SEGREG_REALMODE(&g->vmcb, ss, M.x86.R_SS);
    }
#else 
    uint64_t guest_cs_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_CS_BASE, &guest_cs_base); 
    if ((guest_cs_base >> 4) != M.x86.R_CS) {
        VMCS_WRITE_SEGREG_REALMODE(g->dcb_cap, CS, M.x86.R_CS);
    }
    uint64_t guest_ds_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_DS_BASE, &guest_ds_base); 
    if ((guest_ds_base >> 4) != M.x86.R_DS) {
        VMCS_WRITE_SEGREG_REALMODE(g->dcb_cap, DS, M.x86.R_DS);
    }
    uint64_t guest_es_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_ES_BASE, &guest_es_base); 
    if ((guest_es_base >> 4) != M.x86.R_ES) {
        VMCS_WRITE_SEGREG_REALMODE(g->dcb_cap, ES, M.x86.R_ES);
    }
    uint64_t guest_fs_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_FS_BASE, &guest_fs_base); 
    if ((guest_fs_base >> 4) != M.x86.R_FS) {
        VMCS_WRITE_SEGREG_REALMODE(g->dcb_cap, FS, M.x86.R_FS);
    }
    uint64_t guest_gs_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_GS_BASE, &guest_gs_base); 
    if ((guest_gs_base >> 4) != M.x86.R_GS) {
        VMCS_WRITE_SEGREG_REALMODE(g->dcb_cap, GS, M.x86.R_GS);
    }
    uint64_t guest_ss_base;
    err += invoke_dispatcher_vmread(g->dcb_cap, VMX_GUEST_SS_BASE, &guest_ss_base); 
    if ((guest_ss_base >> 4) != M.x86.R_SS) {
        VMCS_WRITE_SEGREG_REALMODE(g->dcb_cap, SS, M.x86.R_SS);
    }
    assert(err_is_ok(err));
#endif
    env = NULL;
}

#ifndef CONFIG_SVM
// Return true if the "Descriptor-table exiting" Secondary Processor-based control 
// is set in the VMCS, else false.
static inline bool vmx_desc_table_exiting(void) {
    uint64_t sec_proc_ctrls;
    errval_t err = invoke_dispatcher_vmread(env->dcb_cap, VMX_EXEC_SEC_PROC, &sec_proc_ctrls);
    assert(err_is_ok(err));
    if (sec_proc_ctrls & SP_CLTS_DESC_TABLE) {
        uint64_t prim_proc_ctrls;
	err += invoke_dispatcher_vmread(env->dcb_cap, VMX_EXEC_PRIM_PROC, &prim_proc_ctrls);
	assert(err_is_ok(err));
	assert(prim_proc_ctrls & PP_CLTS_SEC_CTLS);
	return true;
    }
    return false;
}
#endif

int
realmode_exec (void)
{
    assert(env != NULL);

    valid_exit = false;

    // run the simulator
    X86EMU_exec ();

    // examine halt reason
    if (valid_exit) {
        return REALMODE_ERR_OK;
    }

    uint8_t *code = (uint8_t *)(M.mem_base + (M.x86.R_CS << 4) + M.x86.R_EIP);

    // check for two byte operation
    if (code[-2] == 0x0f) {
        union x86_modrm mod;

        // move EIP back to the start of the instruction
        M.x86.R_EIP -= 2;
        mod.raw = code[0];

        // check for LGDT
        if (code[-1] == 0x01 && mod.u.regop == 2) {
            // handle instruction
#ifdef CONFIG_SVM
            if (amd_vmcb_intercepts_rd(&env->vmcb).wrgdtr == 1) {
                set_vmcb_exit(&env->vmcb, SVM_VMEXIT_GDTR_WRITE, 0, 0);
#else
	    if (vmx_desc_table_exiting()) {
	        saved_exit_reason = VMX_EXIT_REASON_GDTR_IDTR;
#endif
                return REALMODE_ERR_OK;
            } else {
                assert(!"LGTR not intercepted");
            }
        }
        // check for LIDT
        if (code[-1] == 0x01 && mod.u.regop == 3) {
#ifdef CONFIG_SVM
            if (amd_vmcb_intercepts_rd(&env->vmcb).wridtr == 1) {
                set_vmcb_exit(&env->vmcb, SVM_VMEXIT_IDTR_WRITE, 0, 0);
#else
	    if (vmx_desc_table_exiting()) {
	        saved_exit_reason = VMX_EXIT_REASON_GDTR_IDTR;
#endif
                return REALMODE_ERR_OK;
            } else {
                assert(!"LITR not intercepted");
            }
        }
        // check for CR access
        else if (code[-1] == 0x20 || code[-1] == 0x22) {
            if (mod.u.regop != 0) {
                assert(!"realmode: only access to CR0 are allowed atm");
            }
#ifndef CONFIG_SVM
	    union x86_modrm mod2;
	    mod2.raw = code[2];
#endif
            if (code[-1] == 0x20) { // check for read
#ifdef CONFIG_SVM
                if (amd_vmcb_cr_access_rd(&env->vmcb).rdcr0 == 1) {
                    set_vmcb_exit(&env->vmcb, SVM_VMEXIT_CR0_READ, 0, 0);
                    return REALMODE_ERR_OK;
                }
#else
		saved_exit_qual = (0x1 << 4) | (mod.u.rm << 8);
		saved_exit_reason = VMX_EXIT_REASON_CR_ACCESS;
		return REALMODE_ERR_OK;
#endif
            } else {
#ifdef CONFIG_SVM
                if (amd_vmcb_cr_access_rd(&env->vmcb).wrcr0 == 1) {
                    set_vmcb_exit(&env->vmcb, SVM_VMEXIT_CR0_WRITE, 0, 0);
                    return REALMODE_ERR_OK;
                }
#else
		saved_exit_qual = mod.u.rm << 8;
		saved_exit_reason = VMX_EXIT_REASON_CR_ACCESS;

		return REALMODE_ERR_OK;
#endif
            }
            assert(!"CR0 access not intercepted");
        }
        // CPUID
        else if (code[-1] == 0xa2) {
#ifdef CONFIG_SVM
            set_vmcb_exit(&env->vmcb, SVM_VMEXIT_CPUID, 0, 0);
#else
	    saved_exit_reason = VMX_EXIT_REASON_CPUID;
#endif
            return REALMODE_ERR_OK;
        }
    }

    printf("EIP: %08x, code %02x %02x %02x %02x\n", M.x86.R_EIP - 2,
           code[-2], code[-1], code[0], code[1]);
    assert(!"Unknown exit condition");
    return REALMODE_ERR_OK;
}

/* libx86emu relies on this function */
void printk(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
}
