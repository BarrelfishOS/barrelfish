/**
 * \file
 * \brief Contains VMKit kernel interface for version using VMX extensions.
 */

/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#include <string.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <vmx_vmkit.h>
#include <vmx_checks.h>
#include <x86.h>
#include <dispatch.h>
#include <exec.h>
#include <irq.h>
#include <barrelfish_kpi/vmkit.h>
#include <barrelfish_kpi/syscalls.h>

#include <dev/ia32_dev.h>

// Execution, entry, and exit controls that we want to use 
// for each VM
#ifdef CONFIG_ARRAKISMON
#define GUEST_PIN_BASE_CTLS_ENABLE \
    (PIN_CTLS_EXT_INTR | PIN_CTLS_NMI | PIN_CTLS_VIRT_NMI)

#define GUEST_PIN_BASE_CTLS_DISABLE \
    (0)

#define GUEST_PP_CTLS_ENABLE \
    (PP_CLTS_MSRBMP | PP_CLTS_IOBMP | PP_CLTS_HLT)

#define GUEST_PP_CTLS_DISABLE \
    (0)

#define GUEST_SP_CTLS_ENABLE \
    (0)
     
#define GUEST_SP_CTLS_DISABLE \
    (0)

#define GUEST_EXIT_CTLS_ENABLE \
    (EXIT_CLTS_HOST_SIZE | EXIT_CLTS_SAVE_EFER | EXIT_CLTS_LOAD_EFER)

#define GUEST_EXIT_CTLS_DISABLE \
    (0)

#define GUEST_ENTRY_CTLS_ENABLE \
    (ENTRY_CLTS_LOAD_EFER | ENTRY_CLTS_LOAD_DBG | ENTRY_CLTS_IA32E_MODE)

#define GUEST_ENTRY_CTLS_DISABLE \
    (0)
#else
#define GUEST_PIN_BASE_CTLS_ENABLE \
    (PIN_CTLS_EXT_INTR | PIN_CTLS_NMI | PIN_CTLS_VIRT_NMI)

#define GUEST_PIN_BASE_CTLS_DISABLE \
    (0)

#define GUEST_PP_CTLS_ENABLE \
    (PP_CLTS_MSRBMP | PP_CLTS_IOBMP | PP_CLTS_HLT | PP_CLTS_SEC_CTLS)

#define GUEST_PP_CTLS_DISABLE \
    (0)

#define GUEST_SP_CTLS_ENABLE \
    (SP_CLTS_ENABLE_EPT | SP_CLTS_UNRSTD_GUEST)
     
#define GUEST_SP_CTLS_DISABLE \
    (0)

#define GUEST_EXIT_CTLS_ENABLE \
    (EXIT_CLTS_HOST_SIZE | EXIT_CLTS_SAVE_EFER | EXIT_CLTS_LOAD_EFER | \
     EXIT_CLTS_SAVE_PAT  | EXIT_CLTS_LOAD_PAT)

#define GUEST_EXIT_CTLS_DISABLE \
    (0)

#define GUEST_ENTRY_CTLS_ENABLE \
    (ENTRY_CLTS_LOAD_EFER)

#define GUEST_ENTRY_CTLS_DISABLE \
    (0)
#endif

extern void *vmx_return_func;

static struct guest_control *ctrl = NULL;

static int launched = 0;

#ifndef CONFIG_ARRAKISMON
// List of MSRs that are loaded on VM-exit.
static uint32_t msr_list[VMX_MSR_COUNT] = 
    {MSR_KERNEL_GS_BASE, MSR_STAR, MSR_LSTAR, MSR_CSTAR, MSR_SFMASK};

// VM-exit MSR-load area that contains host MSR values that are saved prior
// to VM-entry and loaded on VM exit.  
static struct msr_entry host_msr_area[VMX_MSR_COUNT]
__attribute__ ((aligned(16)));
#endif

// VMX controls that are written to the VMCS. In addition to the controls 
// that are requested, these values may have bits that are reserved set.
vmx_controls pin_based_ctls = 0, pp_based_ctls = 0, sp_based_ctls = 0,
    entry_ctls = 0, exit_ctls = 0; 

static uint8_t vmxon_region[BASE_PAGE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

// Returns true if extended page tables (EPT) are enabled.
static inline int ept_enabled(void)
{
    return ((GUEST_SP_CTLS_ENABLE & SP_CLTS_ENABLE_EPT) != 0);
}

static inline errval_t instr_err(void)
{
    errval_t err;
    __asm volatile("jnc vmx_err_check_zf%=\n\t"
		   "mov %[VMfailInvalid], %[err]\n\t"
		   "jmp vmx_err_done%=\n\t"
		   "vmx_err_check_zf%=:\n\t"
		   "jnz vmx_err_succeed%=\n\t"
		   "mov %[VMfailValid], %[err]\n\t"
		   "jmp vmx_err_done%=\n\t"
		   "vmx_err_succeed%=:\n\t"
		   "mov %[VMsucceed], %[err]\n\t"
		   "vmx_err_done%=:\n\t"
		   : [err] "=r" (err)
		   : [VMfailInvalid] "i" (SYS_ERR_VMKIT_VMX_VMFAIL_INVALID),
		     [VMfailValid] "i" (SYS_ERR_VMKIT_VMX_VMFAIL_VALID),
		     [VMsucceed] "i" (SYS_ERR_OK)
		   : "memory");
    return err;
}

// Executes the vmptrld instruction, which makes the VMCS referenced by
// 'vmcs_base' active and current.
errval_t vmptrld(lpaddr_t vmcs_base)
{
    __asm volatile("vmptrld %[vmcs_base]\n\t"
		   :
		   : [vmcs_base] "m" (vmcs_base)
		   : "memory");
    return instr_err();
}

// Returns the physical address base of the current VMCS.
lpaddr_t vmptrst(void)
{
   lpaddr_t dest_addr;
    __asm volatile("vmptrst %[dest_addr]\n\t"
		   :
		   : [dest_addr] "m" (dest_addr)
		   : "memory");
    return dest_addr;
}

// Executes the vmclear instruction, which makes the VMCS referenced 
// by 'vmcs_base' clear and inactive. This instruction also ensures 
// that the referenced VMCS data is saved.
errval_t vmclear(lpaddr_t vmcs_base)
{
    __asm volatile("vmclear %[vmcs_base]\n\t"
		   :
		   : [vmcs_base] "m" (vmcs_base)
		   : "memory");
    return instr_err();
}

// Reads a component with a specified encoding from the current VMCS
// to an address dest_addr using the vmread instruction.
errval_t vmread(uintptr_t encoding, lvaddr_t *dest_addr)
{
    __asm volatile("vmread %[encoding], %[dest_addr]\n\t"
		   :
		   : [encoding] "r" (encoding), [dest_addr] "m" (*dest_addr)
		   : "memory");
    return instr_err();
}

// Writes a component with a specifed encoding and value to the current
// VMCS using the vmwrite instruction.
errval_t vmwrite(uintptr_t encoding, uintptr_t value)
{
    __asm volatile("vmwrite %[value], %[encoding]\n\t"
		   :
		   : [encoding] "r" (encoding), [value] "r" (value)
		   : "memory");
    return instr_err();
}

// Using a provided VMXON region, causes the logical processor to enter 
// into root-mode by executing the vmxon instruction.
errval_t vmxon(lpaddr_t base_addr) 
{
    __asm volatile("vmxon %[base_addr]\n\t"
		   :
		   : [base_addr] "m" (base_addr)
		   : "memory");
    return instr_err();
}

// Exits VMX operation by executing the vmxoff instruction.
errval_t vmxoff(void)
{
    __asm volatile("vmxoff");
    return instr_err();
}

// Reads and returns the MSR that reports the allowed settings
// for ALL of the bits of the controls indicated by 'type.'
static uint64_t msr_ctls_true(enum vmx_ctls_t type)
{
    uint64_t true_msr = 0;
    switch(type) {
    case VMX_CTLS_PIN_BASED: 
        true_msr = ia32_vmx_true_pinbased_ctls_rd(NULL);
	break;
    case VMX_CTLS_PRIMARY_PROCESSOR: 
        true_msr = ia32_vmx_true_ppbased_ctls_rd(NULL);
	break;
    case VMX_CTLS_SECONDARY_PROCESSOR: 
        assert(!"No such MSR for secondary processor controls!\n");
	break;
    case VMX_CTLS_EXIT: 
        true_msr = ia32_vmx_true_exit_ctls_rd(NULL);
	break;
    case VMX_CTLS_ENTRY: 
        true_msr = ia32_vmx_true_entry_ctls_rd(NULL);
	break;
    }
    return true_msr;
}

// Reads and returns the MSR that reports the allowed settings
// for MOST of the bits of the controls indicated by 'type.'
static uint64_t msr_ctls(enum vmx_ctls_t type)
{
    uint64_t msr = 0;
    switch(type) {
    case VMX_CTLS_PIN_BASED: 
        msr = ia32_vmx_pinbased_ctls_rd(NULL);
	break;
    case VMX_CTLS_PRIMARY_PROCESSOR: 
        msr = ia32_vmx_ppbased_ctls_rd(NULL);
	break;
    case VMX_CTLS_SECONDARY_PROCESSOR: 
        msr = ia32_vmx_spbased_ctls_rd(NULL);
	break;
    case VMX_CTLS_EXIT: 
        msr = ia32_vmx_exit_ctls_rd(NULL);
	break;
    case VMX_CTLS_ENTRY: 
        msr = ia32_vmx_entry_ctls_rd(NULL);
	break;
    }
    return msr;
}

// Writes the controls indicated by 'type' to the VMCS using 'mask_1s'
// and 'mask_0s', which correspond to the controls that should be enabled 
// and disabled, respectively.  
static uint32_t set_vmx_controls(uint32_t mask_1s, 
    uint32_t mask_0s, enum vmx_ctls_t type)
{
    uint32_t controls = 0;
    
    ia32_vmx_basic_t vmx_basic = ia32_vmx_basic_rd(NULL);
    bool true_ctls = !!(ia32_vmx_basic_ctls_clear_extract(vmx_basic));   
    if (true_ctls && (type != VMX_CTLS_SECONDARY_PROCESSOR)) {
        uint64_t true_msr = msr_ctls_true(type);
	controls = ((DWORD_LS(true_msr) | mask_1s) & DWORD_MS(true_msr));
    } else {
        uint64_t msr = msr_ctls(type);
	controls = ((DWORD_LS(msr) | mask_1s) & DWORD_MS(msr));
    }
    assert((mask_1s & (~controls)) == 0);
    assert((mask_0s & controls) == 0);
    return controls;
}

/**
 * \brief Tries to enable hardware assisted virtualization.
 *
 * Checks whether hardware assisted virtualization is available on the platform
 * and enables this feature.
 *
 * \Return Returns VMKIT_ERR_OK on successful initialization of the subsystem
 *         or VMKIT_ERR_UNAVAIL if virtualization is unavailable.
 */
errval_t vmx_enable_virtualization (void)
{
    uint32_t cpuid_ecx;
    cpuid(CPUID_VMX, NULL, NULL, &cpuid_ecx, NULL);
    if (!(cpuid_ecx & VMX_SUPPORT)) {
        return SYS_ERR_VMKIT_UNAVAIL;
    }

    // The 'lock' and 'enable VMXON outside' bits of the IA32_FEATURE_CONTROL_MSR 
    // must be set
    ia32_feature_cntl_t feat_cntl_msr;
    feat_cntl_msr = ia32_feature_cntl_rd(NULL);
    if (!ia32_feature_cntl_lock_extract(feat_cntl_msr) || 
	!ia32_feature_cntl_vmxoutsmx_extract(feat_cntl_msr)) {
        return SYS_ERR_VMKIT_UNAVAIL;
    }

    pin_based_ctls = set_vmx_controls(
        GUEST_PIN_BASE_CTLS_ENABLE, GUEST_PIN_BASE_CTLS_DISABLE, VMX_CTLS_PIN_BASED);

    pp_based_ctls = set_vmx_controls(
	GUEST_PP_CTLS_ENABLE, GUEST_PP_CTLS_DISABLE, VMX_CTLS_PRIMARY_PROCESSOR);
	  
    sp_based_ctls = set_vmx_controls(
	GUEST_SP_CTLS_ENABLE, GUEST_SP_CTLS_DISABLE, VMX_CTLS_SECONDARY_PROCESSOR);

    entry_ctls = set_vmx_controls(
        GUEST_ENTRY_CTLS_ENABLE, GUEST_ENTRY_CTLS_DISABLE, VMX_CTLS_ENTRY);

    exit_ctls = set_vmx_controls(
        GUEST_EXIT_CTLS_ENABLE, GUEST_EXIT_CTLS_DISABLE, VMX_CTLS_EXIT);

    // Initialize the VMXON region
    memset(vmxon_region, 0x0, BASE_PAGE_SIZE);
    ia32_vmx_basic_t vmx_basic;
    vmx_basic = ia32_vmx_basic_rd(NULL);
    uint32_t vmcs_rev_id = ia32_vmx_basic_vmcs_rev_id_extract(vmx_basic);
    vmxon_region[0] = vmcs_rev_id;

    // The logical processor must use PAE paging
    uint64_t cr0 = rdcr0();
    if ((cr0 & CR0_PE) == 0 || (rdcr0() & CR0_PG) == 0) {
        return SYS_ERR_VMKIT_UNAVAIL;
    }

    // The CR0 register value has to support all of the CR0 fixed bits
    if (cr0 != vmx_fixed_cr0()) {
        return SYS_ERR_VMKIT_UNAVAIL;
    }

    // Enable virtualization, if not already enabled
    if (!vmx_enabled()) {
        enable_vmx();
    }
    // The CR4 register value has to support all of the CR4 fixed bits
    if (rdcr4() != vmx_fixed_cr4()) {
        return SYS_ERR_VMKIT_UNAVAIL;
    }
	  
    // Execute VMXON to place processor into VMX root operation
    errval_t err = vmxon(mem_to_local_phys((lvaddr_t)vmxon_region));
    assert(err_is_ok(err));

    return SYS_ERR_OK;
}

static inline void vmx_set_exception_bitmap(void)
{
    errval_t err = vmwrite(VMX_EXCP_BMP, ~(1UL << 7)); 
    assert(err_is_ok(err));
}

#ifndef CONFIG_ARRAKISMON
static uint64_t vmx_read_msr(uint32_t index) {
    uint64_t val;
    switch (index) { 
    case MSR_KERNEL_GS_BASE:
        val = ia32_kernel_gs_base_rd(NULL);
	break;
    case MSR_STAR:
        val = ia32_star_rd(NULL);
	break;
    case MSR_LSTAR:
        val = ia32_lstar_rd(NULL);
	break;
    case MSR_CSTAR:
        val = ia32_cstar_rd(NULL);
	break;
    case MSR_SFMASK:
        val = ia32_fmask_rd(NULL);
	break;
    default:
        assert(!"MSR index not supported");
    }
    return val;
}

static void vmx_host_msr_area_init(struct msr_entry *msr_area)
{ 
    for (int i = 0; i < VMX_MSR_COUNT; i++) {
        msr_area[i].index = msr_list[i];
	msr_area[i].val = vmx_read_msr(msr_list[i]);
    }
}
#endif

// Writes the host state, which is used after a VM-exit, to the
// current VMCS
static void vmx_set_host_state(void)
{    
    // On a page-fault the processor checks whether:
    // (#PF error-code) & (#PF error-code mask) = (#PF error-code match)
    
    // Setting the mask to 0, the match to 0xFFFFFFFF, and bit 14 in the 
    // exception bitmap results in no VM-exits on guest page-faults.
    errval_t err = vmwrite(VMX_PF_ERR_MASK, 0);
    err += vmwrite(VMX_PF_ERR_MATCH, 0xFFFFFFFF);
    err += vmwrite(VMX_CR3_TARGET_CNT, 0);

    uint64_t cr0 = rdcr0(), cr3 = rdcr3(), cr4 = rdcr4();

    uint64_t cr0_fixed0 = ia32_vmx_cr0_fixed0_rd(NULL);
    uint64_t cr0_fixed1 = ia32_vmx_cr0_fixed1_rd(NULL);
    uint64_t cr4_fixed0 = ia32_vmx_cr4_fixed0_rd(NULL);
    uint64_t cr4_fixed1 = ia32_vmx_cr4_fixed1_rd(NULL);

    assert((~cr0 & cr0_fixed0) == 0); 
    assert((cr0 & ~cr0_fixed1) == 0);
    assert((~cr4 & cr4_fixed0) == 0);
    assert((cr4 & ~cr4_fixed1) == 0);

    assert(((cr0 | cr0_fixed0) & cr0_fixed1) == cr0);
    assert(((cr4 | cr4_fixed0) & cr4_fixed1) == cr4);
    assert(rdcr4() & CR4_PAE);
    
    err += vmwrite(VMX_HOST_CR0, cr0); 
    err += vmwrite(VMX_HOST_CR3, cr3);
    err += vmwrite(VMX_HOST_CR4, cr4);
    
    err += vmwrite(VMX_HOST_ES_SEL, rd_es() & ~0x7);
    err += vmwrite(VMX_HOST_CS_SEL, rd_cs() & ~0x7);
    err += vmwrite(VMX_HOST_SS_SEL, rd_ss() & ~0x7);
    err += vmwrite(VMX_HOST_DS_SEL, rd_ds() & ~0x7);
    err += vmwrite(VMX_HOST_TR_SEL, rd_tr() & ~0x7);

    err += vmwrite(VMX_HOST_TR_BASE, tr_addr(rd_tr(), gdtr_addr(rd_gdtr())));
    err += vmwrite(VMX_HOST_GDTR_BASE, gdtr_addr(rd_gdtr()));
    err += vmwrite(VMX_HOST_IDTR_BASE, idtr_addr(rd_idtr()));
    err += vmwrite(VMX_HOST_SYSENTER_CS, 0);
    err += vmwrite(VMX_HOST_SYSENTER_ESP, 0);
    err += vmwrite(VMX_HOST_SYSENTER_EIP, 0);
    err += vmwrite(VMX_HOST_PAT_F, ia32_cr_pat_rd(NULL));

    ia32_efer_t efer_msr = ia32_efer_rd(NULL);    
    err += vmwrite(VMX_HOST_EFER_F, efer_msr);
    assert(ia32_efer_lme_extract(efer_msr));
    assert(ia32_efer_lma_extract(efer_msr));

    err += vmwrite(VMX_HOST_GS_SEL, 0x0);
    err += vmwrite(VMX_HOST_GS_BASE, 0x0);       
    
    err += vmwrite(VMX_HOST_FS_SEL, 0x0);
    err += vmwrite(VMX_HOST_FS_BASE, 0x0);

    err += vmwrite(VMX_HOST_RIP, (uint64_t)(&vmx_return_func));
#ifndef CONFIG_ARRAKISMON
    vmx_host_msr_area_init(host_msr_area);

    lpaddr_t msr_area_base = mem_to_local_phys((lvaddr_t)host_msr_area);
    err += vmwrite(VMX_EXIT_MSR_LOAD_F, canonical_form(msr_area_base));
    err += vmwrite(VMX_EXIT_MSR_LOAD_CNT, VMX_MSR_COUNT);
#endif
    assert(err_is_ok(err));
}

// Writes the VMX controls to the current VMCS. 
void vmx_set_exec_ctls(void)
{
    // VM-execution controls
    errval_t err = vmwrite(VMX_EXEC_PIN_BASED, pin_based_ctls); 
    err += vmwrite(VMX_EXEC_PRIM_PROC, pp_based_ctls); 
    err += vmwrite(VMX_EXEC_SEC_PROC, sp_based_ctls);
    
    // VM-entry and VM-exit control fields
    err += vmwrite(VMX_EXIT_CONTROLS, exit_ctls); 
    err += vmwrite(VMX_ENTRY_CONTROLS, entry_ctls); 

    vmx_set_exception_bitmap();

    err += vmwrite(VMX_ENTRY_INTR_INFO, 0);
    err += vmwrite(VMX_ENTRY_EXCP_ERR, 0);
    err += vmwrite(VMX_ENTRY_INSTR_LEN, 0);
    assert(err_is_ok(err));
}

errval_t initialize_vmcs(lpaddr_t vmcs_paddr)
{
    struct vmcs *vmcs = (struct vmcs *)local_phys_to_mem(vmcs_paddr);

    ia32_vmx_basic_t vmx_basic;
    vmx_basic = ia32_vmx_basic_rd(NULL);
    uint32_t vmcs_rev_id = ia32_vmx_basic_vmcs_rev_id_extract(vmx_basic);

    memset(vmcs, 0x0, BASE_PAGE_SIZE);
    vmcs->prelude.p.revision_id = vmcs_rev_id;
    vmcs->prelude.p.shadow = 0;
    errval_t err = vmclear(vmcs_paddr);
    err += vmptrld(vmcs_paddr);

    err += vmwrite(VMX_GUEST_VMCS_LPTR_F, ~0x0);
    err += vmwrite(VMX_GUEST_VMCS_LPTR_H, ~0x0);
    err += vmwrite(VMX_GUEST_SYSENTER_CS, 0x0);
    err += vmwrite(VMX_GUEST_SYSENTER_ESP, 0x0);
    err += vmwrite(VMX_GUEST_SYSENTER_EIP, 0x0);
#ifdef CONFIG_ARRAKISMON
    err += vmwrite(VMX_GUEST_DR7, 0x0);
    err += vmwrite(VMX_GUEST_EFER_F, ia32_efer_rd(NULL) | EFER_LME | EFER_LMA);

    err += vmwrite(VMX_GUEST_ACTIV_STATE, 0x0);
    err += vmwrite(VMX_GUEST_INTR_STATE, 0x0);

    err += vmwrite(VMX_GUEST_CS_LIM, 0xFFFFFFFF);
    err += vmwrite(VMX_GUEST_DS_LIM, 0xFFFFFFFF);
    err += vmwrite(VMX_GUEST_ES_LIM, 0xFFFFFFFF);
    err += vmwrite(VMX_GUEST_SS_LIM, 0xFFFFFFFF);
    err += vmwrite(VMX_GUEST_FS_LIM, 0xFFFFFFFF);
    err += vmwrite(VMX_GUEST_GS_LIM, 0xFFFFFFFF);
    err += vmwrite(VMX_GUEST_TR_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_LDTR_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_GDTR_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_IDTR_LIM, 0xFFFF);

    err += vmwrite(VMX_GUEST_CS_ACCESS, 0xA09B);
    err += vmwrite(VMX_GUEST_DS_ACCESS, 0xC093);
    err += vmwrite(VMX_GUEST_ES_ACCESS, 0xC093);
    err += vmwrite(VMX_GUEST_FS_ACCESS, 0xC093);
    err += vmwrite(VMX_GUEST_GS_ACCESS, 0xC093);
    err += vmwrite(VMX_GUEST_SS_ACCESS, 0xC093);
    err += vmwrite(VMX_GUEST_TR_ACCESS, 0x8B);
    err += vmwrite(VMX_GUEST_LDTR_ACCESS, 0x82);

    err += vmwrite(VMX_GUEST_CS_SEL, 0x8);
    err += vmwrite(VMX_GUEST_SS_SEL, 0x10);
    err += vmwrite(VMX_GUEST_DS_SEL, 0x10);
    err += vmwrite(VMX_GUEST_ES_SEL, 0x10); 
    err += vmwrite(VMX_GUEST_FS_SEL, 0x10);
    err += vmwrite(VMX_GUEST_GS_SEL, 0x10);
    err += vmwrite(VMX_GUEST_TR_SEL, 0x10); 
    err += vmwrite(VMX_GUEST_LDTR_SEL, 0x10);
    
    err += vmwrite(VMX_GUEST_CS_BASE, 0x0);
    err += vmwrite(VMX_GUEST_SS_BASE, 0x0);
    err += vmwrite(VMX_GUEST_DS_BASE, 0x0);
    err += vmwrite(VMX_GUEST_ES_BASE, 0x0);
    err += vmwrite(VMX_GUEST_FS_BASE, 0x0);
    err += vmwrite(VMX_GUEST_GS_BASE, 0x0);    
    err += vmwrite(VMX_GUEST_TR_BASE, 0x0);
    err += vmwrite(VMX_GUEST_LDTR_BASE, 0x0);
    err += vmwrite(VMX_GUEST_GDTR_BASE, 0x0);
    err += vmwrite(VMX_GUEST_IDTR_BASE, 0x0);

    uint64_t guest_cr0 = 0x60000010 | CR0_PE | CR0_PG;
    err += vmwrite(VMX_GUEST_CR0, (uint32_t)(guest_cr0 | ia32_vmx_cr0_fixed0_rd(NULL)) & 
		   ia32_vmx_cr0_fixed1_rd(NULL));

    uint64_t guest_cr4 = CR4_PAE;
    err += vmwrite(VMX_GUEST_CR4, (guest_cr4 | ia32_vmx_cr4_fixed0_rd(NULL)) & 
		   ia32_vmx_cr4_fixed1_rd(NULL));

    err += vmwrite(VMX_CR0_GH_MASK, 0UL);
    err += vmwrite(VMX_CR4_GH_MASK, 0UL);
#else
    err += vmwrite(VMX_GUEST_DR7, 0x400);
    err += vmwrite(VMX_GUEST_EFER_F, 0x0);
    err += vmwrite(VMX_GUEST_PAT_F, 0x0007040600070406ul);

    err += vmwrite(VMX_GUEST_ACTIV_STATE, 0x0);
    err += vmwrite(VMX_GUEST_INTR_STATE, 0x0);

    err += vmwrite(VMX_GUEST_CS_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_DS_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_ES_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_FS_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_GS_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_SS_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_TR_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_LDTR_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_GDTR_LIM, 0xFFFF);
    err += vmwrite(VMX_GUEST_IDTR_LIM, 0xFFFF);

    err += vmwrite(VMX_GUEST_CS_ACCESS, 0x9B);
    err += vmwrite(VMX_GUEST_DS_ACCESS, 0x93);
    err += vmwrite(VMX_GUEST_ES_ACCESS, 0x93);
    err += vmwrite(VMX_GUEST_FS_ACCESS, 0x93);
    err += vmwrite(VMX_GUEST_GS_ACCESS, 0x93);
    err += vmwrite(VMX_GUEST_SS_ACCESS, 0x93);
    err += vmwrite(VMX_GUEST_TR_ACCESS, 0x8B);
    err += vmwrite(VMX_GUEST_LDTR_ACCESS, 0x82);

    err += vmwrite(VMX_GUEST_CS_SEL, 0x0);
    err += vmwrite(VMX_GUEST_DS_SEL, 0x0);
    err += vmwrite(VMX_GUEST_ES_SEL, 0x0);
    err += vmwrite(VMX_GUEST_FS_SEL, 0x0);
    err += vmwrite(VMX_GUEST_GS_SEL, 0x0);
    err += vmwrite(VMX_GUEST_SS_SEL, 0x0);
    err += vmwrite(VMX_GUEST_TR_SEL, 0x0);
    err += vmwrite(VMX_GUEST_LDTR_SEL, 0x0);

    err += vmwrite(VMX_GUEST_CS_BASE, 0x0);
    err += vmwrite(VMX_GUEST_DS_BASE, 0x0);
    err += vmwrite(VMX_GUEST_ES_BASE, 0x0);
    err += vmwrite(VMX_GUEST_FS_BASE, 0x0);    
    err += vmwrite(VMX_GUEST_GS_BASE, 0x0);
    err += vmwrite(VMX_GUEST_SS_BASE, 0x0);
    err += vmwrite(VMX_GUEST_TR_BASE, 0x0);
    err += vmwrite(VMX_GUEST_LDTR_BASE, 0x0);    
    err += vmwrite(VMX_GUEST_GDTR_BASE, 0x0);
    err += vmwrite(VMX_GUEST_IDTR_BASE, 0x0);

    err += vmwrite(VMX_GUEST_RFLAGS, 0x200002);
    err += vmwrite(VMX_GUEST_RIP, 0xFFF0);
    err += vmwrite(VMX_GUEST_RSP, 0x0);

    uint64_t guest_cr0 = (0x60000010 | ia32_vmx_cr0_fixed0_rd(NULL)) & 
        ia32_vmx_cr0_fixed1_rd(NULL);
    err += vmwrite(VMX_GUEST_CR0, guest_cr0 & ~(CR0_PE | CR0_PG));

    uint64_t guest_cr4 = ia32_vmx_cr4_fixed0_rd(NULL) &
        ia32_vmx_cr4_fixed1_rd(NULL);
    assert((guest_cr4 & CR4_PCIDE) == 0);
    err += vmwrite(VMX_GUEST_CR4, guest_cr4);
        
    uint64_t cr0_shadow;
    err += vmread(VMX_GUEST_CR0, &cr0_shadow);

    err += vmwrite(VMX_CR0_RD_SHADOW, cr0_shadow);
    err += vmwrite(VMX_CR0_GH_MASK, CR0_PE);
    err += vmwrite(VMX_CR4_GH_MASK, 0x0);
#endif
    assert(err_is_ok(err));

    vmx_set_exec_ctls();
    
    return SYS_ERR_OK;
}

static uint32_t fail = 0;

static inline void enter_guest(void)
{
    // Set the host state prior to every VM-entry in case the values 
    // written to the VMCS change. 
    vmx_set_host_state();

    // This is necessary or else a #GPF will be incurred in the 
    // monitor domain.
    uint16_t ldtr_sel = rd_ldtr();

    // Perform most checks that are performed by the processor
    if (!launched) {
        check_guest_state_area();
	check_host_state_area();
	check_vmx_controls();
    }

    __asm volatile("mov %[ctrl], %%rdi\n\t"
		   
		   // save host host
		   "mov %%rsp, %%r8\n\t"
		   "mov %[host_rsp_encoding], %%r9\n\t"
		   "vmwrite %%r8, %%r9\n\t"
		   
		   "mov %%rbx, (148 + 1*8)(%%rdi)\n\t"
		   "mov %%rbp, (148 + 6*8)(%%rdi)\n\t"
		   "mov %%r12, (148 + 12*8)(%%rdi)\n\t"
		   "mov %%r13, (148 + 13*8)(%%rdi)\n\t"
		   "mov %%r14, (148 + 14*8)(%%rdi)\n\t"
		   "mov %%r15, (148 + 15*8)(%%rdi)\n\t"
		   "mov %%cr2, %%rsi\n\t"
		   "mov %%rsi, 38*8(%%rdi)\n\t"

		   // load guest state		   
		   "mov 37*8(%%rdi), %%rsi\n\t"
		   "mov %%rsi, %%cr2\n\t"		   

		   "mov 0*8(%%rdi), %%rax\n\t"
		   "mov 1*8(%%rdi), %%rbx\n\t"
		   "mov 2*8(%%rdi), %%rcx\n\t"
		   "mov 3*8(%%rdi), %%rdx\n\t"
		   "mov 4*8(%%rdi), %%rsi\n\t"
		   "mov 6*8(%%rdi), %%rbp\n\t"
		   "mov 8*8(%%rdi), %%r8\n\t"
		   "mov 9*8(%%rdi), %%r9\n\t"
		   "mov 10*8(%%rdi), %%r10\n\t"
		   "mov 11*8(%%rdi), %%r11\n\t"
		   "mov 12*8(%%rdi), %%r12\n\t"
		   "mov 13*8(%%rdi), %%r13\n\t"
		   "mov 14*8(%%rdi), %%r14\n\t"
		   "mov 15*8(%%rdi), %%r15\n\t"
		   "mov 5*8(%%rdi), %%rdi\n\t"
		   
		   // enter the guest VM
		   "cmpl $0, %[launched]\n\t"
		   "jne 1f\n\t"
		   "sti\n\t"
		   "vmlaunch\n\t"
		   "jmp 2f\n\t"
		   "1: "
		   "sti\n\t"
		   "vmresume\n\t"
		   "2: "
		   "setbe %[fail]\n\t"
		   "vmx_return_func:\n\t"
		   "cli\n\t"
		   
		   "push %%rdi\n\t"
		   "mov %[ctrl], %%rdi\n\t"

		   // save guest state
		   "mov %%rax, 0*8(%%rdi)\n\t"
		   "mov %%rbx, 1*8(%%rdi)\n\t"
		   "mov %%rcx, 2*8(%%rdi)\n\t"
		   "mov %%rdx, 3*8(%%rdi)\n\t"
		   "mov %%rsi, 4*8(%%rdi)\n\t"
		   "mov %%rbp, 6*8(%%rdi)\n\t"
		   "mov %%r8, 8*8(%%rdi)\n\t"
		   "mov %%r9, 9*8(%%rdi)\n\t"
		   "mov %%r10, 10*8(%%rdi)\n\t"
		   "mov %%r11, 11*8(%%rdi)\n\t"
		   "mov %%r12, 12*8(%%rdi)\n\t"
		   "mov %%r13, 13*8(%%rdi)\n\t"
		   "mov %%r14, 14*8(%%rdi)\n\t"
		   "mov %%r15, 15*8(%%rdi)\n\t"
		   
		   "mov %%cr2, %%rsi\n\t"
		   "mov %%rsi, 37*8(%%rdi)\n\t"

		   "pop %%rsi\n\t"
		   "mov %%rsi, 5*8(%%rdi)\n\t"

		   // load host state
		   "mov (148 + 1*8)(%%rdi), %%rbx\n\t"
		   "mov (148 + 6*8)(%%rdi), %%rbp\n\t"
		   "mov (148 + 12*8)(%%rdi), %%r12\n\t"
		   "mov (148 + 13*8)(%%rdi), %%r13\n\t"
		   "mov (148 + 14*8)(%%rdi), %%r14\n\t"
		   "mov (148 + 15*8)(%%rdi), %%r15\n\t"
		   "mov 38*8(%%rdi), %%rsi\n\t"
		   "mov %%rsi, %%cr2\n\t"
		   : [fail] "=m" (fail)  
		   : [ctrl] "m" (ctrl), [launched] "m" (launched),
		     [host_rsp_encoding] "i" (VMX_HOST_RSP)
		   : "memory"
		   );
    assert(!fail);
    wr_ldtr(ldtr_sel);

    launched = 1;
}

static inline void print_vmcs_info(struct guest_control *g)
{
    uint64_t guest_rip, guest_rsp, guest_rflags;
    uint64_t reason, exit_qual;
    uint64_t exit_intr_info, intr_err;
    uint64_t idt_vec_info, idt_vec_err;
    uint64_t instr_len, instr_info;
    uint64_t instr_error, gpaddr, gladdr;  
    uint64_t entry_intr_info, activ_state, intr_state;
    uint64_t guest_cr0, guest_cr3, guest_cr4;
    uint64_t guest_efer;

    uint64_t guest_es_sel, guest_es_base, guest_es_lim, guest_es_access;
    uint64_t guest_cs_sel, guest_cs_base, guest_cs_lim, guest_cs_access; 
    uint64_t guest_ss_sel, guest_ss_base, guest_ss_lim, guest_ss_access;
    uint64_t guest_ds_sel, guest_ds_base, guest_ds_lim, guest_ds_access;
    uint64_t guest_fs_sel, guest_fs_base, guest_fs_lim, guest_fs_access;
    uint64_t guest_gs_sel, guest_gs_base, guest_gs_lim, guest_gs_access;
    uint64_t guest_tr_sel, guest_tr_base, guest_tr_lim, guest_tr_access;
    uint64_t guest_ldtr_sel, guest_ldtr_base, guest_ldtr_lim, guest_ldtr_access;
    uint64_t guest_idtr_base, guest_idtr_lim;
    uint64_t guest_gdtr_base, guest_gdtr_lim;

    errval_t err = vmread(VMX_GUEST_ES_SEL, &guest_es_sel);
    err += vmread(VMX_GUEST_ES_BASE, &guest_es_base);
    err += vmread(VMX_GUEST_ES_LIM, &guest_es_lim);
    err += vmread(VMX_GUEST_ES_ACCESS, &guest_es_access);
    err += vmread(VMX_GUEST_CS_SEL, &guest_cs_sel);
    err += vmread(VMX_GUEST_CS_BASE, &guest_cs_base);
    err += vmread(VMX_GUEST_CS_LIM, &guest_cs_lim);
    err += vmread(VMX_GUEST_CS_ACCESS, &guest_cs_access);
    err += vmread(VMX_GUEST_SS_SEL, &guest_ss_sel);
    err += vmread(VMX_GUEST_SS_BASE, &guest_ss_base);
    err += vmread(VMX_GUEST_SS_LIM, &guest_ss_lim);
    err += vmread(VMX_GUEST_SS_ACCESS, &guest_ss_access);
    err += vmread(VMX_GUEST_DS_SEL, &guest_ds_sel);
    err += vmread(VMX_GUEST_DS_BASE, &guest_ds_base);
    err += vmread(VMX_GUEST_DS_LIM, &guest_ds_lim);
    err += vmread(VMX_GUEST_DS_ACCESS, &guest_ds_access);
    err += vmread(VMX_GUEST_FS_SEL, &guest_fs_sel);
    err += vmread(VMX_GUEST_FS_BASE, &guest_fs_base);
    err += vmread(VMX_GUEST_FS_LIM, &guest_fs_lim);
    err += vmread(VMX_GUEST_FS_ACCESS, &guest_fs_access);
    err += vmread(VMX_GUEST_GS_SEL, &guest_gs_sel);
    err += vmread(VMX_GUEST_GS_BASE, &guest_gs_base);
    err += vmread(VMX_GUEST_GS_LIM, &guest_gs_lim);
    err += vmread(VMX_GUEST_GS_ACCESS, &guest_gs_access);
    err += vmread(VMX_GUEST_TR_SEL, &guest_tr_sel);
    err += vmread(VMX_GUEST_TR_BASE, &guest_tr_base);
    err += vmread(VMX_GUEST_TR_LIM, &guest_tr_lim);
    err += vmread(VMX_GUEST_TR_ACCESS, &guest_tr_access);
    err += vmread(VMX_GUEST_LDTR_SEL, &guest_ldtr_sel);
    err += vmread(VMX_GUEST_LDTR_BASE, &guest_ldtr_base);
    err += vmread(VMX_GUEST_LDTR_LIM, &guest_ldtr_lim);
    err += vmread(VMX_GUEST_LDTR_ACCESS, &guest_ldtr_access);
    err += vmread(VMX_GUEST_IDTR_BASE, &guest_idtr_base);
    err += vmread(VMX_GUEST_IDTR_LIM, &guest_idtr_lim);
    err += vmread(VMX_GUEST_GDTR_BASE, &guest_gdtr_base);
    err += vmread(VMX_GUEST_GDTR_LIM, &guest_gdtr_lim);
    
    err += vmread(VMX_GUEST_RIP, &guest_rip); 
    err += vmread(VMX_GUEST_RSP, &guest_rsp);   
    err += vmread(VMX_GUEST_RFLAGS, &guest_rflags);
    err += vmread(VMX_EXIT_REASON, &reason);
    err += vmread(VMX_EXIT_QUAL, &exit_qual); 
    err += vmread(VMX_EXIT_INTR_INFO, &exit_intr_info);
    err += vmread(VMX_EXIT_INTR_ERR, &intr_err);
    err += vmread(VMX_IDT_VEC_INFO, &idt_vec_info);
    err += vmread(VMX_IDT_VEC_ERR, &idt_vec_err);
    err += vmread(VMX_INSTR_ERROR, &instr_error);
    err += vmread(VMX_GPADDR_F, &gpaddr);
    err += vmread(VMX_GL_ADDR, &gladdr);
    err += vmread(VMX_ENTRY_INTR_INFO, &entry_intr_info);
    err += vmread(VMX_GUEST_ACTIV_STATE, &activ_state);
    err += vmread(VMX_GUEST_INTR_STATE, &intr_state);
    err += vmread(VMX_EXIT_INSTR_LEN, &instr_len);
    err += vmread(VMX_EXIT_INSTR_INFO, &instr_info);
    err += vmread(VMX_GUEST_CR0, &guest_cr0);
    err += vmread(VMX_GUEST_CR3, &guest_cr3);
    err += vmread(VMX_GUEST_CR4, &guest_cr4);
    err += vmread(VMX_GUEST_EFER_F, &guest_efer);
    assert(err_is_ok(err));

    printf("VMCS info:\n");
    printf("\tvmexit reason = %d\n", (int)reason & 0xFFFF);
    printf("\texit qualification = 0x%"PRIx64"\n", exit_qual);  
    printf("\tBit 31 of reason = %x\n", ((int)reason >> 31) & 1);
    
    printf("\tVM-exit interruption information = 0x%"PRIx64"\n", exit_intr_info);
    printf("\tVM-exit interruption error = 0x%"PRIx64"\n", intr_err);

    printf("\tVM-entry interruption info=0x%"PRIx64"\n", entry_intr_info);

    printf("\tIDT vector information = 0x%"PRIx64"\n", idt_vec_info);
    printf("\tIDT vector error = 0x%"PRIx64"\n", idt_vec_err);

    printf("\tInstruction error = 0x%"PRIx64", gladdr = 0x%"PRIx64", gpaddr = 0x%"PRIx64"\n",
	   instr_error, gpaddr, gladdr);
    printf("\tActivity state=0x%"PRIx64", Interruptibility state=0x%"PRIx64"\n", 
	   activ_state, intr_state);  
    printf("\tVM-exit instruction length = 0x%"PRIx64"\n", instr_len);
    printf("\tVM-exit instruction info = 0x%"PRIx64"\n", instr_info);
    
    printf("\tguest_rip = 0x%"PRIx64", guest_rflags = 0x%"PRIx64"\n", 
	   guest_rip, guest_rflags);
    printf("\tRAX=0x%"PRIx64"    RBX=0x%"PRIx64"    RCX=0x%"PRIx64"    RDX=0x%"PRIx64"\n",
	   g->regs.rax, g->regs.rbx, g->regs.rcx, g->regs.rdx);
    printf("\tRSP=0x%"PRIx64"    RBP=0x%"PRIx64"    RSI=0x%"PRIx64"    RDI=0x%"PRIx64"\n",
	   guest_rsp, g->regs.rbp, g->regs.rsi, g->regs.rdi);
    printf("\tR8 =0x%"PRIx64"    R9 =0x%"PRIx64"    R10=0x%"PRIx64"    R11=0x%"PRIx64"\n",
	   g->regs.r8, g->regs.r9, g->regs.r10, g->regs.r11);
    printf("\tR12=0x%"PRIx64"    R13=0x%"PRIx64"    R14=0x%"PRIx64"    R15=0x%"PRIx64"\n",
	   g->regs.r12, g->regs.r13, g->regs.r14, g->regs.r15);
    printf("\tCR0=0x%"PRIx64", CR3=0x%"PRIx64", CR4=0x%"PRIx64"\n", 
	   guest_cr0, guest_cr3, guest_cr4);
    
    printf("\tES: sel=0x%"PRIx64", base=0x%"PRIx64", lim=0x%"PRIx64", access=0x%"PRIx64"\n", 
	   guest_es_sel, guest_es_base, guest_es_lim, guest_es_access);  
    printf("\tCS: sel=0x%"PRIx64", base=0x%"PRIx64", lim=0x%"PRIx64", access=0x%"PRIx64"\n", 
	   guest_cs_sel, guest_cs_base, guest_cs_lim, guest_cs_access);  
    printf("\tSS: sel= 0x%"PRIx64", base=0x%"PRIx64", lim=0x%"PRIx64", access=0x%"PRIx64"\n", 
	   guest_ss_sel, guest_ss_base, guest_ss_lim, guest_ss_access);  
    printf("\tDS: sel=0x%"PRIx64", base=0x%"PRIx64", lim=0x%"PRIx64", access=0x%"PRIx64"\n", 
	   guest_ds_sel, guest_ds_base, guest_ds_lim, guest_ds_access);  
    printf("\tFS: sel=0x%"PRIx64", base=0x%"PRIx64", lim=0x%"PRIx64", access=0x%"PRIx64"\n", 
	   guest_fs_sel, guest_fs_base, guest_fs_lim, guest_fs_access);  
    printf("\tGS: sel=0x%"PRIx64", base=0x%"PRIx64", lim=0x%"PRIx64", access=0x%"PRIx64"\n", 
	   guest_gs_sel, guest_gs_base, guest_gs_lim, guest_gs_access);  
    printf("\tTR: sel=0x%"PRIx64", base=0x%"PRIx64", lim=0x%"PRIx64", access=0x%"PRIx64"\n", 
	   guest_tr_sel, guest_tr_base, guest_tr_lim, guest_tr_access);  
    printf("\tLDTR: sel=0x%"PRIx64", base=0x%"PRIx64", lim=0x%"PRIx64", access=0x%"PRIx64"\n", 
	   guest_ldtr_sel, guest_ldtr_base, guest_ldtr_lim, guest_ldtr_access);  
    printf("\tIDTR: base=0x%"PRIx64", lim=0x%"PRIx64"\n", 
	   guest_idtr_base, guest_idtr_lim);  
    printf("\tGDTR: base=0x%"PRIx64", lim=0x%"PRIx64"\n", 
	   guest_gdtr_base, guest_gdtr_lim);  

    printf("\tEFER = 0x%"PRIx64"\n", guest_efer);
}

static inline uint64_t interruption_type(uint64_t intr_info) {
    return (intr_info >> 8) & 0x7;
}

static void __attribute__ ((noreturn)) 
call_monitor(struct dcb *dcb)
{
    ctrl->num_vm_exits_with_monitor_invocation++;
    /* the guest exited not due to an interrupt but some condition the
     * monitor has to handle, therefore notify the monitor */

    assert(dcb->is_vm_guest);

    // disable the domain
    scheduler_remove(dcb);

    // call the monitor
    errval_t err = lmp_deliver_notification(&dcb->guest_desc.monitor_ep.cap);
    if (err_is_fail(err)) {
        printk(LOG_ERR, "Unexpected error delivering VMEXIT");
    }
    
    // run the monitor
    dispatch(dcb->guest_desc.monitor_ep.cap.u.endpoint.listener);
}

struct sysret sys_syscall(uint64_t syscall, uint64_t arg0, uint64_t arg1,
                          uint64_t *args, uint64_t rflags, uint64_t rip);

extern uint64_t user_stack_save;

void __attribute__ ((noreturn))
vmx_vmkit_vmenter (struct dcb *dcb)
{
    errval_t err;
    lpaddr_t lpaddr = gen_phys_to_local_phys(dcb->guest_desc.ctrl.cap.u.frame.base);
    ctrl = (void *)local_phys_to_mem(lpaddr);

    assert(dcb != NULL);
    assert(dcb->vspace != 0);
    assert(dcb->is_vm_guest);

    if (ept_enabled()) {
        err = vmwrite(VMX_EPTP_F, ((dcb->vspace) & pa_width_mask() & ~BASE_PAGE_MASK) | 0x18);
	assert(err_is_ok(err));
    } else {
        err = vmwrite(VMX_GUEST_CR3, dcb->vspace);
	assert(err_is_ok(err));
    }
   
 vmx_vmenter_loop:

    enter_guest();

    uint16_t exit_reason;
    err = vmread(VMX_EXIT_REASON, (uint64_t *)&exit_reason);

    switch(exit_reason) {
    case VMX_EXIT_REASON_INVAL_VMCS:
      {
	// A condition that violates ones of the processor checks may be violated 
	// during the execution of the guest. With the Linux guest we used, the GS
	// limit is set to 0x10ffef, which causes one of the checks to fail. 
	uint64_t gs_lim;
	err += vmread(VMX_GUEST_GS_LIM, &gs_lim);
	assert(gs_lim == 0x10ffef);
	err += vmwrite(VMX_GUEST_GS_LIM, 0xfffef);
	assert(err_is_ok(err));
      }
      goto vmx_vmenter_loop;

    case VMX_EXIT_REASON_EXCEPTION:
      {
        uint64_t intr_info, type;
	err += vmread(VMX_EXIT_INTR_INFO, &intr_info);
	assert(err_is_ok(err));

	type = interruption_type(intr_info);

	if (type != TYPE_NMI) {
	    call_monitor(dcb);
	    break;
	}
      }
    case VMX_EXIT_REASON_EXT_INTR:
    case VMX_EXIT_REASON_SMI:
      {
	ctrl->num_vm_exits_without_monitor_invocation++;

#ifdef CONFIG_ARRAKISMON
	uint64_t guest_rip, guest_rsp, guest_rflags;
	err += vmread(VMX_GUEST_RIP, &guest_rip);
	err += vmread(VMX_GUEST_RSP, &guest_rsp);
	err += vmread(VMX_GUEST_RFLAGS, &guest_rflags);
	
	uint64_t guest_fs_sel, guest_gs_sel;
	err += vmread(VMX_GUEST_FS_SEL, &guest_fs_sel);	
	err += vmread(VMX_GUEST_GS_SEL, &guest_gs_sel);
	assert(err_is_ok(err));

	arch_registers_state_t *area = NULL;

	// Store user state into corresponding save area
	if(dispatcher_is_disabled_ip(dcb->disp, guest_rip)) {
	    area = dispatcher_get_disabled_save_area(dcb->disp);
	    dcb->disabled = true;
	} else {
	    area = dispatcher_get_enabled_save_area(dcb->disp);
	    dcb->disabled = false;
	}
	memcpy(area, &ctrl->regs, sizeof(arch_registers_state_t));
	area->rip = guest_rip;
	area->rax = ctrl->regs.rax;
	area->rsp = guest_rsp;
	area->eflags = guest_rflags;
	area->fs = guest_fs_sel;
	area->gs = guest_gs_sel;
#endif	
	wait_for_interrupt();
      }
      break;
#ifdef CONFIG_ARRAKISMON
    case VMX_EXIT_REASON_VMCALL:
      {
	// Translate this to a SYSCALL
	struct registers_x86_64 *regs = &ctrl->regs;
	uint64_t args[10] = {
	    regs->r10, regs->r8, regs->r9, regs->r12, regs->r13, regs->r14,
	    regs->r15, regs->rax, regs->rbp, regs->rbx
	};
	
	/* printf("VMMCALL\n"); */

	uint64_t guest_rip, guest_rsp, guest_rflags;
	err += vmread(VMX_GUEST_RIP, &guest_rip);
	err += vmread(VMX_GUEST_RSP, &guest_rsp);
	err += vmread(VMX_GUEST_RFLAGS, &guest_rflags);
	// Advance guest RIP to next instruction
	err += vmwrite(VMX_GUEST_RIP, guest_rip + 3);
	assert(err_is_ok(err));

	user_stack_save = guest_rsp;
	
	struct sysret ret = sys_syscall(regs->rdi, regs->rsi, regs->rdx, 
					args, guest_rflags, guest_rip + 3);
	regs->rax = ret.error;
	regs->rdx = ret.value;
      }
      goto vmx_vmenter_loop;
#endif
    default:      
        call_monitor(dcb);
	break;
    }
}
