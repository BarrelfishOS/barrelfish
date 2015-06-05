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

#ifndef VMX_VMKIT_H
#define VMX_VMKIT_H

#include <arch/x86/x86.h>
#include <arch/x86_64/x86.h>
#include <barrelfish_kpi/vmkit.h>
#include <barrelfish_kpi/vmx_controls.h>
#include <barrelfish_kpi/vmx_encodings.h>
#include <barrelfish_kpi/vmx_exit_reasons.h>

#include <dev/ia32_dev.h>

// Number of MSRs that are loaded on VM-exit
#define VMX_MSR_COUNT 5

// Size of the host MSR-load area
#define VMX_MSR_AREA_SIZE (VMX_MSR_COUNT * 16)

#define MSR_KERNEL_GS_BASE 0xc0000102
#define MSR_STAR           0xc0000081
#define MSR_LSTAR          0xc0000082
#define MSR_CSTAR          0xc0000083
#define MSR_SFMASK         0xc0000084

#define PAGE_SIZE 0x1000
#define CPUID_PA_WIDTH (0x80000008)
#define VMX_PA_WIDTH_MASK (0xFF)
#define CPUID_VMX (0x1) 
#define VMX_SUPPORT (1 << 5)

#define TYPE_EXT_INTR (0)
#define TYPE_NMI (2)
#define TYPE_HW_EXCP (3)
#define TYPE_SW_EXCP (6)

#define RFLAGS_IF (1 << 9)
#define RFLAGS_VM (1 << 17)

#define EFER_LME (1UL << 8)
#define EFER_LMA (1UL << 10)
#define EFER_NXE (1UL << 11)

#define CR0_PE (1 << 0)
#define CR0_NW (1 << 29)
#define CR0_CD (1 << 30)
#define CR0_PG (1 << 31)

#define CR4_PAE (1UL << 5)
#define CR4_MCE (1UL << 6)
#define CR4_PGE (1UL << 7)
#define CR4_PCE (1UL << 8)
#define CR4_VMXE (1UL << 13)
#define CR4_PCIDE (1UL << 17)

#define DWORD_MS(val) ((val >> 32) & 0xFFFFFFFF)
#define DWORD_LS(val) (val & 0xFFFFFFFF)

union vmcs_prelude {
    uint32_t raw;
    struct {
        uint32_t revision_id :31;
        uint32_t shadow      :1;
    } p;
};

// Represents a VMCS structure which is comprised of up to 4-KB, 
// the data portion of the structure is implemenation-specific.
struct vmcs {
    union vmcs_prelude prelude;
    uint32_t vmx_abort;
    char data[PAGE_SIZE - 8];
} __attribute__ ((aligned(PAGE_SIZE)));

static const int benign_exceptions[] = {1,2,3,4,5,6,7,9,16,17,18,19};
static inline bool benign_exception(int vector)
{
    for (int i = 0; i < sizeof(benign_exceptions); i++) {
        if (benign_exceptions[i] == vector) return true;
    }
    return false;
}

static const int contributory_exceptions[] = {0,10,11,12,13};
static inline bool contributory_exception(int vector)
{
    for (int i = 0; i < sizeof(contributory_exceptions); i++) {
        if (contributory_exceptions[i] == vector) return true;
    }
    return false;
}

// Returns true if secondary processor-based VM-execution controls 
// are used.
static inline bool sec_ctls_used(uint64_t pp_controls) 
{
    return !!(pp_controls & PP_CLTS_SEC_CTLS);
}

// Returns the canonical form of the address addr.
static inline uint64_t canonical_form(uint64_t addr)
{
    if ((addr >> 47) & 0x1) {
        return (addr | ~0xffffffffffffUL); 
    } else {
        return (addr & 0xffffffffffffUL);
    }
}

// Functions for reading segment registers are used in saving the 
// host state (via vmwrite instructions) prior to VM-entry

static inline uint16_t rd_es(void)
{
    uint16_t es;
    __asm volatile("mov %%es, %[es]" : [es] "=r" (es));
    return es;
}

static inline uint16_t rd_cs(void)
{
    uint16_t cs;
    __asm volatile("mov %%cs, %[cs]" : [cs] "=r" (cs));
    return cs;
}

static inline uint16_t rd_ss(void)
{
    uint16_t ss;
    __asm volatile("mov %%ss, %[ss]" : [ss] "=r" (ss));
    return ss;
}

static inline uint16_t rd_ds(void)
{
    uint16_t ds;
    __asm volatile("mov %%ds, %[ds]" : [ds] "=r" (ds));
    return ds;
}

static inline uint16_t rd_fs(void)
{
    uint16_t fs;
    __asm volatile("mov %%fs, %[fs]" : [fs] "=r" (fs));
    return fs;
}

static inline uint16_t rd_gs(void)
{
    uint16_t gs;
    __asm volatile("mov %%gs, %[gs]" : [gs] "=r" (gs));
    return gs;
}

static inline uint16_t rd_tr(void)
{
    uint16_t tr;
    __asm volatile("str %[tr]" : [tr] "=r" (tr));
    return tr;
}

static inline uint64_t rd_idtr(void)
{
    uint64_t idtr;
    __asm volatile("sidt %[idtr]" : [idtr] "=m" (idtr));
    return idtr;
}

static inline uint16_t rd_ldtr(void)
{
    uint16_t ldtr;
    __asm volatile("sldt %[ldtr]" : [ldtr] "=r" (ldtr));
    return ldtr;
}

static inline void wr_ldtr(uint16_t val)
{
  __asm volatile("lldt %[val]" :: [val] "m" (val) : "memory");
}

static inline uint64_t rd_gdtr(void)
{
    uint64_t gdtr;
    __asm volatile("sgdt %[gdtr]" : [gdtr] "=m" (gdtr));
    return gdtr;
}

static inline uint64_t gdtr_addr(uint64_t gdtr)
{
    return canonical_form(gdtr >> 16);
}

static inline uint64_t idtr_addr(uint64_t idtr)
{
    return canonical_form(idtr >> 16);
}

// Return true if the segment selector is in the the LDT (the TI flag 
// is set), else false, meaning that it's contained in the GDT.
static inline int seg_in_ldt(uint16_t seg_sel)
{
    return ((seg_sel >> 2) & 0x1);
}

// The index of corresponding segment descriptor in the LDT or GDT.
static inline int seg_sel_index(uint16_t seg_sel)
{
    return ((seg_sel >> 3) & 0x1FFF);
}

static inline uint64_t tr_addr(uint16_t tr_sel, uint64_t gdtr_base)
{
    int tss_index_low  = seg_sel_index(tr_sel);
    int tss_index_high = tss_index_low + 1;

    uint64_t *gdt_new = (uint64_t *)gdtr_base;
    uint64_t tss_low = gdt_new[tss_index_low];
    uint64_t tss_high = gdt_new[tss_index_high];
    
    uint64_t tss_base = (((tss_low >> 16) & 0xFFFFFF)   |
                         ((tss_low >> 32) & 0xFF000000) |
                         (tss_high << 32));
    return tss_base;
}

static inline void vmlaunch(void)
{
    __asm volatile("vmlaunch");
}

static inline void vmresume(void)
{
    __asm volatile("vmresume"); 
}

static inline void enable_vmx(void)
{
    wrcr4(rdcr4() | CR4_VMXE);
}

static inline int vmx_enabled(void)
{
    return (rdcr4() & CR4_VMXE);
}

static inline void disable_vmx(void)
{
    wrcr4(rdcr4() & ~CR4_VMXE);
}

static inline uint64_t vmx_fixed_cr0(void) 
{ 
    uint64_t cr0_fixed0 = ia32_vmx_cr0_fixed0_rd(NULL);
    uint64_t cr0_fixed1 = ia32_vmx_cr0_fixed1_rd(NULL);

    uint64_t cr0_1s_mask = (cr0_fixed0 & cr0_fixed1);
    uint64_t cr0_0s_mask = (~cr0_fixed0 & ~cr0_fixed1);
    return ((rdcr0() | cr0_1s_mask) & ~cr0_0s_mask);
}

static inline uint64_t vmx_fixed_cr4(void) 
{ 
    uint64_t cr4_fixed0 = ia32_vmx_cr4_fixed0_rd(NULL);
    uint64_t cr4_fixed1 = ia32_vmx_cr4_fixed1_rd(NULL);

    uint64_t cr4_1s_mask = (cr4_fixed0 & cr4_fixed1);
    uint64_t cr4_0s_mask = (~cr4_fixed0 & ~cr4_fixed1);
    return ((rdcr4() | cr4_1s_mask) & ~cr4_0s_mask);
}

static inline bool is_canonical(uint64_t addr)
{
    uint64_t canonical_addr = canonical_form(addr);
    return (canonical_addr == addr);
}

// Returns the physical-address width supported by the logical
// processor.
static inline uint64_t physical_address_width(void)
{
    uint32_t cpuid_eax;
    cpuid(CPUID_PA_WIDTH, &cpuid_eax, NULL, NULL, NULL);
    return (cpuid_eax & VMX_PA_WIDTH_MASK);
}

// Returns a mask for the bits 0:N-1, where N is the physical-address
// width supported by the logical processor.
static inline uint64_t pa_width_mask(void)
{
    uint64_t pa_width = physical_address_width();
    uint64_t physical_addr_width_mask = (1UL << pa_width) - 1;
    return physical_addr_width_mask;
}

errval_t vmptrld(lpaddr_t vmcs_base);
lpaddr_t vmptrst(void);
errval_t vmclear(lpaddr_t vmcs_base);
errval_t vmread(uintptr_t encoding, lvaddr_t *dest_addr);
errval_t vmwrite(uintptr_t encoding, uintptr_t source);
errval_t vmxon(lpaddr_t base_addr);
errval_t vmxoff(void);

errval_t initialize_vmcs(lpaddr_t vmcs_base);
void vmx_set_exec_ctls(void);

errval_t vmx_enable_virtualization (void);
void __attribute__ ((noreturn)) vmx_vmkit_vmenter (struct dcb *dcb);

#endif // VMX_VMKIT_H
