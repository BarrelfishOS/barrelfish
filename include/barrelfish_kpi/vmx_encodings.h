/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef VMX_ENCODINGS_H
#define VMX_ENCODINGS_H

// 16-bit field encodings

// read-only data fields
#define VMX_VPID 0x0 // Virtual-processor identifier (VPID)
#define VMX_PINV 0x2 // Posted-interrupt notification vector
#define VMX_EPTP 0x4 // EPTP index

// guest-state fields
#define VMX_GUEST_ES_SEL 0x800 // Guest ES selector
#define VMX_GUEST_CS_SEL 0x802 // Guest CS selector
#define VMX_GUEST_SS_SEL 0x804 // Guest SS selector
#define VMX_GUEST_DS_SEL 0x806 // Guest DS selector
#define VMX_GUEST_FS_SEL 0x808 // Guest FS selector
#define VMX_GUEST_GS_SEL 0x80A // Guest GS selector
#define VMX_GUEST_LDTR_SEL 0x80C // Guest LDTR selector
#define VMX_GUEST_TR_SEL 0x80E // Guest TR selector
#define VMX_GUEST_IS 0x810 // Guest interrupt status

// host-state fields
#define VMX_HOST_ES_SEL 0xC00 // Guest ES selector
#define VMX_HOST_CS_SEL 0xC02 // Guest CS selector
#define VMX_HOST_SS_SEL 0xC04 // Guest SS selector
#define VMX_HOST_DS_SEL 0xC06 // Guest DS selector
#define VMX_HOST_FS_SEL 0xC08 // Guest FS selector
#define VMX_HOST_GS_SEL 0xC0A // Guest GS selector
#define VMX_HOST_TR_SEL 0xC0C // Guest TR selector

// 32-bit field encodings

// control fields
#define VMX_EXEC_PIN_BASED 0x4000 // Pin-based controls
#define VMX_EXEC_PRIM_PROC 0x4002 // Primary processor-based controls
#define VMX_EXCP_BMP 0x4004 // Exception bitmap
#define VMX_PF_ERR_MASK 0x4006 // Page-fault error-code mask
#define VMX_PF_ERR_MATCH 0x4008 // Page-fault error-code match
#define VMX_CR3_TARGET_CNT 0x400A // CR3-target count
#define VMX_EXIT_CONTROLS 0x400C // VM-exit controls
#define VMX_EXIT_MSR_STORE_CNT 0x400E // VM-exit MSR-store count
#define VMX_EXIT_MSR_LOAD_CNT 0x4010 // VM-exit MSR-load count
#define VMX_ENTRY_CONTROLS 0x4012 // VM-entry controls
#define VMX_ENTRY_MSR_LOAD_CNT 0x4014 // VM-entry MSR-load count
#define VMX_ENTRY_INTR_INFO 0x4016 // VM-entry interruption-information field
#define VMX_ENTRY_EXCP_ERR 0x4018 // VM-entry exception error code
#define VMX_ENTRY_INSTR_LEN 0x401A // VM-entry instruction length
#define VMX_TPR_THRESHOLD 0x401C // TPR threshold
#define VMX_EXEC_SEC_PROC 0x401E // Secondary processor-based controls
#define VMX_PLE_GAP 0x4020 // PLE_Gap
#define VMX_PLE_WINDOW 0x4022 // PLE_Window

// read-only data fields
#define VMX_INSTR_ERROR 0x4400 // VM-instruction error
#define VMX_EXIT_REASON 0x4402 // Exit reason
#define VMX_EXIT_INTR_INFO 0x4404 // VM-exit interruption information
#define VMX_EXIT_INTR_ERR 0x4406 // VM-exit interruption error code
#define VMX_IDT_VEC_INFO 0x4408 // IDT-vectoring information field
#define VMX_IDT_VEC_ERR 0x440A // IDT-vectoring error code
#define VMX_EXIT_INSTR_LEN 0x440C // VM-exit instruction length
#define VMX_EXIT_INSTR_INFO 0x440E // VM-exit instruction information

// guest-state fields
#define VMX_GUEST_ES_LIM 0x4800 // Guest ES limit
#define VMX_GUEST_CS_LIM 0x4802 // Guest CS limit
#define VMX_GUEST_SS_LIM 0x4804 // Guest SS limit
#define VMX_GUEST_DS_LIM 0x4806 // Guest DS limit
#define VMX_GUEST_FS_LIM 0x4808 // Guest FS limit
#define VMX_GUEST_GS_LIM 0x480A // Guest GS limit
#define VMX_GUEST_LDTR_LIM 0x480C // Guest LDTR limit
#define VMX_GUEST_TR_LIM 0x480E // Guest TR limit
#define VMX_GUEST_GDTR_LIM 0x4810 // Guest GDTR limit
#define VMX_GUEST_IDTR_LIM 0x4812 // Guest IDTR limit
#define VMX_GUEST_ES_ACCESS 0x4814 // Guest ES access rights
#define VMX_GUEST_CS_ACCESS 0x4816 // Guest CS access rights
#define VMX_GUEST_SS_ACCESS 0x4818 // Guest SS access rights
#define VMX_GUEST_DS_ACCESS 0x481A // Guest DS access rights
#define VMX_GUEST_FS_ACCESS 0x481C // Guest FS access rights
#define VMX_GUEST_GS_ACCESS 0x481E // Guest GS access rights
#define VMX_GUEST_LDTR_ACCESS 0x4820 // Guest LDTR access rights
#define VMX_GUEST_TR_ACCESS 0x4822 // Guest TR access rights
#define VMX_GUEST_INTR_STATE 0x4824 // Guest activity state
#define VMX_GUEST_ACTIV_STATE 0x4826 // Guest activity state
#define VMX_GUEST_SMBASE 0x4828 // Guest SMBASE
#define VMX_GUEST_SYSENTER_CS 0x482A // Guest IA32_SYSENTER_CS
#define VMX_GUEST_PREEMPT_TIMER 0x482E // VMX-preemption timer value

// host-state fields
#define VMX_HOST_SYSENTER_CS 0x4C00 // Host IA32_SYSENTER_CS

// 64-bit field encodings

// control fields
#define VMX_IOBMP_A_F 0x2000 // Address of I/O bitmap A (full)
#define VMX_IOBMP_A_H 0x2001 // Address of I/O bitmap A (high)
#define VMX_IOBMP_B_F 0x2002 // Address of I/O bitmap B (full)
#define VMX_IOBMP_B_H 0x2003 // Address of I/O bitmap B (high)
#define VMX_MSRBMP_F 0x2004 // Address of MSR bitmaps (full)
#define VMX_MSRBMP_H 0x2005 // Address of MSR bitmaps (high)
#define VMX_EXIT_MSR_STORE_F 0x2006 // VM-exit MSR-store address (full)
#define VMX_EXIT_MSR_STORE_H 0x2007 // VM-exit MSR-store address (high)
#define VMX_EXIT_MSR_LOAD_F 0x2008 // VM-exit MSR-load address (full)
#define VMX_EXIT_MSR_LOAD_H 0x2009 // VM-exit MSR-load address (high)
#define VMX_ENTRY_MSR_LOAD_F 0x200A // VM-entry MSR-load address (full)
#define VMX_ENTRY_MSR_LOAD_H 0x200B // VM-entry MSR-load address (high)
#define VMX_EVMCS_PTR_F 0x200C // Executive-VMCS pointer (full)
#define VMX_EVMCS_PTR_H 0x200D // Executive-VMCS pointer (high)
#define VMX_TSC_OFF_F 0x2010 // TSC offset (full)
#define VMX_TSC_OFF_H 0x2011 // TSC offset (high)
#define VMX_VAPIC_F 0x2012 // Virtual-APIC address (full)
#define VMX_VAPIC_H 0x2013 // Virtual-APIC address (high)
#define VMX_APIC_ACC_F 0x2014 // APIC-access address (full)
#define VMX_APIC_ACC_H 0x2015 // APIC-access address (high)
#define VMX_PID_F 0x2016 // Posted-interrupt descriptor address (full)
#define VMX_PID_H 0x2017 // Posted-interrupt descriptor address (high)
#define VMX_VMFUNC_F 0x2018 // VM-function controls (full)
#define VMX_VMFUNC_H 0x2019 // VM-function controls (high)
#define VMX_EPTP_F 0x201A // EPT pointer (full)
#define VMX_EPTP_H 0x201B // EPT pointer (high)
#define VMX_EOI_EXIT_BMP0_F 0x201C // EOI-exit bitmap 0 (full)
#define VMX_EOI_EXIT_BMP0_H 0x201D // EOI-exit bitmap 0 (high)
#define VMX_EOI_EXIT_BMP1_F 0x201E // EOI-exit bitmap 1 (full)
#define VMX_EOI_EXIT_BMP1_H 0x201F // EOI-exit bitmap 1 (high)
#define VMX_EOI_EXIT_BMP2_F 0x2020 // EOI-exit bitmap 2 (full)
#define VMX_EOI_EXIT_BMP2_H 0x2021 // EOI-exit bitmap 2 (high)
#define VMX_EOI_EXIT_BMP3_F 0x2022 // EOI-exit bitmap 3 (full)
#define VMX_EOI_EXIT_BMP3_H 0x2023 // EOI-exit bitmap 3 (high)
#define VMX_EPTP_LIST_F 0x2024 // EPTP-list address (full)
#define VMX_EPTP_LIST_H 0x2025 // EPTP-list address (high)
#define VMX_VMREAD_BMP_F 0x2026 // VMREAD-bitmap address (full)
#define VMX_VMREAD_BMP_H 0x2027 // VMREAD-bitmap address (high)
#define VMX_VMWRITE_BMP_F 0x2028 // VMWRITE-bitmap address (full)
#define VMX_VMWRITE_BMP_H 0x2029 // VMWRITE-bitmap address (high)
#define VMX_VEXCP_INFO_F 0x202A // Virtualization-exception info. address (full)
#define VMX_VEXCP_INFO_H 0x202B // Virtualization-exception info. address (high)

// read-only data fields
#define VMX_GPADDR_F 0x2400 // Guest-physical address (full)
#define VMX_GPADDR_H 0x2401 // Guest-physical address (high)

// guest-state fields
#define VMX_GUEST_VMCS_LPTR_F 0x2800 // VMCS link pointer (full)
#define VMX_GUEST_VMCS_LPTR_H 0x2801 // VMCS link pointer (high)
#define VMX_GUEST_DCTL_F 0x2802 // Guest IA32_DEBUGCTL (full)
#define VMX_GUEST_DCTL_H 0x2803 // Guest IA32_DEBUGCTL (high)
#define VMX_GUEST_PAT_F 0x2804 // Guest IA32_PAT (full)
#define VMX_GUEST_PAT_H 0x2805 // Guest IA32_PAT (high)
#define VMX_GUEST_EFER_F 0x2806 // Guest IA32_EFER (full)
#define VMX_GUEST_EFER_H 0x2807 // Guest IA32_EFER (high)
#define VMX_GUEST_PGC_F 0x2808 // Guest IA32_PERF_GLOBAL_CTRL (full)
#define VMX_GUEST_PGC_H 0x2809 // Guest IA32_PERF_GLOBAL_CTRL (high)
#define VMX_GUEST_PDPTE0_F 0x280A // Guest PDPTE0 (full)
#define VMX_GUEST_PDPTE0_H 0x280B // Guest PDPTE0 (high)
#define VMX_GUEST_PDPTE1_F 0x280C // Guest PDPTE1 (full)
#define VMX_GUEST_PDPTE1_H 0x280D // Guest PDPTE1 (high)
#define VMX_GUEST_PDPTE2_F 0x280E // Guest PDPTE2 (full)
#define VMX_GUEST_PDPTE2_H 0x280F // Guest PDPTE2 (high) 
#define VMX_GUEST_PDPTE3_F 0x2810 // Guest PDPTE3 (full)
#define VMX_GUEST_PDPTE3_H 0x2811 // Guest PDPTE3 (high)

// host-state fields
#define VMX_HOST_PAT_F 0x2C00 // Host IA32_PAT (full)
#define VMX_HOST_PAT_H 0x2C01 // Host IA32_PAT (high)
#define VMX_HOST_EFER_F 0x2C02 // Host IA32_EFER (full)
#define VMX_HOST_EFER_H 0x2C03 // Host IA32_EFER (high)
#define VMX_HOST_PGC_F 0x2C04 // Host IA32_PERF_GLOBAL_CTRL (full)
#define VMX_HOST_PGC_H 0x2C05 // Host IA32_PERF_GLOBAL_CTRL (high)

// Natural-width field encodings

// control fields
#define VMX_CR0_GH_MASK 0x6000 // CR0 guest/host mask
#define VMX_CR4_GH_MASK 0x6002 // CR4 guest/host mask
#define VMX_CR0_RD_SHADOW 0x6004 // CR0 read shadow 
#define VMX_CR4_RD_SHADOW 0x6006 // CR4 read shadow
#define VMX_CR3_T0 0x6008 // CR3-target value 0
#define VMX_CR3_T1 0x600A // CR3-target value 1
#define VMX_CR3_T2 0x600C // CR3-target value 2
#define VMX_CR3_T3 0x600E // CR3-target value 3

// read-only data fields
#define VMX_EXIT_QUAL 0x6400 // Exit qualification
#define VMX_IO_RCX 0x6402 // I/O RCX
#define VMX_IO_RSI 0x6404 // I/O RSI
#define VMX_IO_RDI 0x6406 // I/O RDI
#define VMX_IO_RIP 0x6408 // I/O RIP
#define VMX_GL_ADDR 0x640A // Guest-linear address

// guest-state fields
#define VMX_GUEST_CR0 0x6800 // Guest CR0
#define VMX_GUEST_CR3 0x6802 // Guest CR3
#define VMX_GUEST_CR4 0x6804 // Guest CR4
#define VMX_GUEST_ES_BASE 0x6806 // Guest ES base
#define VMX_GUEST_CS_BASE 0x6808 // Guest CS base
#define VMX_GUEST_SS_BASE 0x680A // Guest SS base
#define VMX_GUEST_DS_BASE 0x680C // Guest DS base
#define VMX_GUEST_FS_BASE 0x680E // Guest FS base
#define VMX_GUEST_GS_BASE 0x6810 // Guest GS base
#define VMX_GUEST_LDTR_BASE 0x6812 // Guest LDTR base
#define VMX_GUEST_TR_BASE 0x6814 // Guest TR base
#define VMX_GUEST_GDTR_BASE 0x6816 // Guest GDTR base
#define VMX_GUEST_IDTR_BASE 0x6818 // Guest IDTR base
#define VMX_GUEST_DR7 0x681A // Guest DR7
#define VMX_GUEST_RSP 0x681C // Guest RSP
#define VMX_GUEST_RIP 0x681E // Guest RIP
#define VMX_GUEST_RFLAGS 0x6820 // Guest RFLAGS
#define VMX_GUEST_PDEXCP 0x6822 // Guest pending debug exceptions
#define VMX_GUEST_SYSENTER_ESP  0x6824 // Guest IA32_SYSENTER_ESP
#define VMX_GUEST_SYSENTER_EIP  0x6826 // Guest IA32_SYSENTER_EIP

// host-state fields
#define VMX_HOST_CR0 0x6C00 // Host CR0
#define VMX_HOST_CR3 0x6C02 // Host CR3
#define VMX_HOST_CR4 0x6C04 // Host CR4
#define VMX_HOST_FS_BASE 0x6C06 // Host FS base
#define VMX_HOST_GS_BASE 0x6C08 // Host GS base
#define VMX_HOST_TR_BASE 0x6C0A // Host TR base
#define VMX_HOST_GDTR_BASE 0x6C0C // Host GDTR base
#define VMX_HOST_IDTR_BASE 0x6C0E // Host IDTR base
#define VMX_HOST_SYSENTER_ESP 0x6C10 // Host IA32_SYSENTER_ESP
#define VMX_HOST_SYSENTER_EIP 0x6C12 // Host IA32_SYSENTER_EIP
#define VMX_HOST_RSP 0x6C14 // Host RSP
#define VMX_HOST_RIP 0x6C16 // Host RIP

#endif // VMX_ENCODINGS_H
