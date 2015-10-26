/**
 * \file
 * \brief Contains definitions of VMX controls along with the controls 
 * that are desired to be used for each VMCS. 
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

#ifndef VMX_CONTROLS_H
#define VMX_CONTROLS_H

typedef uint32_t vmx_controls;

enum vmx_ctls_t {
    VMX_CTLS_PIN_BASED              = 0,
    VMX_CTLS_PRIMARY_PROCESSOR      = 1,
    VMX_CTLS_SECONDARY_PROCESSOR    = 2,
    VMX_CTLS_EXIT                   = 3,
    VMX_CTLS_ENTRY                  = 4
};

// Pin-based VM-execution controls
#define PIN_CTLS_EXT_INTR       (1 << 0)
#define PIN_CTLS_NMI            (1 << 3)
#define PIN_CTLS_VIRT_NMI       (1 << 5)
#define PIN_CTLS_PREEMPT_TIMER  (1 << 6)
#define PIN_CTLS_POSTED_INTR    (1 << 7)

// Primary processor-based VM-execution controls 
#define PP_CTLS_INTR_WINDOW     (1 << 2)
#define PP_CLTS_TSC_OFF         (1 << 3)
#define PP_CLTS_HLT             (1 << 7)
#define PP_CLTS_INVLPG          (1 << 9)
#define PP_CLTS_MWAIT           (1 << 10)
#define PP_CLTS_RDPMC           (1 << 11)
#define PP_CLTS_RDTSC           (1 << 12)
#define PP_CLTS_C3_LOAD         (1 << 15)
#define PP_CLTS_C3_STORE        (1 << 16)
#define PP_CLTS_C8_LOAD         (1 << 19)
#define PP_CLTS_C8_STORE        (1 << 20)
#define PP_CLTS_TPR_SHADOW      (1 << 21)
#define PP_CLTS_NMI_WINDOW      (1 << 22)
#define PP_CLTS_MOVDR           (1 << 23)
#define PP_CLTS_UNCOND_IO       (1 << 24)
#define PP_CLTS_IOBMP           (1 << 25)
#define PP_CLTS_MONITOR_TF      (1 << 27)
#define PP_CLTS_MSRBMP          (1 << 28)
#define PP_CLTS_MONITOR         (1 << 29)
#define PP_CLTS_PAUSE           (1 << 30)
#define PP_CLTS_SEC_CTLS        (1 << 31)

// Secondary processor-based VM-execution controls
#define SP_CLTS_VIRT_APIC       (1 << 0)
#define SP_CLTS_ENABLE_EPT      (1 << 1)
#define SP_CLTS_DESC_TABLE      (1 << 2)
#define SP_CLTS_RDTSCP          (1 << 3)
#define SP_CLTS_VIRT_X2APIC     (1 << 4)
#define SP_CLTS_ENABLE_VPID     (1 << 5)
#define SP_CLTS_WBINVD          (1 << 6)
#define SP_CLTS_UNRSTD_GUEST    (1 << 7)
#define SP_CLTS_VIRT_APIC_REG   (1 << 8)
#define SP_CLTS_VIRQ_DEL        (1 << 9)
#define SP_CLTS_PAUSE_LOOP      (1 << 10)
#define SP_CLTS_RDRAND          (1 << 11)
#define SP_CLTS_ENABLE_INVPCID  (1 << 12)
#define SP_CLTS_VMFUNC          (1 << 13)
#define SP_CLTS_VMCS_SHADOW     (1 << 14)
#define SP_CLTS_EPT_VIOL        (1 << 18)

// VM-exit controls
#define EXIT_CLTS_SAVE_DBG      (1 << 2)
#define EXIT_CLTS_HOST_SIZE     (1 << 9)
#define EXIT_CLTS_LOAD_PGC      (1 << 12)
#define EXIT_CLTS_ACK_INTR      (1 << 15)
#define EXIT_CLTS_SAVE_PAT      (1 << 18)
#define EXIT_CLTS_LOAD_PAT      (1 << 19)
#define EXIT_CLTS_SAVE_EFER     (1 << 20)
#define EXIT_CLTS_LOAD_EFER     (1 << 21)
#define EXIT_CLTS_SAVE_PREEMPT  (1 << 22)

// VM-entry controls
#define ENTRY_CLTS_LOAD_DBG     (1 << 2)
#define ENTRY_CLTS_IA32E_MODE   (1 << 9)
#define ENTRY_CLTS_SMM          (1 << 10)
#define ENTRY_CLTS_DUAL_MONITOR (1 << 11)
#define ENTRY_CLTS_LOAD_PGC     (1 << 13)
#define ENTRY_CLTS_LOAD_PAT     (1 << 14)
#define ENTRY_CLTS_LOAD_EFER    (1 << 15)

#endif // VMX_CONTROLS_H
