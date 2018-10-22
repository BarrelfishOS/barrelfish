/**
 * \file
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

#ifndef INTEL_VMX_H
#define INTEL_VMX_H

#include <stdint.h>
#include <barrelfish_kpi/vmx_controls.h>
#include <barrelfish_kpi/vmx_encodings.h>
#include <barrelfish_kpi/vmx_exit_reasons.h>

// Number of MSRs that are stored/loaded for the guest on VM-exit/VM-entry
#define VMX_MSR_COUNT 5

// Size of the guest MSR store/load area
#define VMX_MSR_AREA_SIZE (VMX_MSR_COUNT * 16)

#define VMX_EXIT_REASON_SWINT 57
#define RFLAGS_CF (1UL << 0)
#define RFLAGS_IF (1UL << 9)
#define CR0_PE (1UL << 0)
#define CR0_PG (1UL << 31)
#define CR4_PAE (1UL << 5)
#define EFER_LMA (1UL << 10)
#define ACCESS_RIGHTS_LONG_MODE (1UL << 13)

/**
 * \brief Convenience macro to write real-mode segmentation registers
 *
 * Write the selector, base and limit to a selector reg according to real-mode
 * segmentation rules to the VMCS.
 */

#define VMCS_WRITE_SEGREG_REALMODE(dcb_cap,reg,selector)		                                \
do {									                                \
    errval_t err_val = invoke_dispatcher_vmwrite(dcb_cap, VMX_GUEST_ ##reg## _SEL, (selector));         \
    err_val += invoke_dispatcher_vmwrite(dcb_cap, VMX_GUEST_ ##reg## _BASE, (selector) << 4);           \
    err_val += invoke_dispatcher_vmwrite(dcb_cap, VMX_GUEST_ ##reg## _LIM, ((selector) << 4) + 0xffff); \
    assert(err_is_ok(err_val));                                                                         \
} while (0)

#endif // INTEL_VMX_H
