/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef VMX_EXIT_REASONS_H
#define VMX_EXIT_REASONS_H

#define VMX_EXIT_REASON_EXCEPTION           0
#define VMX_EXIT_REASON_EXT_INTR            1
#define VMX_EXIT_REASON_TRIPLE_FAULT        2
#define VMX_EXIT_REASON_INIT                3
#define VMX_EXIT_REASON_SIPI                4
#define VMX_EXIT_REASON_IO_SMI              5
#define VMX_EXIT_REASON_SMI                 6
#define VMX_EXIT_REASON_INTR_WINDOW         7
#define VMX_EXIT_REASON_NMI_WINDOW          8
#define VMX_EXIT_REASON_TASK_SWITCH         9
#define VMX_EXIT_REASON_CPUID               10
#define VMX_EXIT_REASON_GETSEC              11
#define VMX_EXIT_REASON_HLT                 12
#define VMX_EXIT_REASON_INVD                13
#define VMX_EXIT_REASON_INVLPG              14
#define VMX_EXIT_REASON_RDPMC               15
#define VMX_EXIT_REASON_RDTSC               16
#define VMX_EXIT_REASON_RSM                 17
#define VMX_EXIT_REASON_VMCALL              18
#define VMX_EXIT_REASON_VMCLEAR             19
#define VMX_EXIT_REASON_VMLAUNCH            20
#define VMX_EXIT_REASON_VMPTRLD             21
#define VMX_EXIT_REASON_VMPTRST             22
#define VMX_EXIT_REASON_VMREAD              23
#define VMX_EXIT_REASON_VMRESUME            24
#define VMX_EXIT_REASON_VMWRITE             25
#define VMX_EXIT_REASON_VMXOFF              26
#define VMX_EXIT_REASON_VMXON               27
#define VMX_EXIT_REASON_CR_ACCESS           28
#define VMX_EXIT_REASON_DR_ACCESS           29
#define VMX_EXIT_REASON_INOUT               30
#define VMX_EXIT_REASON_RDMSR               31
#define VMX_EXIT_REASON_WRMSR               32
#define VMX_EXIT_REASON_INVAL_VMCS          33
#define VMX_EXIT_REASON_INVAL_MSR           34
#define VMX_EXIT_REASON_MWAIT               36
#define VMX_EXIT_REASON_MTF                 37
#define VMX_EXIT_REASON_MONITOR             39
#define VMX_EXIT_REASON_PAUSE               40
#define VMX_EXIT_REASON_MCE                 41
#define VMX_EXIT_REASON_TPR                 43
#define VMX_EXIT_REASON_APIC_ACCESS         44
#define VMX_EXIT_REASON_VIRTUALIZED_EOI     45
#define VMX_EXIT_REASON_GDTR_IDTR           46
#define VMX_EXIT_REASON_LDTR_TR             47
#define VMX_EXIT_REASON_EPT_FAULT           48
#define VMX_EXIT_REASON_EPT_MISCONFIG       49
#define VMX_EXIT_REASON_INVEPT              50
#define VMX_EXIT_REASON_RDTSCP              51
#define VMX_EXIT_REASON_VMX_PREEMPT         52
#define VMX_EXIT_REASON_INVVPID             53
#define VMX_EXIT_REASON_WBINVD              54
#define VMX_EXIT_REASON_XSETBV              55
#define VMX_EXIT_REASON_APIC_WRITE          56

#endif // VMX_EXIT_REASONS_H
