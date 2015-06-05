/**
 * \file
 * \brief Contains VMKit kernel interface for version using SVM extensions.
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

#ifndef SVM_VMKIT_H
#define SVM_VMKIT_H

// SVM relevant CPUID info
#define CPUID_AMD_EXTFEAT       0x80000001
#define AMD_EXTFEAT_ECX_SVM     (1 << 2)

// some EXITCODE values
#define VMEXIT_INTR     0x60
#define VMEXIT_NMI      0x61
#define VMEXIT_SMI      0x62
#define VMEXIT_VMMCALL  0x81

errval_t svm_enable_virtualization (void);
void __attribute__ ((noreturn)) svm_vmkit_vmenter (struct dcb *dcb);

#endif // SVM_VMKIT_H
