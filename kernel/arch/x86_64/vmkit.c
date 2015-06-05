/**
 * \file
 * \brief VMKit Kernel interface.
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <vmkit.h>
#include <x86.h>
#include <dispatch.h>
#include <exec.h>
#include <barrelfish_kpi/vmkit.h>
#include <barrelfish_kpi/syscalls.h>

#include <dev/amd_vmcb_dev.h>

errval_t
vmkit_enable_virtualization (void)
{
#ifdef CONFIG_SVM
    return svm_enable_virtualization();
#else
    return vmx_enable_virtualization();
#endif
}

void __attribute__ ((noreturn))
vmkit_vmexec (struct dcb *dcb, lvaddr_t entry)
{
    dispatcher_handle_t handle = dcb->disp;
    struct dispatcher_shared_generic *disp = get_dispatcher_shared_generic(handle);
    lpaddr_t lpaddr = gen_phys_to_local_phys(dcb->guest_desc.ctrl.cap.u.frame.base);
    struct guest_control *ctrl = (void *)local_phys_to_mem(lpaddr);
    memset(&ctrl->regs, 0, sizeof(struct registers_x86_64));
    ctrl->regs.rdi = disp->udisp;
#ifdef CONFIG_SVM
    lpaddr = gen_phys_to_local_phys(dcb->guest_desc.vmcb.cap.u.frame.base);
    amd_vmcb_t vmcb;
    amd_vmcb_initialize(&vmcb, (void *)local_phys_to_mem(lpaddr));

    amd_vmcb_rip_wr(&vmcb, disp->dispatcher_run);
    amd_vmcb_rsp_wr(&vmcb, 0);
    amd_vmcb_rax_wr(&vmcb, 0);
    amd_vmcb_rflags_wr_raw(&vmcb, USER_RFLAGS);
    amd_vmcb_fs_selector_wr(&vmcb, 0);
    amd_vmcb_gs_selector_wr(&vmcb, 0);
    svm_vmkit_vmenter(dcb);
#else
    vmwrite(VMX_GUEST_RIP, disp->dispatcher_run);
    vmwrite(VMX_GUEST_RSP, 0);
    vmwrite(VMX_GUEST_RFLAGS, USER_RFLAGS);	
    vmwrite(VMX_GUEST_FS_SEL, 0);
    vmwrite(VMX_GUEST_GS_SEL, 0);
    vmx_vmkit_vmenter(dcb);
#endif
}

void __attribute__ ((noreturn))
vmkit_vmenter (struct dcb *dcb)
{
#ifdef CONFIG_SVM
    svm_vmkit_vmenter(dcb);
#else
    vmx_vmkit_vmenter(dcb);
#endif
}
