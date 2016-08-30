/**
 * \file
 * \brief Low-level capability invocations
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INCLUDEBARRELFISH_INVOCATIONS_ARCH_H
#define INCLUDEBARRELFISH_INVOCATIONS_ARCH_H

#include <barrelfish/syscall_arch.h> // for sys_invoke and cap_invoke
#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish_kpi/distcaps.h>            // for distcap_state_t
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish/caddr.h>
#include <barrelfish_kpi/paging_arch.h>

/**
 * capability invocation syscall wrapper, copied from x86_32 version
 */
static inline struct sysret cap_invoke(struct capref to, uintptr_t argc, uintptr_t cmd,
                                       uintptr_t arg2, uintptr_t arg3,
                                       uintptr_t arg4, uintptr_t arg5,
                                       uintptr_t arg6, uintptr_t arg7,
                                       uintptr_t arg8, uintptr_t arg9,
                                       uintptr_t arg10, uintptr_t arg11)
{
    capaddr_t invoke_cptr = get_cap_addr(to);
    enum cnode_type invoke_level = get_cap_level(to);

    assert(cmd <= 0xFF);
    assert(invoke_level <= 0xF);
    // flags << 24 | invoke_bits << 16 | cmd << 8 | syscall_invoke
    // ^ used for LMP
    uint32_t invocation = ((invoke_level << 16) | (cmd << 8) | SYSCALL_INVOKE);

    switch (argc) {
        case 0:
        return syscall2(invocation, invoke_cptr);
        case 1:
        return syscall3(invocation, invoke_cptr, arg2);
        case 2:
        return syscall4(invocation, invoke_cptr, arg2, arg3);
        case 3:
        return syscall5(invocation, invoke_cptr, arg2, arg3, arg4);
        case 4:
        return syscall6(invocation, invoke_cptr, arg2, arg3, arg4, arg5);
        case 5:
        return syscall7(invocation, invoke_cptr, arg2, arg3, arg4, arg5, arg6);
        case 6:
        return syscall8(invocation, invoke_cptr, arg2, arg3, arg4, arg5, arg6,
                        arg7);
        case 7:
        return syscall9(invocation, invoke_cptr, arg2, arg3, arg4, arg5, arg6,
                        arg7, arg8);
        case 8:
        return syscall10(invocation, invoke_cptr, arg2, arg3, arg4, arg5, arg6,
                        arg7, arg8, arg9);
        case 9:
        return syscall11(invocation, invoke_cptr, arg2, arg3, arg4, arg5, arg6,
                         arg7, arg8, arg9, arg10);
        case 10:
        return syscall12(invocation, invoke_cptr, arg2, arg3, arg4, arg5, arg6,
                         arg7, arg8, arg9, arg10, arg11);
        default:
        return SYSRET(SYS_ERR_ILLEGAL_INVOCATION);
    }
    assert(!"reached");
}

#define cap_invoke11(to, _a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k)   \
    cap_invoke(to, 10, _a, _b, _c, _d, _e, _f, _g, _h, _i, _j, _k)
#define cap_invoke10(to, _a, _b, _c, _d, _e, _f, _g, _h, _i, _j)   \
    cap_invoke(to, 9, _a, _b, _c, _d, _e, _f, _g, _h, _i, _j, 0)
#define cap_invoke9(to, _a, _b, _c, _d, _e, _f, _g, _h, _i)        \
    cap_invoke(to, 8, _a, _b, _c, _d, _e, _f, _g, _h, _i, 0, 0)
#define cap_invoke8(to, _a, _b, _c, _d, _e, _f, _g, _h)    \
    cap_invoke(to, 7, _a, _b, _c, _d, _e, _f, _g, _h, 0, 0, 0)
#define cap_invoke7(to, _a, _b, _c, _d, _e, _f, _g)    \
    cap_invoke(to, 6, _a, _b, _c, _d, _e, _f, _g, 0, 0, 0, 0)
#define cap_invoke6(to, _a, _b, _c, _d, _e, _f)        \
    cap_invoke(to, 5, _a, _b, _c, _d, _e, _f, 0, 0, 0, 0, 0)
#define cap_invoke5(to, _a, _b, _c, _d, _e)            \
    cap_invoke(to, 4, _a, _b, _c, _d, _e, 0, 0, 0, 0, 0, 0)
#define cap_invoke4(to, _a, _b, _c, _d)                \
    cap_invoke(to, 3, _a, _b, _c, _d, 0, 0, 0, 0, 0, 0, 0)
#define cap_invoke3(to, _a, _b, _c)                    \
    cap_invoke(to, 2, _a, _b, _c, 0, 0, 0, 0, 0, 0, 0, 0)
#define cap_invoke2(to, _a, _b)                        \
    cap_invoke(to, 1, _a, _b, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#define cap_invoke1(to, _a)                            \
    cap_invoke(to, 0, _a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

/**
 * \brief Retype (part of) a capability.
 *
 * Retypes (part of) CPtr 'cap' into 'objsize'd caps of type 'newtype' and places them
 * into slots starting at slot 'slot' in the CNode, addressed by 'to', with
 * 'bits' address bits of 'to' valid.
 *
 * See also cap_retype(), which wraps this.
 *
 * \param root          Capability of the Root CNode to invoke
 * \param cap           Address of cap to retype.
 * \param offset        Offset into cap to retype
 * \param newtype       Kernel object type to retype to.
 * \param objsize       Size of created objects, for variable-sized types
 * \param count         Number of objects to create
 * \param to            Address of CNode cap to place retyped caps into.
 * \param slot          Slot in CNode cap to start placement.
 * \param bits          Number of valid address bits in 'to'.
 *
 * \return Error code
 */
STATIC_ASSERT(ObjType_Num < 0xFFFF, "retype invocation argument packing does not truncate enum objtype");
static inline errval_t
invoke_cnode_retype(struct capref root, capaddr_t src_cspace, capaddr_t cap,
                    gensize_t offset, enum objtype newtype, gensize_t objsize,
                    size_t count, capaddr_t to_cspace, capaddr_t to,
                    enum cnode_type to_level, capaddr_t slot)
{
    assert(cap != CPTR_NULL);

    assert(newtype < ObjType_Num);
    assert(offset <= 0xFFFFFFFF);
    assert(objsize <= 0xFFFFFFFF);
    assert(count <= 0xFFFFFFFF);
    assert(to_level <= 0xF);

    return cap_invoke10(root, CNodeCmd_Retype, src_cspace, cap, offset,
                        ((uint32_t)to_level << 16) | newtype,
                        objsize, count, to_cspace, to, slot).error;
}

static inline errval_t
invoke_vnode_map(struct capref ptable, capaddr_t slot,
                 capaddr_t src_root, capaddr_t src,
                 enum cnode_type srclevel, size_t
                 flags, size_t offset, size_t pte_count,
                 capaddr_t mcnroot, capaddr_t mcnaddr,
                 enum cnode_type mcnlevel, cslot_t mapping_slot)
{
    assert(slot <= 0xffff);
    assert(srclevel <= 0xf);
    assert(mcnlevel <= 0xf);
    assert(offset <= 0xffffffff);
    assert(flags <= 0xffffffff);
    assert(pte_count <= 0xffff);
    assert(mapping_slot <= L2_CNODE_SLOTS);

    uintptr_t small_values = srclevel |
                             (mcnlevel << 4) |
                             (mapping_slot << 8) |
                             (slot << 16);

    return cap_invoke9(ptable, VNodeCmd_Map, src_root, src, flags, offset,
                       pte_count, mcnroot, mcnaddr, small_values).error;
}

/**
 * \brief Duplicate ARMv7 core_data into the supplied frame.
 *
 * \param frame    CSpace address of frame capability
 *
 * \return Error code
 */

static inline errval_t
invoke_kcb_clone(struct capref kcb, struct capref frame)
{
    capaddr_t frame_cptr = get_cap_addr(frame);
    enum cnode_type frame_level = get_cap_level(frame);

    return cap_invoke3(kcb, KCBCmd_Clone, frame_cptr, frame_level).error;
}

static inline errval_t invoke_iocap_in(struct capref iocap, enum io_cmd cmd,
                                       uint16_t port, uint32_t *data)
{
    // Not strictly applicable on ARM
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t invoke_iocap_out(struct capref iocap, enum io_cmd cmd,
                                        uint16_t port, uint32_t data)
{
    // Not strictly applicable on ARM
    return LIB_ERR_NOT_IMPLEMENTED;
}


/**
 * \brief Setup a VM guest DCB
 *
 * \param dcb       Dispatcher capability
 */
static inline errval_t invoke_dispatcher_setup_guest(struct capref dispatcher,
                                                     capaddr_t ep_cap,
                                                     capaddr_t vnode,
                                                     capaddr_t vmkit_guest,
                                                     capaddr_t guest_control_cap)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}

/**
 * \brief Return the system-wide unique ID of the passed ID capability.
 *
 * \param idcap ID capability to invoke.
 * \param id    Filled-in with system-wide unique ID of ID cap.
 *
 * \return      Error code
 */
static inline errval_t invoke_idcap_identify(struct capref idcap,
                                             idcap_id_t *id)
{
    assert(id != NULL);
    assert(get_croot_addr(idcap) == CPTR_ROOTCN);

    return cap_invoke2(idcap, IDCmd_Identify, (uintptr_t)id).error;
}

static inline errval_t invoke_get_global_paddr(struct capref kernel_cap, genpaddr_t* global)
{
    assert(global != NULL);
    return cap_invoke2(kernel_cap, KernelCmd_GetGlobalPhys, (uintptr_t)global).error;
}

#endif
