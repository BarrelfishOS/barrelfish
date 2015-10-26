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
#include <barrelfish/debug.h> // for USER_PANIC()

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
    // XXX: TODO
    USER_PANIC("NYI");
    uint8_t invoke_bits = get_cap_valid_bits(to);
    capaddr_t invoke_cptr = get_cap_addr(to) >> (CPTR_BITS - invoke_bits);

    assert(cmd < 0xFF);
    assert(invoke_bits < 0xFF);
    // flags << 24 | invoke_bits << 16 | cmd << 8 | syscall_invoke
    // ^ used for LMP
    uint32_t invocation = ((invoke_bits << 16) | (cmd << 8) | SYSCALL_INVOKE);

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
 * \brief Retype a capability.
 *
 * Retypes CPtr 'cap' into 2^'objbits' caps of type 'newtype' and places them
 * into slots starting at slot 'slot' in the CNode, addressed by 'to', with
 * 'bits' address bits of 'to' valid.
 *
 * See also cap_retype(), which wraps this.
 *
 * \param root          Capability of the CNode to invoke
 * \param cap           Address of cap to retype.
 * \param newtype       Kernel object type to retype to.
 * \param objbits       Size of created objects, for variable-sized types
 * \param to            Address of CNode cap to place retyped caps into.
 * \param slot          Slot in CNode cap to start placement.
 * \param bits          Number of valid address bits in 'to'.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_retype(struct capref root, capaddr_t cap,
                                           enum objtype newtype, int objbits,
                                           capaddr_t to, capaddr_t slot, int bits)
{
    assert(cap != CPTR_NULL);

    uint8_t invoke_bits = get_cap_valid_bits(root);
    capaddr_t invoke_cptr = get_cap_addr(root) >> (CPTR_BITS - invoke_bits);

    assert(newtype <= 0xffff);
    assert(objbits <= 0xff);
    assert(bits <= 0xff);
    return syscall6((invoke_bits << 16) | (CNodeCmd_Retype << 8) | SYSCALL_INVOKE, invoke_cptr, cap,
                    (newtype << 16) | (objbits << 8) | bits,
                    to, slot).error;
}

/**
 * \brief Create a capability.
 *
 * Create a new capability of type 'type' and size 'objbits'. The new cap will
 * be placed in the slot 'dest_slot' of the CNode located at 'dest_cnode_cptr'
 * in the address space rooted at 'root'.
 *
 * See also cap_create(), which wraps this.
 *
 * \param root            Capability of the CNode to invoke.
 * \param type            Kernel object type to create.
 * \param objbits         Size of created object
 *                        (ignored for fixed-size objects)
 * \param dest_cnode_cptr Address of CNode cap, where newly created cap will be
 *                        placed into.
 * \param dest_slot       Slot in CNode cap to place new cap.
 * \param dest_vbits      Number of valid address bits in 'dest_cnode_cptr'.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_create(struct capref root,
                                           enum objtype type, uint8_t objbits,
                                           capaddr_t dest_cnode_cptr,
                                           capaddr_t dest_slot,
                                           uint8_t dest_vbits)
{
    /* Pack arguments */
    assert(dest_cnode_cptr != CPTR_NULL);

    uint8_t invoke_bits = get_cap_valid_bits(root);
    capaddr_t invoke_cptr = get_cap_addr(root) >> (CPTR_BITS - invoke_bits);

    assert(type <= 0xffff);
    assert(objbits <= 0xff);
    assert(dest_vbits <= 0xff);

    return syscall5((invoke_bits << 16) | (CNodeCmd_Create << 8) | SYSCALL_INVOKE,
                    invoke_cptr, (type << 16) | (objbits << 8) | dest_vbits,
                    dest_cnode_cptr, dest_slot).error;
}

/**
 * \brief "Mint" a capability.
 *
 * Copies CPtr 'from' into slot 'slot' in the CNode, addressed by 'to', within
 * the address space, rooted at 'root' and with 'tobits' and 'frombits' address
 * bits of 'to' and 'from' valid, respectively.
 *
 * See also cap_mint(), which wraps this.
 *
 * \param root          Capability of the CNode to invoke
 * \param to            CNode to place copy into.
 * \param slot          Slot in CNode cap to place copy into.
 * \param from          Address of cap to copy.
 * \param tobits        Number of valid address bits in 'to'.
 * \param frombits      Number of valid address bits in 'from'.
 * \param param1        1st cap-dependent parameter.
 * \param param2        2nd cap-dependent parameter.
 *
 * \return Error code
 */

//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_cnode_mint(struct capref root, capaddr_t to,
		capaddr_t slot, capaddr_t from, int tobits,
		int frombits, uintptr_t param1,
		uintptr_t param2)
{
    uint8_t invoke_bits = get_cap_valid_bits(root);
    capaddr_t invoke_cptr = get_cap_addr(root) >> (CPTR_BITS - invoke_bits);

    assert(slot <= 0xffff);
    assert(tobits <= 0xff);
    assert(frombits <= 0xff);

    return syscall7((invoke_bits << 16) | (CNodeCmd_Mint << 8) | SYSCALL_INVOKE,
                    invoke_cptr, to, from,
                    (slot << 16) | (tobits << 8) | frombits,
                    param1, param2).error;
}

/**
 * \brief Copy a capability.
 *
 * Copies CPtr 'from' into slot 'slot' in the CNode, addressed by 'to', within
 * the address space, rooted at 'root' and with 'tobits' and 'frombits' address
 * bits of 'to' and 'from' valid, respectively.
 *
 * See also cap_copy(), which wraps this.
 *
 * \param root          Capability of the CNode to invoke
 * \param to            CNode to place copy into.
 * \param slot          Slot in CNode cap to place copy into.
 * \param from          Address of cap to copy.
 * \param tobits        Number of valid address bits in 'to'.
 * \param frombits      Number of valid address bits in 'from'.
 *
 * \return Error code
 */
//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower

#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_cnode_copy(struct capref root, capaddr_t to,
                                         capaddr_t slot, capaddr_t from, int tobits,
                                         int frombits)
{
    uint8_t invoke_bits = get_cap_valid_bits(root);
    capaddr_t invoke_cptr = get_cap_addr(root) >> (CPTR_BITS - invoke_bits);

    assert(slot <= 0xffff);
    assert(tobits <= 0xff);
    assert(frombits <= 0xff);

    return syscall5((invoke_bits << 16) | (CNodeCmd_Copy << 8) | SYSCALL_INVOKE,
                    invoke_cptr, to, from,
                    (slot << 16) | (tobits << 8) | frombits).error;
}

/**
 * \brief Delete a capability.
 *
 * Delete the capability pointed to by 'cap', with 'bits' address bits
 * of it valid, from the address space rooted at 'root'.
 *
 * \param root  Capability of the CNode to invoke
 * \param cap   Address of cap to delete.
 * \param bits  Number of valid bits within 'cap'.
 *
 * \return Error code
 */
//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_cnode_delete(struct capref root, capaddr_t cap,
                                           int bits)
{
    uint8_t invoke_bits = get_cap_valid_bits(root);
    capaddr_t invoke_cptr = get_cap_addr(root) >> (CPTR_BITS - invoke_bits);

    assert(bits <= 0xff);

    return syscall4((invoke_bits << 16) | (CNodeCmd_Delete << 8) | SYSCALL_INVOKE,
                    invoke_cptr, cap, bits).error;
}
//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_cnode_revoke(struct capref root, capaddr_t cap,
                                           int bits)
{
    uint8_t invoke_bits = get_cap_valid_bits(root);
    capaddr_t invoke_cptr = get_cap_addr(root) >> (CPTR_BITS - invoke_bits);

    assert(bits <= 0xff);

    return syscall4((invoke_bits << 16) | (CNodeCmd_Revoke << 8) | SYSCALL_INVOKE,
                    invoke_cptr, cap, bits).error;
}

//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_cnode_get_state(struct capref root, capaddr_t cap,
                                              int bits, distcap_state_t *ret)
{
    uint8_t invoke_bits = get_cap_valid_bits(root);
    capaddr_t invoke_cptr = get_cap_addr(root) >> (CPTR_BITS - invoke_bits);

    assert (bits <= 0xff);

    struct sysret sysret =
        syscall4((invoke_bits << 16) | (CNodeCmd_GetState << 8) | SYSCALL_INVOKE,
                invoke_cptr, cap, bits);

    assert(ret != NULL);
    if (err_is_ok(sysret.error)) {
        *ret = sysret.value;
    }
    else {
        *ret = 0;
    }
    return sysret.error;
}

//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_vnode_map(struct capref ptable, capaddr_t slot, capaddr_t from,
                 int frombits, uintptr_t flags, uintptr_t offset,
                 uintptr_t pte_count)
{
    uint8_t invoke_bits = get_cap_valid_bits(ptable);
    capaddr_t invoke_cptr = get_cap_addr(ptable) >> (CPTR_BITS - invoke_bits);

    assert(slot <= 0xffff);
    assert(frombits <= 0xff);

    // XXX: needs check of flags, offset, and pte_count sizes
    return syscall7((invoke_bits << 16) | (VNodeCmd_Map << 8) | SYSCALL_INVOKE,
                    invoke_cptr, from, (slot << 16) | frombits,
                    flags, offset, pte_count).error;
}

//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_vnode_unmap(struct capref cap, capaddr_t mapping_cptr, int mapping_bits,
                   size_t entry, size_t pte_count)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap);
    capaddr_t invoke_cptr = get_cap_addr(cap) >> (CPTR_BITS - invoke_bits);

    pte_count -= 1;

    assert(entry < 1024);
    assert(pte_count < 1024);
    assert(mapping_bits <= 0xff);

    return syscall4((invoke_bits << 16) | (VNodeCmd_Unmap << 8) | SYSCALL_INVOKE,
                    invoke_cptr, mapping_cptr,
                    ((mapping_bits & 0xff)<<20) | ((pte_count & 0x3ff)<<10) |
                     (entry & 0x3ff)).error;
}

/**
 * \brief Return the physical address and size of a frame capability
 *
 * \param frame    CSpace address of frame capability
 * \param ret      frame_identity struct filled in with relevant data
 *
 * \return Error code
 */

//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_frame_identify (struct capref frame, struct frame_identity *ret)
{
    uint8_t invoke_bits = get_cap_valid_bits(frame);
    capaddr_t invoke_cptr = get_cap_addr(frame) >> (CPTR_BITS - invoke_bits);

    uintptr_t arg1 = ((uintptr_t)invoke_bits) << 16;
    arg1 |= ((uintptr_t)FrameCmd_Identify<<8);
    arg1 |= (uintptr_t)SYSCALL_INVOKE;
    struct sysret sysret =
        syscall2(arg1, //(invoke_bits << 16) | (FrameCmd_Identify << 8) | SYSCALL_INVOKE,
                 invoke_cptr);

    assert(ret != NULL);
    if (err_is_ok(sysret.error)) {
        ret->base = sysret.value & (~BASE_PAGE_MASK);
        ret->bits = sysret.value & BASE_PAGE_MASK;
        return sysret.error;
    }

    ret->base = 0;
    ret->bits = 0;
    return sysret.error;
}

/**
 * \brief Return the physical address and size of a frame capability
 *
 * \param frame    CSpace address of frame capability
 * \param ret      frame_identity struct filled in with relevant data
 *
 * \return Error code
 */

//XXX: workaround for inline bug of arm-gcc 4.6.1 and lower
#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 1
static __attribute__((noinline, unused)) errval_t
#else
static inline errval_t
#endif
invoke_frame_modify_flags (struct capref frame, uintptr_t offset,
		uintptr_t pages, uintptr_t flags)
{
    uint8_t invoke_bits = get_cap_valid_bits(frame);
    capaddr_t invoke_cptr = get_cap_addr(frame) >> (CPTR_BITS - invoke_bits);

    uintptr_t arg1 = ((uintptr_t)invoke_bits) << 16;
    arg1 |= ((uintptr_t)FrameCmd_ModifyFlags<<8);
    arg1 |= (uintptr_t)SYSCALL_INVOKE;

    return syscall5(arg1, invoke_cptr, offset, pages, flags).error;
}

static inline errval_t invoke_iocap_in(struct capref iocap, enum io_cmd cmd,
                                       uint16_t port, uint32_t *data)
{
    // Not strictly applicable on ARM
//    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

static inline errval_t invoke_iocap_out(struct capref iocap, enum io_cmd cmd,
                                        uint16_t port, uint32_t data)
{
    // Not strictly applicable on ARM
//    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}


/**
 * \brief Setup a dispatcher, possibly making it runnable
 *
 * \param dispatcher    Address of dispatcher capability
 * \param domdispatcher Address of existing dispatcher for domain ID
 * \param cspace_root   Root of CSpace for new dispatcher
 * \param cspace_root_bits  Number of valid bits in cspace_root
 * \param vspace_root   Root of VSpace for new dispatcher
 * \param dispatcher_frame Frame capability for dispatcher structure
 * \param run           Make runnable if true
 *
 * Any arguments of CPTR_NULL are ignored.
 *
 * \return Error code
 */
static inline errval_t
invoke_dispatcher(struct capref dispatcher, struct capref domdispatcher,
                  struct capref cspace, struct capref vspace,
                  struct capref dispframe, bool run)
{
    uint8_t root_vbits = get_cap_valid_bits(cspace);
    capaddr_t root_caddr = get_cap_addr(cspace) >> (CPTR_BITS - root_vbits);
    capaddr_t vtree_caddr = get_cap_addr(vspace);
    capaddr_t disp_caddr = get_cap_addr(dispframe);
    capaddr_t dd_caddr = get_cap_addr(domdispatcher);
    uint8_t invoke_bits = get_cap_valid_bits(dispatcher);
    capaddr_t invoke_cptr = get_cap_addr(dispatcher) >> (CPTR_BITS - invoke_bits);

    assert(root_vbits <= 0xff);

    return syscall7((invoke_bits << 16) | (DispatcherCmd_Setup << 8) | SYSCALL_INVOKE,
                    invoke_cptr, dd_caddr, root_caddr,
                    (run << 8) | (root_vbits & 0xff), vtree_caddr,
                    disp_caddr).error;
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

static inline errval_t invoke_irqtable_alloc_vector(struct capref irqcap, int *retirq)
{
    uint8_t invoke_bits = get_cap_valid_bits(irqcap);
    capaddr_t invoke_cptr = get_cap_addr(irqcap) >> (CPTR_BITS - invoke_bits);

    struct sysret ret = syscall2(
                    (invoke_bits << 16) | (IRQTableCmd_Alloc << 8) | SYSCALL_INVOKE,
                    invoke_cptr);
    if (err_is_ok(ret.error)) {
        *retirq = ret.value;
    } else {
        *retirq = 0;
    }
    return ret.error;
}

static inline errval_t invoke_irqtable_set(struct capref irqcap, int irq,
                                           struct capref ep)
{
    uint8_t invoke_bits = get_cap_valid_bits(irqcap);
    capaddr_t invoke_cptr = get_cap_addr(irqcap) >> (CPTR_BITS - invoke_bits);

    return syscall4((invoke_bits << 16) | (IRQTableCmd_Set << 8) | SYSCALL_INVOKE,
                    invoke_cptr, irq, get_cap_addr(ep)).error;
}

static inline errval_t invoke_irqtable_delete(struct capref irqcap, int irq)
{
    uint8_t invoke_bits = get_cap_valid_bits(irqcap);
    capaddr_t invoke_cptr = get_cap_addr(irqcap) >> (CPTR_BITS - invoke_bits);

    return syscall3((invoke_bits << 16) | (IRQTableCmd_Delete << 8) | SYSCALL_INVOKE,
                    invoke_cptr, irq).error;
}

static inline errval_t invoke_kernel_get_core_id(struct capref kern_cap,
                                                 coreid_t *core_id)
{
    assert(core_id != NULL);

    uint8_t invoke_bits = get_cap_valid_bits(kern_cap);
    capaddr_t invoke_cptr = get_cap_addr(kern_cap) >> (CPTR_BITS - invoke_bits);

    struct sysret sysret =
        syscall2((invoke_bits << 16) | (KernelCmd_Get_core_id << 8) | SYSCALL_INVOKE,
                 invoke_cptr);

    if (sysret.error == SYS_ERR_OK) {
        *core_id = sysret.value;
    }

    return sysret.error;
}

static inline errval_t invoke_dispatcher_dump_ptables(struct capref dispcap)
{
    uint8_t invoke_bits = get_cap_valid_bits(dispcap);
    capaddr_t invoke_cptr = get_cap_addr(dispcap) >> (CPTR_BITS - invoke_bits);

    return syscall2((invoke_bits << 16) | (DispatcherCmd_DumpPTables << 8) |
            SYSCALL_INVOKE, invoke_cptr).error;
}

static inline errval_t
invoke_dispatcher_properties(
    struct capref dispatcher,
    enum task_type type, unsigned long deadline,
    unsigned long wcet, unsigned long period,
    unsigned long release, unsigned short weight
                            )
{
    uint8_t invoke_bits = get_cap_valid_bits(dispatcher);
    capaddr_t invoke_cptr = get_cap_addr(dispatcher) >> (CPTR_BITS - invoke_bits);

    if (weight > 0xffff)
    {
        weight = 0xffff;
    }

    return syscall7((invoke_bits << 16) | (DispatcherCmd_Properties << 8) | SYSCALL_INVOKE,
                    invoke_cptr,
                    (type << 16) | weight,
                    deadline, wcet, period, release).error;
}

static inline errval_t
invoke_idcap_identify(
    struct capref idcap,
    idcap_id_t *id
                      )
{
    assert(id != NULL);

    uint8_t invoke_bits = get_cap_valid_bits(idcap);
    capaddr_t invoke_cptr = get_cap_addr(idcap) >> (CPTR_BITS - invoke_bits);

    // user-space pointer 'id' is directly written to by kernel.
    struct sysret sysret =
        syscall3((invoke_bits << 16) | (IDCmd_Identify << 8) | SYSCALL_INVOKE,
                 invoke_cptr, (uintptr_t) id);

    return sysret.error;
}

static inline errval_t invoke_get_global_paddr(struct capref kernel_cap, genpaddr_t* global)
{
    uint8_t invoke_bits = get_cap_valid_bits(kernel_cap);
    capaddr_t invoke_cptr = get_cap_addr(kernel_cap) >> (CPTR_BITS - invoke_bits);
    uintptr_t invocation = (invoke_bits << 16)
                         | (KernelCmd_GetGlobalPhys << 8)
                         | SYSCALL_INVOKE;
    struct sysret sysret = syscall2(invocation, invoke_cptr);
    if (err_is_ok(sysret.error)) {
        *global = sysret.value;
    }

    return sysret.error;
}

#endif
