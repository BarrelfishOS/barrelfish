/**
 * \file
 * \brief Low-level capability invocations
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish_kpi/legacy_idc_buffer.h> // for idc_send_msg
#include <barrelfish/syscall_arch.h> // for sys_invoke and cap_invoke


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
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, CNodeCmd_Retype);
    idc_msg_encode_word(&msg, cap);
    idc_msg_encode_word(&msg, newtype);
    idc_msg_encode_word(&msg, objbits);
    idc_msg_encode_word(&msg, to);
    idc_msg_encode_word(&msg, slot);
    idc_msg_encode_word(&msg, bits);

    return cap_invoke(root, &msg).error;
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
static inline errval_t invoke_cnode_mint(struct capref root, capaddr_t to,
                                         capaddr_t slot, capaddr_t from, int tobits,
                                         int frombits, uint64_t param1,
                                         uint64_t param2)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, CNodeCmd_Mint);
    idc_msg_encode_word(&msg, to);
    idc_msg_encode_word(&msg, slot);
    idc_msg_encode_word(&msg, from);
    idc_msg_encode_word(&msg, tobits);
    idc_msg_encode_word(&msg, frombits);
    idc_msg_encode_word(&msg, param1);
    idc_msg_encode_word(&msg, param2);

    return cap_invoke(root, &msg).error;
}

// Required to compile
static inline errval_t invoke_vnode_unmap(struct capref cap, size_t entry)
{
    assert(!"should not be called");
    return LIB_ERR_NOT_IMPLEMENTED;
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
static inline errval_t invoke_cnode_copy(struct capref root, capaddr_t to,
                                         capaddr_t slot, capaddr_t from, int tobits,
                                         int frombits)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, CNodeCmd_Copy);
    idc_msg_encode_word(&msg, to);
    idc_msg_encode_word(&msg, slot);
    idc_msg_encode_word(&msg, from);
    idc_msg_encode_word(&msg, tobits);
    idc_msg_encode_word(&msg, frombits);

    return cap_invoke(root, &msg).error;
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
static inline errval_t invoke_cnode_delete(struct capref root, capaddr_t cap,
                                           int bits)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, CNodeCmd_Delete);
    idc_msg_encode_word(&msg, cap);
    idc_msg_encode_word(&msg, bits);

    return cap_invoke(root, &msg).error;
}

static inline errval_t invoke_cnode_revoke(struct capref root, capaddr_t cap,
                                           int bits)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, CNodeCmd_Revoke);
    idc_msg_encode_word(&msg, cap);
    idc_msg_encode_word(&msg, bits);

    return cap_invoke(root, &msg).error;
}

/**
 * \brief Return the physical address and size of a frame capability
 *
 * \param frame    CSpace address of frame capability
 * \param ret      frame_identity struct filled in with relevant data
 *
 * \return Error code
 */
static inline errval_t invoke_frame_identify(struct capref frame,
                                             struct frame_identity *ret)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, FrameCmd_Identify);
    struct sysret sysret = cap_invoke(frame, &msg);
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

static inline errval_t invoke_iocap_in(struct capref iocap, enum io_cmd cmd,
                                       uint16_t port, uint32_t *data)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, cmd);
    idc_msg_encode_word(&msg, port);
    struct sysret sysret = cap_invoke(iocap, &msg);
    if (err_is_ok(sysret.error)) {
        assert(data != NULL);
        *data = sysret.value;
    }
    return sysret.error;
}

static inline errval_t invoke_iocap_out(struct capref iocap, enum io_cmd cmd,
                                        uint16_t port, uint32_t data)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, cmd);
    idc_msg_encode_word(&msg, port);
    idc_msg_encode_word(&msg, data);
    return cap_invoke(iocap, &msg).error;
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

    struct idc_send_msg msg;
    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, DispatcherCmd_Setup);
    idc_msg_encode_word(&msg, root_caddr);
    idc_msg_encode_word(&msg, root_vbits);
    idc_msg_encode_word(&msg, vtree_caddr);
    idc_msg_encode_word(&msg, disp_caddr);
    idc_msg_encode_word(&msg, run);
    idc_msg_encode_word(&msg, dd_caddr);
    return cap_invoke(dispatcher, &msg).error;
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
    struct idc_send_msg msg;
    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, DispatcherCmd_SetupGuest);
    idc_msg_encode_word(&msg, ep_cap);
    idc_msg_encode_word(&msg, vnode);
    idc_msg_encode_word(&msg, vmkit_guest);
    idc_msg_encode_word(&msg, guest_control_cap);
    return cap_invoke(dispatcher, &msg).error;
}

static inline errval_t invoke_irqtable_set(struct capref irqcap, int irq,
                                           struct capref ep)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, IRQTableCmd_Set);
    idc_msg_encode_word(&msg, irq);
    idc_msg_encode_word(&msg, get_cap_addr(ep));
    return cap_invoke(irqcap, &msg).error;
}

static inline errval_t invoke_irqtable_delete(struct capref irqcap, int irq)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, IRQTableCmd_Delete);
    idc_msg_encode_word(&msg, irq);
    return cap_invoke(irqcap, &msg).error;
}

// XXX TODO: Clean this with proper symbols
static inline errval_t invoke_kernel_get_core_id(struct capref kern_cap,
                                                 coreid_t *core_id)
{
    assert(core_id != 0);
    volatile unsigned int *ptr = (void*)0x02;
    unsigned int val = *ptr;
    *core_id = ((val >> 10) & 0xf);
    return SYS_ERR_OK;
}

static inline errval_t
invoke_dispatcher_properties(struct capref dispatcher,
                             enum task_type type, unsigned long deadline,
                             unsigned long wcet, unsigned long period,
                             unsigned long release, unsigned short weight)
{
    return LIB_ERR_NOT_IMPLEMENTED;
}
