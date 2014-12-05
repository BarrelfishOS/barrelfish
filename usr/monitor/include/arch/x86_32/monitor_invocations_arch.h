/**
 * \file
 * \brief Capability invocations specific to the monitors
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_INVOCATIONS_ARCH_H
#define MONITOR_INVOCATIONS_ARCH_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish/caddr.h>
#include <barrelfish/invocations_arch.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/syscall_overflows_arch.h>
#include <barrelfish_kpi/syscalls.h>
 
static inline errval_t
invoke_monitor_remote_relations(capaddr_t root_cap, int root_bits,
                                capaddr_t cap, int bits,
                                uint8_t relations, uint8_t mask,
                                uint8_t *ret_remote_relations)
{
    struct sysret r = cap_invoke6(cap_kernel, KernelCmd_Remote_relations,
                                  root_cap, root_bits, cap, bits,
                                  ((uint16_t)relations) | (((uint16_t)mask)<<8));
    if (err_is_ok(r.error) && ret_remote_relations) {
        *ret_remote_relations = r.value;
    }
    return r.error;
}

static inline errval_t
invoke_monitor_cap_has_relations(capaddr_t caddr, uint8_t bits, uint8_t mask,
                                 uint8_t *res)
{
    assert(res);
    struct sysret ret = cap_invoke4(cap_kernel, KernelCmd_Cap_has_relations,
                                    caddr, bits, mask);
    if (err_is_ok(ret.error)) {
        *res = ret.value;
    }
    return ret.error;
}

static inline errval_t
invoke_monitor_identify_cap(capaddr_t cap, int bits, struct capability *out)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall5((invoke_bits << 16) | (KernelCmd_Identify_cap << 8)
                    | SYSCALL_INVOKE, invoke_cptr, cap, bits,
                    (uintptr_t)out).error;
}

static inline errval_t 
invoke_monitor_identify_domains_cap(capaddr_t root_cap, int root_bits,
                                    capaddr_t cap, int bits,
                                    struct capability *out)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall7((invoke_bits << 16) | (KernelCmd_Identify_domains_cap << 8)
                    | SYSCALL_INVOKE, invoke_cptr, root_cap, root_bits,
                    cap, bits, (uintptr_t)out).error;
}

static inline errval_t
invoke_monitor_nullify_cap(capaddr_t cap, int bits)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall4((invoke_bits << 16) | (KernelCmd_Nullify_cap << 8)
                    | SYSCALL_INVOKE, invoke_cptr, cap, bits).error;
}

static inline errval_t
invoke_monitor_create_cap(uint64_t *raw, capaddr_t caddr, int bits,
                          capaddr_t slot, coreid_t owner)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall7((invoke_bits << 16) | (KernelCmd_Create_cap << 8)
                    | SYSCALL_INVOKE, invoke_cptr, caddr, bits, slot, owner,
                    (uintptr_t)raw).error;
}

static inline errval_t
invoke_monitor_register(struct capref ep)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall3((invoke_bits << 16) | (KernelCmd_Register << 8)
                    | SYSCALL_INVOKE, invoke_cptr, get_cap_addr(ep)).error;
}

/**
 * \brief Set up tracing in the kernel
 *
 */
static inline errval_t
invoke_trace_setup(struct capref cap)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
#if 0
    struct idc_send_msg msg;
    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Setup_trace);
    idc_msg_encode_word(&msg, get_cap_addr(cap));
    return cap_invoke(cap_kernel, &msg);
#endif
}

static inline errval_t
invoke_domain_id(struct capref cap, domainid_t domain_id)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
#if 0
    struct idc_send_msg msg;
    idc_msg_init(&msg);
    idc_msg_encode_word(&msg, KernelCmd_Domain_Id);
    idc_msg_encode_word(&msg, get_cap_addr(cap));
    idc_msg_encode_word(&msg, domain_id);
    return cap_invoke(cap_kernel, &msg);
#endif
}

static inline errval_t
invoke_monitor_ipi_register(struct capref ep, int chanid)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall4((invoke_bits << 16) | (KernelCmd_IPI_Register << 8)
                    | SYSCALL_INVOKE, invoke_cptr,
                    get_cap_addr(ep),
                    chanid).error;
}

static inline errval_t
invoke_monitor_ipi_delete(int chanid)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall3((invoke_bits << 16) | (KernelCmd_IPI_Delete << 8)
                    | SYSCALL_INVOKE, invoke_cptr,
                    chanid).error;
}

static inline errval_t
invoke_monitor_get_arch_id(uintptr_t *arch_id)
{
    assert(arch_id != NULL);

    struct sysret sysret = cap_invoke1(cap_kernel, KernelCmd_Get_arch_id);
    if (sysret.error == SYS_ERR_OK) {
        *arch_id = sysret.value;
    }
    return sysret.error;
}

static inline errval_t invoke_monitor_sync_timer(uint64_t synctime)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall4((invoke_bits << 16) | (KernelCmd_Sync_timer << 8)
                    | SYSCALL_INVOKE, invoke_cptr, synctime >> 32,
                    synctime & 0xffffffff).error;
}

static inline errval_t
invoke_monitor_add_kcb(uintptr_t kcb_base)
{
    assert(kcb_base);
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall3((invoke_bits << 16) | (KernelCmd_Add_kcb << 8) | SYSCALL_INVOKE,
                    invoke_cptr,
                    kcb_base).error;
}

static inline errval_t
invoke_monitor_remove_kcb(uintptr_t kcb_base)
{
    assert(kcb_base);

    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall3((invoke_bits << 16) | (KernelCmd_Remove_kcb << 8) | SYSCALL_INVOKE,
                    invoke_cptr,
                    kcb_base).error;
}

static inline errval_t
invoke_monitor_suspend_kcb_scheduler(bool suspend)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall3((invoke_bits << 16) | (KernelCmd_Suspend_kcb_sched << 8) | SYSCALL_INVOKE,
                    invoke_cptr,
                    suspend).error;
}

#ifdef __scc__
static inline errval_t invoke_monitor_spawn_scc_core(uint8_t id,
                                                     genpaddr_t urpcframe_base,
                                                     uint8_t urpcframe_bits,
                                                     int chanid)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    return syscall4((invoke_bits << 16) | (KernelCmd_Spawn_SCC_Core << 8)
                    | SYSCALL_INVOKE, invoke_cptr, urpcframe_base,
                    (id << 24) | (urpcframe_bits << 16) | (chanid & 0xffff)).error;
}
#endif

static inline errval_t
invoke_monitor_remote_cap_retype(capaddr_t rootcap_addr, uint8_t rootcap_vbits,
                                 capaddr_t src, enum objtype newtype,
                                 int objbits, capaddr_t to, capaddr_t slot,
                                 int bits)
{
    uint8_t invoke_bits = get_cap_valid_bits(cap_kernel);
    capaddr_t invoke_cptr = get_cap_addr(cap_kernel) >> (CPTR_BITS - invoke_bits);

    assert(newtype <= 0xffff);
    assert(objbits <= 0xff);
    assert(bits <= 0xff);
    assert(rootcap_vbits <= 0xff);

    struct remote_retype_syscall_overflow rootcap_struct = {
        .rootcap_addr = rootcap_addr,
        .rootcap_vbits = rootcap_vbits,
    };

    // arguments 2-5 must match the deserialisation in
    // kernel/arch/x86_32/handle_retype_common.
    return syscall7((invoke_bits << 16) | (KernelCmd_Retype << 8) | SYSCALL_INVOKE,
            invoke_cptr, (uintptr_t)&rootcap_struct, src,
            (newtype << 16) | (objbits << 8) | bits, to, slot).error;
}

static inline errval_t
invoke_monitor_copy_existing(uint64_t *raw, capaddr_t cn_addr, int cn_bits, cslot_t slot)
{
    // XXX: this is assumed in client code of this function!
    assert(sizeof(struct capability) <= 4*sizeof(uint64_t));

    return cap_invoke5(cap_kernel, KernelCmd_Copy_existing,
                       cn_addr, cn_bits, slot, (uintptr_t)raw).error;
}

static inline errval_t
invoke_monitor_get_cap_owner(capaddr_t root, int rbits, capaddr_t cap, int cbits, coreid_t *ret_owner)
{
    struct sysret sysret = cap_invoke5(cap_kernel, KernelCmd_Get_cap_owner,
                                       root, rbits, cap, cbits);
    if (err_is_ok(sysret.error)) {
        *ret_owner = sysret.value;
    }
    return sysret.error;
}

static inline errval_t
invoke_monitor_set_cap_owner(capaddr_t root, int rbits, capaddr_t cap, int cbits, coreid_t owner)
{
    return cap_invoke6(cap_kernel, KernelCmd_Set_cap_owner, root, rbits, cap, cbits, owner).error;
}

static inline errval_t
invoke_monitor_lock_cap(capaddr_t root, int rbits, capaddr_t cap, int cbits)
{
    return cap_invoke5(cap_kernel, KernelCmd_Lock_cap, root, rbits, cap, cbits).error;
}

static inline errval_t
invoke_monitor_unlock_cap(capaddr_t root, int rbits, capaddr_t cap, int cbits)
{
    return cap_invoke5(cap_kernel, KernelCmd_Unlock_cap, root, rbits, cap, cbits).error;
}

static inline errval_t
invoke_monitor_delete_last(capaddr_t root, int rbits, capaddr_t cap, int cbits,
                           capaddr_t retcn, int retcnbits, cslot_t retslot)
{
    assert(rbits <= 0xff);
    assert(cbits <= 0xff);
    assert(retcnbits <= 0xff);

    return cap_invoke6(cap_kernel, KernelCmd_Delete_last, root, cap,
                       retcn, retslot, ((cbits<<16)|(rbits<<8)|retcnbits)).error;
}

static inline errval_t
invoke_monitor_delete_foreigns(capaddr_t cap, int bits)
{
    return cap_invoke3(cap_kernel, KernelCmd_Delete_foreigns, cap, bits).error;
}

static inline errval_t
invoke_monitor_revoke_mark_target(capaddr_t root, int rbits,
                                  capaddr_t cap, int cbits)
{
    return cap_invoke5(cap_kernel, KernelCmd_Revoke_mark_target,
                       root, rbits, cap, cbits).error;
}

static inline errval_t
invoke_monitor_revoke_mark_relations(uint64_t *raw_base)
{
    // XXX: this is assumed in client code of this function!
    assert(sizeof(struct capability) / sizeof(uint64_t) <= 4);
    return cap_invoke2(cap_kernel, KernelCmd_Revoke_mark_relations,
                       (uintptr_t)raw_base).error;
}

static inline errval_t
invoke_monitor_delete_step(capaddr_t retcn, int retcnbits, cslot_t retslot)
{
    return cap_invoke4(cap_kernel, KernelCmd_Delete_step,
                       retcn, retcnbits, retslot).error;
}

static inline errval_t
invoke_monitor_clear_step(capaddr_t retcn, int retcnbits, cslot_t retslot)
{
    return cap_invoke4(cap_kernel, KernelCmd_Clear_step,
                       retcn, retcnbits, retslot).error;
}

static inline errval_t
invoke_monitor_has_descendants(uint64_t *raw, bool *res)
{
    // XXX: this is assumed in client code of this function!
    assert(sizeof(struct capability) / sizeof(uint64_t) <= 4);

    struct sysret sysret;
    sysret = cap_invoke2(cap_kernel, KernelCmd_Has_descendants,
                         (uintptr_t)raw);
    if (err_is_ok(sysret.error)) {
        *res = sysret.value;
    }
    return sysret.error;
}

#endif
