/*
 * Copyright (c) 2009,2011,2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>

#include <barrelfish_kpi/lmp.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/sys_debug.h>
#include <barrelfish_kpi/platform.h>
#include <mdb/mdb_tree.h>

#include <arm_hal.h>
#include <irq.h>

#include <paging_kernel_arch.h>
#include <dispatch.h>
#include <exec.h>
#include <stdio.h>
#include <sys_debug.h>
#include <syscall.h>
#include <start_aps.h>
#include <useraccess.h>
#include <systime.h>
#include <psci.h>
#include <arch/arm/gic.h>
#include <arch/arm/platform.h>
#include <arch/arm/syscall_arm.h>

// helper macros  for invocation handler definitions
#define INVOCATION_HANDLER(func) \
static struct sysret \
func( \
    struct capability *kernel_cap, \
    arch_registers_state_t* context, \
    int argc \
    )

#define INVOCATION_PRELUDE(n) \
    assert(n == argc); \
    struct registers_aarch64_syscall_args* sa = &context->syscall_args

#define NYI(str) printf("armv8: %s\n", str)

__attribute__((noreturn)) void sys_syscall_kernel(void);
__attribute__((noreturn))
void sys_syscall(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                 uint64_t a4, uint64_t a5, uint64_t a6,
                 arch_registers_state_t *context);

__attribute__((noreturn))
void sys_syscall_kernel(void)
{
    panic("Why is the kernel making a system call?");
}

static struct sysret
handle_dispatcher_setup(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(8 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    capaddr_t root  = sa->arg2;
    uint8_t   level = sa->arg3;
    capaddr_t vptr  = sa->arg4;
    capaddr_t dptr  = sa->arg5;
    bool      run   = sa->arg6;
    capaddr_t odptr = sa->arg7;

    return sys_dispatcher_setup(to, root, level, vptr, dptr, run, odptr);
}

static struct sysret
handle_dispatcher_properties(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(8 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    enum task_type type = (enum task_type)(sa->arg3 >> 16);
    uint16_t weight = sa->arg3 & 0xffff;

    return sys_dispatcher_properties(to, type, sa->arg4,
                                     sa->arg5, sa->arg6, sa->arg7, weight);
}

static struct sysret
handle_dispatcher_perfmon(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    /* XXX - implement this? */
    return SYSRET(SYS_ERR_PERFMON_NOT_AVAILABLE);
}

static struct sysret copy_or_mint(struct capability *root,
                                  struct registers_aarch64_syscall_args* args,
                                  bool mint)
{
    /* Retrieve arguments */
    capaddr_t dest_cspace_cptr = args->arg2;
    capaddr_t destcn_cptr      = args->arg3;
    uint64_t  dest_slot        = args->arg4;
    capaddr_t source_croot_ptr = args->arg5;
    capaddr_t source_cptr      = args->arg6;
    uint8_t destcn_level       = args->arg7;
    uint8_t source_level       = args->x8;
    uint64_t param1, param2;
    // params only sent if mint operation
    if (mint) {
        param1 = args->x9;
        param2 = args->x10;
    } else {
        param1 = param2 = 0;
    }

    struct sysret sr = sys_copy_or_mint(root, dest_cspace_cptr, destcn_cptr, dest_slot,
                                        source_croot_ptr, source_cptr,
                                        destcn_level, source_level,
                                        param1, param2, mint);
    return sr;
}

static struct sysret
handle_mint(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(11 == argc);

    return copy_or_mint(root, &context->syscall_args, true);
}

static struct sysret
handle_copy(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(9 == argc);

    return copy_or_mint(root, &context->syscall_args, false);
}

static struct sysret
handle_retype_common(
    struct capability* root,
    bool from_monitor,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(11 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    // Source capability cptr
    capaddr_t source_croot     = sa->arg2;
    capaddr_t source_cptr      = sa->arg3;
    gensize_t offset           = sa->arg4;
    uint32_t word              = sa->arg5;
    // Type to retype to
    enum objtype type          = word & 0xFFFF;
    assert(type < ObjType_Num);
    // Object size for variable-sized types
    gensize_t objsize          = sa->arg6;
    // number of new objects
    size_t count               = sa->arg7;
    // Destination cspace cptr
    capaddr_t dest_cspace_cptr = sa->x8;
    // Destination cnode cptr
    capaddr_t dest_cnode_cptr  = sa->x9;
    // Destination slot number
    capaddr_t dest_slot        = sa->x10;
    // Level of destination cnode in destination cspace
    uint8_t dest_cnode_level   = (word >> 16) & 0xF;

    return sys_retype(root, source_croot, source_cptr, offset, type,
                      objsize, count, dest_cspace_cptr, dest_cnode_cptr,
                      dest_cnode_level, dest_slot, from_monitor);
}

static struct sysret
handle_retype(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    return handle_retype_common(root, false, context, argc);
}

static struct sysret
handle_delete(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(4 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    capaddr_t cptr = (capaddr_t)sa->arg2;
    int     bits = (int)sa->arg3;

    return sys_delete(root, cptr, bits);
}

static struct sysret
handle_create(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(7 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    enum objtype type      = sa->arg2;
    size_t       objsize   = sa->arg3;
    capaddr_t    dest_cptr = sa->arg4;
    uint8_t      dest_level= sa->arg5;
    cslot_t      dest_slot = sa->arg6;
    printk(LOG_NOTE, "type = %d, bytes = %zu\n", type, objsize);

    return sys_create(root, type, objsize, dest_cptr, dest_level, dest_slot);
}

static struct sysret
handle_revoke(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(4 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    capaddr_t cptr = (capaddr_t)sa->arg2;
    int     bits = (int)sa->arg3;

    return sys_revoke(root, cptr, bits);
}

static struct sysret
handle_get_state(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(4 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    capaddr_t cptr = (capaddr_t)sa->arg2;
    int       bits = (int)sa->arg3;

    return sys_get_state(root, cptr, bits);
}

static struct sysret
handle_get_size(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(2 == argc);
    return sys_get_size_l1cnode(root);
}

static struct sysret
handle_resize(
    struct capability* root,
    arch_registers_state_t* context,
    int argc
    )
{
    INVOCATION_PRELUDE(5);

    capaddr_t newroot_ptr = sa->arg2;
    capaddr_t retcn_ptr   = sa->arg3;
    cslot_t   retslot     = sa->arg4;

    return sys_resize_l1cnode(root, newroot_ptr, retcn_ptr, retslot);
}

static struct sysret
handle_map(
    struct capability *ptable,
    arch_registers_state_t *context,
    int argc
    )
{
    assert(10 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    /* Retrieve arguments */
    capaddr_t source_root_cptr = (capaddr_t)sa->arg2;
    capaddr_t source_cptr      = (capaddr_t)sa->arg3;
    uintptr_t flags            = (uintptr_t)sa->arg4;
    uintptr_t offset           = (uintptr_t)sa->arg5;
    uintptr_t pte_count        = (uintptr_t)sa->arg6;
    capaddr_t mcn_root         = (capaddr_t)sa->arg7;
    capaddr_t mcn_addr         = (capaddr_t)sa->x8;
    uint32_t  word             = sa->x9;
    uint8_t   source_level     = word & 0xF;
    uint8_t   mcn_level        = (word >> 4) & 0xF;
    cslot_t   mapping_slot     = (word >> 8) & 0xFF;
    cslot_t   slot             = (word >> 16) & 0xFFFF;

    return sys_map(ptable, slot, source_root_cptr, source_cptr, source_level,
                   flags, offset, pte_count, mcn_root, mcn_addr, mcn_level,
                   mapping_slot);
}

static struct sysret
handle_unmap(
    struct capability* ptable,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(4 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    /* Retrieve arguments */
    capaddr_t  mapping_cptr  = (capaddr_t)sa->arg2;
    int mapping_bits         = (int)sa->arg3 & 0xff;

    errval_t err;
    struct cte *mapping = NULL;
    err = caps_lookup_slot(&dcb_current->cspace.cap, mapping_cptr, mapping_bits,
                           &mapping, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        printk(LOG_NOTE, "%s: caps_lookup_slot: %ld\n", __FUNCTION__, err);
        return SYSRET(err_push(err, SYS_ERR_CAP_NOT_FOUND));
    }

    err = page_mappings_unmap(ptable, mapping);
    if (err_is_fail(err)) {
        printk(LOG_NOTE, "%s: page_mappings_unmap: %ld\n", __FUNCTION__, err);
    }
    return SYSRET(err);
}

static struct sysret
handle_mapping_destroy(
        struct capability *to,
        arch_registers_state_t *context,
        int argc)
{
    panic("NYI!");
    return SYSRET(SYS_ERR_OK);
}

static struct sysret
handle_mapping_modify(
        struct capability *to,
        arch_registers_state_t *context,
        int argc
        )
{
    assert(6 == argc);
    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    // Modify flags of (part of) mapped region of frame
    assert(type_is_mapping(to->type));

    // unpack arguments
    size_t offset = sa->arg2; // in pages; of first page to modify from first
                             // page in mapped region
    size_t pages  = sa->arg3; // #pages to modify
    size_t flags  = sa->arg4; // new flags

    errval_t err = paging_modify_flags(to, offset, pages, flags);

    return (struct sysret) {
        .error = err,
        .value = 0,
    };
}

/// Different handler for cap operations performed by the monitor
INVOCATION_HANDLER(monitor_handle_retype)
{
    assert(argc == 11);
    return handle_retype_common(&dcb_current->cspace.cap, true, context, argc);
}

INVOCATION_HANDLER(monitor_handle_has_descendants)
{
    INVOCATION_PRELUDE(3);
    // check access to user pointer
    if (!access_ok(ACCESS_READ, sa->arg2, sizeof(struct capability))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }

    struct capability *src = (struct capability *)sa->arg2;

    struct cte *next = mdb_find_greater(src, false);

    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = (next && is_ancestor(&next->cap, src)),
    };
}

INVOCATION_HANDLER(monitor_handle_is_retypeable)
{
    INVOCATION_PRELUDE(6);
    // check access to user pointer
    if (!access_ok(ACCESS_READ, sa->arg2, sizeof(struct capability))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }

    struct capability *src = (struct capability *)sa->arg2;

    uintptr_t offset  = sa->arg3;
    uintptr_t objsize = sa->arg4;
    uintptr_t count   = sa->arg5;

    return sys_monitor_is_retypeable(src, offset, objsize, count);
}

INVOCATION_HANDLER(monitor_handle_delete_last)
{
    INVOCATION_PRELUDE(9);
    capaddr_t root_caddr   = sa->arg2;
    uint8_t   root_level   = sa->arg3;
    capaddr_t target_caddr = sa->arg4;
    uint8_t   target_level = sa->arg5;
    capaddr_t retcn_caddr  = sa->arg6;
    uint8_t retcn_level    = sa->arg7;
    cslot_t retcn_slot     = sa->x8;

    return sys_monitor_delete_last(root_caddr, root_level, target_caddr,
                                   target_level, retcn_caddr, retcn_level, retcn_slot);
}

INVOCATION_HANDLER(monitor_handle_delete_foreigns)
{
    INVOCATION_PRELUDE(4);
    capaddr_t caddr = sa->arg2;
    uint8_t bits = sa->arg3;
    return sys_monitor_delete_foreigns(caddr, bits);
}

INVOCATION_HANDLER(monitor_handle_revoke_mark_tgt)
{
    INVOCATION_PRELUDE(6);
    capaddr_t root_caddr   = sa->arg2;
    uint8_t root_vbits     = sa->arg3;
    capaddr_t target_caddr = sa->arg4;
    uint8_t target_vbits   = sa->arg5;

    return sys_monitor_revoke_mark_tgt(root_caddr, root_vbits,
                                       target_caddr, target_vbits);
}

INVOCATION_HANDLER(monitor_handle_revoke_mark_rels)
{
    INVOCATION_PRELUDE(3);
    // user pointer to src cap, check access
    if (!access_ok(ACCESS_READ, sa->arg2, sizeof(struct capability))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }
    struct capability *base = (struct capability*)sa->arg2;

    return sys_monitor_revoke_mark_rels(base);
}

INVOCATION_HANDLER(monitor_handle_delete_step)
{
    INVOCATION_PRELUDE(5);
    capaddr_t ret_cn_addr = sa->arg2;
    capaddr_t ret_cn_bits = sa->arg3;
    capaddr_t ret_slot    = sa->arg4;

    return sys_monitor_delete_step(ret_cn_addr, ret_cn_bits, ret_slot);
}

INVOCATION_HANDLER(monitor_handle_clear_step)
{
    INVOCATION_PRELUDE(5);
    capaddr_t ret_cn_addr = sa->arg2;
    capaddr_t ret_cn_bits = sa->arg3;
    capaddr_t ret_slot    = sa->arg4;

    return sys_monitor_clear_step(ret_cn_addr, ret_cn_bits, ret_slot);
}


static struct sysret
monitor_get_core_id(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(2 == argc);

    return (struct sysret) { .error = SYS_ERR_OK, .value = my_core_id };
}

static struct sysret
monitor_get_arch_id(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(2 == argc);

    // TODO: ARM doesn't support multicore yet...
    return (struct sysret) { .error = SYS_ERR_OK, .value = my_core_id };
}

INVOCATION_HANDLER(monitor_handle_domain_id)
{
    INVOCATION_PRELUDE(4);
    capaddr_t cptr       = sa->arg2;
    domainid_t domain_id = sa->arg3;

    return sys_monitor_domain_id(cptr, domain_id);
}

INVOCATION_HANDLER(monitor_get_cap_owner)
{
    INVOCATION_PRELUDE(6);
    capaddr_t root_addr = sa->arg2;
    uint8_t root_bits   = sa->arg3;
    capaddr_t cptr      = sa->arg4;
    uint8_t bits        = sa->arg5;

    return sys_get_cap_owner(root_addr, root_bits, cptr, bits);
}

INVOCATION_HANDLER(monitor_set_cap_owner)
{
    INVOCATION_PRELUDE(7);
    capaddr_t root_addr = sa->arg2;
    uint8_t root_bits   = sa->arg3;
    capaddr_t cptr      = sa->arg4;
    uint8_t bits        = sa->arg5;
    coreid_t owner      = sa->arg6;

    return sys_set_cap_owner(root_addr, root_bits, cptr, bits, owner);
}

INVOCATION_HANDLER(monitor_lock_cap)
{
    INVOCATION_PRELUDE(6);
    capaddr_t root_addr = sa->arg2;
    uint8_t root_bits   = sa->arg3;
    capaddr_t cptr      = sa->arg4;
    uint8_t bits        = sa->arg5;

    return sys_lock_cap(root_addr, root_bits, cptr, bits);
}

INVOCATION_HANDLER(monitor_unlock_cap)
{
    INVOCATION_PRELUDE(6);
    capaddr_t root_addr = sa->arg2;
    uint8_t root_bits   = sa->arg3;
    capaddr_t cptr      = sa->arg4;
    uint8_t bits        = sa->arg5;

    return sys_unlock_cap(root_addr, root_bits, cptr, bits);
}

static struct sysret
monitor_handle_register(
    struct capability* to,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(3 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    capaddr_t ep_caddr = (capaddr_t)sa->arg2;

    return sys_monitor_register(ep_caddr);
}

INVOCATION_HANDLER(monitor_cap_has_relations)
{
    INVOCATION_PRELUDE(5);
    capaddr_t caddr = sa->arg2;
    uint8_t vbits = sa->arg3;
    uint8_t mask = sa->arg4;

    return sys_cap_has_relations(caddr, vbits, mask);
}

INVOCATION_HANDLER(monitor_remote_relations)
{
    INVOCATION_PRELUDE(7);
    capaddr_t root_addr = sa->arg2;
    int root_bits       = sa->arg3;
    capaddr_t cptr      = sa->arg4;
    int bits            = sa->arg5;
    uint8_t relations   = sa->arg6 & 0xFF;
    uint8_t mask        = (sa->arg6 >> 8) & 0xFF;

    return sys_monitor_remote_relations(root_addr, root_bits, cptr, bits,
                                        relations, mask);
}

INVOCATION_HANDLER(monitor_copy_existing)
{
    INVOCATION_PRELUDE(7);
    capaddr_t croot_cptr = sa->arg2;
    capaddr_t cnode_cptr = sa->arg3;
    int cnode_level      = sa->arg4;
    size_t slot          = sa->arg5;

    // user pointer to src cap, check access
    if (!access_ok(ACCESS_READ, sa->arg6, sizeof(struct capability))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }
    /* Get the raw metadata of the capability to create from user pointer */
    struct capability *src = (struct capability *)sa->arg6;

    return sys_monitor_copy_existing(src, croot_cptr, cnode_cptr, cnode_level, slot);
}

INVOCATION_HANDLER(monitor_nullify_cap)
{
    INVOCATION_PRELUDE(4);
    capaddr_t cptr = sa->arg2;
    int bits       = sa->arg3;

    return sys_monitor_nullify_cap(cptr, bits);
}

static struct sysret
monitor_create_cap(
    struct capability *kernel_cap,
    arch_registers_state_t* context,
    int argc
    )
{
    assert(7 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    /* Create the cap in the destination */
    capaddr_t cnode_cptr = sa->arg2;
    int cnode_vbits      = sa->arg3;
    size_t slot          = sa->arg4;
    coreid_t owner       = sa->arg5;
    struct capability *src =
        (struct capability*)sa->arg6;

    /* Cannot create null caps */
    if (src->type == ObjType_Null ) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    /* For certain types, only foreign copies can be created here */
    if ((src->type == ObjType_EndPointLMP || src->type == ObjType_Dispatcher
         || src->type == ObjType_Kernel || src->type == ObjType_IRQTable)
        && owner == my_core_id)
    {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    return SYSRET(caps_create_from_existing(&dcb_current->cspace.cap,
                                            cnode_cptr, cnode_vbits,
                                            slot, owner, src));
}

INVOCATION_HANDLER(monitor_get_platform)
{
    INVOCATION_PRELUDE(3);
    // check args
    if (!access_ok(ACCESS_WRITE, sa->arg2, sizeof(struct platform_info))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }

    platform_get_info((struct platform_info*)sa->arg2);

    return SYSRET(SYS_ERR_OK);
}

INVOCATION_HANDLER(monitor_reclaim_ram)
{
    INVOCATION_PRELUDE(5);
    capaddr_t ret_cn_addr  = sa->arg2;
    capaddr_t ret_cn_level = sa->arg3;
    capaddr_t ret_slot     = sa->arg4;

    return sys_monitor_reclaim_ram(ret_cn_addr, ret_cn_level, ret_slot);
}

/**
 * \brief Spawn a new core and create a kernel cap for it.
 */
static struct sysret
monitor_spawn_core(
    struct capability *kernel_cap,
    arch_registers_state_t* context,
    int argc)
{
    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    hwid_t core_id         = sa->arg2;
    enum cpu_type cpu_type = sa->arg3;
    genvaddr_t entry       = sa->arg4;
    genpaddr_t context_id  = sa->arg5;
    uint64_t psci_use_hvc  = sa->arg6;

    psci_set_use_hvc(psci_use_hvc);
    return sys_monitor_spawn_core(core_id, cpu_type, entry, context_id);
}

static struct sysret
monitor_identify_cap(
    struct capability *kernel_cap,
    arch_registers_state_t* context,
    int argc)
{
    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    capaddr_t cptr = sa->arg2;
    int bits = sa->arg3;
    struct capability *retbuf = (void *)sa->arg4;

    return sys_monitor_identify_cap(&dcb_current->cspace.cap, cptr, bits, retbuf);
}

INVOCATION_HANDLER(monitor_identify_domains_cap)
{
    /* XXX - why is this not used consistently? */
    INVOCATION_PRELUDE(7);
    errval_t err;

    capaddr_t root_caddr = sa->arg2;
    capaddr_t root_vbits = sa->arg3;
    capaddr_t cptr       = sa->arg4;
    int bits             = sa->arg5;
    struct capability *retbuf = (void *)sa->arg6;

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    return sys_monitor_identify_cap(root, cptr, bits, retbuf);
}

static struct sysret handle_irqsrc_get_vec_start(struct capability *to,
        arch_registers_state_t* context,
        int argc)
{
    struct sysret ret;
    ret.error = SYS_ERR_OK;
    ret.value = to->u.irqsrc.vec_start;
    return ret;
}

static struct sysret handle_irqsrc_get_vec_end(struct capability *to,
        arch_registers_state_t* context,
        int argc)
{
    struct sysret ret;
    ret.error = SYS_ERR_OK;
    ret.value = to->u.irqsrc.vec_end;
    return ret;
}


static struct sysret handle_irqdest_connect(struct capability* to,
        arch_registers_state_t* context,
        int argc)

{
    struct registers_aarch64_syscall_args* sa = &context->syscall_args;
    return SYSRET(irq_connect(to, sa->arg2));
}

static struct sysret handle_irqdest_get_vector(struct capability *to,
        arch_registers_state_t* context,
        int argc)
{
    struct sysret ret;
    ret.error = SYS_ERR_OK;
    ret.value = to->u.irqdest.vector;
    return ret;
}

static struct sysret handle_irqdest_get_cpu(struct capability *to,
        arch_registers_state_t* context,
        int argc)
{
    struct sysret ret;
    ret.error = SYS_ERR_OK;
    ret.value = to->u.irqdest.cpu;
    return ret;
}

static struct sysret handle_irq_table_alloc_dest_cap(struct capability* to,
        arch_registers_state_t* context,
        int argc
        )
{
    struct registers_aarch64_syscall_args* sa = &context->syscall_args;
    return SYSRET(irq_table_alloc_dest_cap(sa->arg2, sa->arg3, sa->arg4, sa->arg5));
}

static struct sysret dispatcher_dump_ptables(
    struct capability* to, arch_registers_state_t* context, int argc)
{
    assert(to->type == ObjType_Dispatcher);
    assert(2 == argc);

    printf("kernel_dump_ptables\n");

   // struct dcb *dispatcher = to->u.dispatcher.dcb;

   // paging_dump_tables(dispatcher);

    return SYSRET(SYS_ERR_OK);
}

static struct sysret dispatcher_dump_capabilities(struct capability *cap,
        arch_registers_state_t* context, int argc)
{
    assert(cap->type == ObjType_Dispatcher);
    assert(2 == argc);
    struct dcb *dispatcher = cap->u.dispatcher.dcb;
    errval_t err = debug_print_cababilities(dispatcher);
    return SYSRET(err);
}

static struct sysret handle_idcap_identify(struct capability *cap,
                                           arch_registers_state_t *context,
                                           int argc)
{
    assert(cap->type == ObjType_ID);
    assert(2 == argc);

    idcap_id_t id;
    struct sysret sysret = sys_idcap_identify(cap, &id);
    sysret.value = id;

    return sysret;
}

static struct sysret handle_cap_identify(struct capability *root,
                                         arch_registers_state_t *context, int argc)
{
    assert(5 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    capaddr_t cptr = sa->arg2;
    uint8_t level = sa->arg3;
    struct capability *cap = (void *)sa->arg4;

    return sys_identify_cap(root, cptr, level, cap);
}

static struct sysret handle_devid_create(struct capability *cap,
                                         arch_registers_state_t *context,
                                         int argc)
{
    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    assert(cap->type == ObjType_DeviceIDManager);

    capaddr_t cnode_cptr = sa->arg2;
    capaddr_t cnode_level = sa->arg3;
    uint16_t slot = sa->arg4;

    uint32_t address = sa->arg5;
    uint32_t segflags = sa->arg6;

    struct capability devid;
    devid.type = ObjType_DeviceID;
    devid.u.deviceid.bus      = (uint8_t)(address >> 16);
    devid.u.deviceid.device   = (uint8_t)(address >> 8);
    devid.u.deviceid.function = (uint8_t)(address);
    devid.u.deviceid.type     = (uint8_t)(address >> 24);
    devid.u.deviceid.segment  = (uint16_t)(segflags >> 16);
    devid.u.deviceid.flags    = (uint16_t)(segflags);

    return SYSRET(caps_create_from_existing(&dcb_current->cspace.cap,
                                            cnode_cptr, cnode_level,
                                            slot, my_core_id, &devid));
}

static struct sysret handle_kcb_identify(struct capability *to,
                                  arch_registers_state_t *context,
                                  int argc)
{
    assert(3 == argc);

    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    return sys_handle_kcb_identify(to, (struct frame_identity *)sa->arg2);
}

typedef struct sysret (*invocation_t)(struct capability*,
                                      arch_registers_state_t*, int);

static invocation_t invocations[ObjType_Num][CAP_MAX_CMD] = {
    [ObjType_Dispatcher] = {
        [DispatcherCmd_Setup]       = handle_dispatcher_setup,
        [DispatcherCmd_Properties]  = handle_dispatcher_properties,
        [DispatcherCmd_PerfMon]     = handle_dispatcher_perfmon,
        [DispatcherCmd_DumpPTables]  = dispatcher_dump_ptables,
        [DispatcherCmd_DumpCapabilities] = dispatcher_dump_capabilities
    },
    [ObjType_KernelControlBlock] = {
        [KCBCmd_Identify] = handle_kcb_identify
    },
    [ObjType_L1CNode] = {
        [CNodeCmd_Copy]   = handle_copy,
        [CNodeCmd_Mint]   = handle_mint,
        [CNodeCmd_Retype] = handle_retype,
        [CNodeCmd_Create] = handle_create,
        [CNodeCmd_Delete] = handle_delete,
        [CNodeCmd_Revoke] = handle_revoke,
        [CNodeCmd_GetState] = handle_get_state,
        [CNodeCmd_GetSize] = handle_get_size,
        [CNodeCmd_Resize] = handle_resize,
        [CNodeCmd_CapIdentify] = handle_cap_identify,
    },
    [ObjType_L2CNode] = {
        [CNodeCmd_Copy]   = handle_copy,
        [CNodeCmd_Mint]   = handle_mint,
        [CNodeCmd_Retype] = handle_retype,
        [CNodeCmd_Create] = handle_create,
        [CNodeCmd_Delete] = handle_delete,
        [CNodeCmd_Revoke] = handle_revoke,
        [CNodeCmd_GetState] = handle_get_state,
        [CNodeCmd_Resize] = handle_resize,
        [CNodeCmd_CapIdentify] = handle_cap_identify,
    },
    [ObjType_VNode_AARCH64_l0] = {
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_AARCH64_l1] = {
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_AARCH64_l2] = {
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_AARCH64_l3] = {
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_Frame_Mapping] = {
        [MappingCmd_Destroy] = handle_mapping_destroy,
        [MappingCmd_Modify] = handle_mapping_modify,
    },
    [ObjType_DevFrame_Mapping] = {
        [MappingCmd_Destroy] = handle_mapping_destroy,
        [MappingCmd_Modify] = handle_mapping_modify,
    },
    [ObjType_VNode_AARCH64_l0_Mapping] = {
        [MappingCmd_Destroy] = handle_mapping_destroy,
        [MappingCmd_Modify] = handle_mapping_modify,
    },
    [ObjType_VNode_AARCH64_l1_Mapping] = {
        [MappingCmd_Destroy] = handle_mapping_destroy,
        [MappingCmd_Modify] = handle_mapping_modify,
    },
    [ObjType_VNode_AARCH64_l2_Mapping] = {
        [MappingCmd_Destroy] = handle_mapping_destroy,
        [MappingCmd_Modify] = handle_mapping_modify,
    },
    [ObjType_VNode_AARCH64_l3_Mapping] = {
        [MappingCmd_Destroy] = handle_mapping_destroy,
        [MappingCmd_Modify] = handle_mapping_modify,
    },
	[ObjType_IRQSrc] = {
        [IRQSrcCmd_GetVecStart] = handle_irqsrc_get_vec_start,
        [IRQSrcCmd_GetVecEnd] = handle_irqsrc_get_vec_end
	},
	[ObjType_IRQDest] = {
        [IRQDestCmd_Connect] = handle_irqdest_connect,
        [IRQDestCmd_GetVector] = handle_irqdest_get_vector,
        [IRQDestCmd_GetCpu] = handle_irqdest_get_cpu
	},
    [ObjType_IRQTable] = {
            [IRQTableCmd_AllocDestCap] = handle_irq_table_alloc_dest_cap,
        },
    [ObjType_Kernel] = {
        [KernelCmd_Cap_has_relations] = monitor_cap_has_relations,
        [KernelCmd_Clear_step]        = monitor_handle_clear_step,
        [KernelCmd_Copy_existing]     = monitor_copy_existing,
        [KernelCmd_Create_cap]        = monitor_create_cap,
        [KernelCmd_Delete_foreigns]   = monitor_handle_delete_foreigns,
        [KernelCmd_Delete_last]       = monitor_handle_delete_last,
        [KernelCmd_Delete_step]       = monitor_handle_delete_step,
        [KernelCmd_Domain_Id]         = monitor_handle_domain_id,
        [KernelCmd_Get_arch_id]       = monitor_get_arch_id,
        [KernelCmd_Get_cap_owner]     = monitor_get_cap_owner,
        [KernelCmd_Get_core_id]       = monitor_get_core_id,
        [KernelCmd_Has_descendants]   = monitor_handle_has_descendants,
        [KernelCmd_Identify_cap]      = monitor_identify_cap,
        [KernelCmd_Identify_domains_cap] = monitor_identify_domains_cap,
        [KernelCmd_Is_retypeable]     = monitor_handle_is_retypeable,
        [KernelCmd_Lock_cap]          = monitor_lock_cap,
        [KernelCmd_Nullify_cap]       = monitor_nullify_cap,
        [KernelCmd_Register]          = monitor_handle_register,
        [KernelCmd_Remote_relations]  = monitor_remote_relations,
        [KernelCmd_Retype]            = monitor_handle_retype,
        [KernelCmd_Revoke_mark_relations] = monitor_handle_revoke_mark_rels,
        [KernelCmd_Revoke_mark_target] = monitor_handle_revoke_mark_tgt,
        [KernelCmd_Set_cap_owner]     = monitor_set_cap_owner,
        /* XXX - why is this commented out? */
        //[KernelCmd_Setup_trace]       = handle_trace_setup,
        [KernelCmd_Spawn_core]        = monitor_spawn_core,
        [KernelCmd_Unlock_cap]        = monitor_unlock_cap,
        [KernelCmd_Get_platform]        = monitor_get_platform,
        [KernelCmd_ReclaimRAM]        = monitor_reclaim_ram,
    },
    [ObjType_IPI] = {
        [IPICmd_Send_Start]  = monitor_spawn_core,
    },
    [ObjType_ID] = {
        [IDCmd_Identify] = handle_idcap_identify
    },
    [ObjType_DeviceIDManager] = {
            [DeviceIDManager_CreateID] = handle_devid_create,
    },
};

static struct sysret
handle_invoke(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
              uint64_t a4, uint64_t a5, uint64_t a6,
              arch_registers_state_t *context)
{
    struct registers_aarch64_syscall_args* sa = &context->syscall_args;

    /* XXX - can we generate them from the same source? */
    //
    // Must match lib/barrelfish/include/arch/aarch64/arch/invocations.h
    //
    uint8_t   flags       = FIELD(24,4,a0);
    uint8_t   invoke_bits = FIELD(16,8,a0);
    capaddr_t invoke_cptr = a1;

// printf("%s: %zd %zd\n", __func__, systime_now(), armv8_CNTP_TVAL_EL0_rd(NULL));
    debug(SUBSYS_SYSCALL, "sys_invoke(0x%"PRIxCADDR"(%d))\n",
                invoke_cptr, invoke_bits);

    struct sysret r = { .error = SYS_ERR_OK, .value = 0 };

    struct capability* to;
    r.error = caps_lookup_cap(&dcb_current->cspace.cap,
                              invoke_cptr, invoke_bits,
                              &to, CAPRIGHTS_READ);
    if (err_is_ok(r.error))
    {
        assert(to != NULL);
        assert(to->type < ObjType_Num);

        if (ObjType_EndPointLMP == to->type && !(flags & LMP_FLAG_IDENTIFY))
        {
            struct dcb *listener = to->u.endpointlmp.listener;
            assert(listener != NULL);

            if (listener->disp) {
                uint8_t length_words = FIELD(28,8,a0);
                uint8_t send_bits    = FIELD(8,8,a0);
                capaddr_t send_cptr = a2;
                /* limit length of message from buggy/malicious sender */
                length_words = min(length_words, LMP_MSG_LENGTH);

                // does the sender want to yield their timeslice on success?
                bool sync = flags & LMP_FLAG_SYNC;
                // does the sender want to yield to the target
                // if undeliverable?
                bool yield = flags & LMP_FLAG_YIELD;
                // is the cap (if present) to be deleted on send?
                bool give_away = flags & LMP_FLAG_GIVEAWAY;

                // Message registers in context are
                // discontinguous for now so copy message words
                // to temporary container. This is fixable, but
                // not in this pass.
                uintptr_t msg_words[LMP_MSG_LENGTH];
                msg_words[0] = a3;
                msg_words[1] = a4;
                msg_words[2] = a5;
                msg_words[3] = a6;
                STATIC_ASSERT(LMP_MSG_LENGTH == 4, "Oops");

                // try to deliver message
                r.error = lmp_deliver(to, dcb_current, msg_words,
                                      length_words, send_cptr, send_bits, give_away);

                /* Switch to reciever upon successful delivery
                 * with sync flag, or (some cases of)
                 * unsuccessful delivery with yield flag */
                enum err_code err_code = err_no(r.error);
                if ((sync && err_is_ok(r.error)) ||
                    (yield && (err_code == SYS_ERR_LMP_BUF_OVERFLOW
                               || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_LOOKUP
                               || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID
                               || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_SLOT_OCCUPIED))
                   ) {
                    if (err_is_fail(r.error)) {
                        struct dispatcher_shared_generic *current_disp =
                            get_dispatcher_shared_generic(dcb_current->disp);
                        struct dispatcher_shared_generic *listener_disp =
                            get_dispatcher_shared_generic(listener->disp);
                        debug(SUBSYS_DISPATCH, "LMP failed; %.*s yields to %.*s: %u\n",
                              DISP_NAME_LEN, current_disp->name,
                              DISP_NAME_LEN, listener_disp->name, err_code);
                    }

                    // special-case context switch: ensure correct state in
                    // current DCB
                    dispatcher_handle_t handle = dcb_current->disp;
                    struct dispatcher_shared_aarch64 *disp =
                        get_dispatcher_shared_aarch64(handle);
                    dcb_current->disabled =
                        dispatcher_is_disabled_ip(handle, context->named.pc);
                    if (dcb_current->disabled) {
                        assert(context == &disp->disabled_save_area);
                        context->named.x0 = r.error;
                    }
                    else {
                        assert(context == &disp->enabled_save_area);
                        context->named.x0 = r.error;
                    }
                    dispatch(listener);
                }
            }
            else {
                r.error = SYS_ERR_LMP_NO_TARGET;
            }
        }
        else
        {
            uint8_t cmd = FIELD(8,8,a0);
            int argc    = FIELD(4,4,a0);
            if (cmd < CAP_MAX_CMD)
            {
                invocation_t invocation = invocations[to->type][cmd];
                if (invocation)
                {
                    /* XXX - Until we improve the syscall path, we're stacking
                     * all of the argument registers here.  Yuck. */
                    sa->arg0 = a0;
                    sa->arg1 = a1;
                    sa->arg2 = a2;
                    sa->arg3 = a3;
                    sa->arg4 = a4;
                    sa->arg5 = a5;
                    sa->arg6 = a6;

                    r = invocation(to, context, argc);
                    if (!dcb_current)
                    {
                        // dcb_current was removed, dispatch someone else
                        assert(err_is_ok(r.error));
                        dispatch(schedule());
                    }
                    return r;
                }
            }
            printk(LOG_ERR, "Bad invocation type %d cmd %d\n", to->type, cmd);
            r.error = SYS_ERR_ILLEGAL_INVOCATION;
        }
    }

    return r;
}

static struct sysret handle_debug_syscall(int msg)
{
    struct sysret retval = { .error = SYS_ERR_OK };
    switch (msg) {
        case DEBUG_FLUSH_CACHE:
            /* XXX - implement me */
            break;

        case DEBUG_CONTEXT_COUNTER_RESET:
            dispatch_csc_reset();
            break;

        case DEBUG_CONTEXT_COUNTER_READ:
            retval.value = dispatch_get_csc();
            break;

        case DEBUG_TIMESLICE_COUNTER_READ:
            retval.value = systime_now();
            break;

        case DEBUG_HARDWARE_TIMER_READ:
            retval.value = systime_now();
            break;

        case DEBUG_HARDWARE_TIMER_HERTZ_READ:
            retval.value = systime_frequency;
            break;

        case DEBUG_GET_TSC_PER_MS:
            // XXX: Implement if possible at all.
            retval.value = 1;
            break;

        default:
            printk(LOG_ERR, "invalid sys_debug msg type %d\n", msg);
            retval.error = err_push(retval.error, SYS_ERR_ILLEGAL_SYSCALL);
    }
    return retval;
}


/* XXX - function documentation is inconsistent. */
/**
 * System call dispatch routine.
 *
 * @return struct sysret for all calls except yield / invoke.
 *
 * The first 8 syscall arguments are passed in registers, preserved by the
 * assembly stub.
 *
 */
void sys_syscall(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3,
                 uint64_t a4, uint64_t a5, uint64_t a6,
                 arch_registers_state_t *context)
{
    // XXX
    // Set dcb_current->disabled correctly.  This should really be
    // done in exceptions.S
    // XXX

    assert(dcb_current != NULL);
    dispatcher_handle_t handle = dcb_current->disp;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    assert((struct dispatcher_shared_generic *)(dcb_current->disp) == disp);
    if (dispatcher_is_disabled_ip((dispatcher_handle_t)disp, context->named.pc)) {
        assert(context == dispatcher_get_disabled_save_area((dispatcher_handle_t)disp));
        dcb_current->disabled = true;
    } else {
        assert(context == dispatcher_get_enabled_save_area((dispatcher_handle_t)disp));
        dcb_current->disabled = false;
    }
    // TODO: ARMv7 gets this from assembly code.
    // assert(disabled == dcb_current->disabled);

    STATIC_ASSERT_OFFSETOF(struct sysret, error, 0);

    int syscall = FIELD(0,4,a0);
    int argc    = FIELD(4,4,a0);


    debug(SUBSYS_SYSCALL, "syscall: syscall=%d, argc=%d\n", syscall, argc);
//    debug(SUBSYS_SYSCALL, "syscall: disabled=%d\n", disabled);
    debug(SUBSYS_SYSCALL, "syscall: context=0x%"PRIxLVADDR", disp=0x%"PRIxLVADDR"\n",
            context, disp );

    struct sysret r = { .error = SYS_ERR_INVARGS_SYSCALL, .value = 0 };

    switch (syscall)
    {
        case SYSCALL_INVOKE:
            r = handle_invoke(a0, a1, a2, a3, a4, a5, a6, context);
            break;

        case SYSCALL_YIELD:
            if (argc == 2) {
                r = sys_yield((capaddr_t)a1);
            }
            break;

        case SYSCALL_NOP:
            r.error = SYS_ERR_OK;
            break;

        case SYSCALL_PRINT:
            if (argc == 3) {
                /* XXX - The user can pass arbitrary bad pointers and lengths
                 * here! */
                r.error = sys_print((const char*)a1, (size_t)a2);
            }
            break;

        case SYSCALL_DEBUG:
            if (a1 == DEBUG_CREATE_IRQ_SRC_CAP) {
                r.error = irq_debug_create_src_cap(a2, a3, a4, a5, a6);
            } else if (argc == 2) {
                r = handle_debug_syscall(a1);
            }

            break;

        default:
            printf("Illegal syscall %u\n", syscall);
            r.error = SYS_ERR_ILLEGAL_SYSCALL;
            break;
    }

    if (r.error) {
        debug(SUBSYS_SYSCALL, "syscall failed %016"PRIx64
                              " => %016"PRIxERRV"\n", a0, r.error);
    }

    /* XXX - shouldn't stack & unstack these. */
    context->named.x0 = r.error;
    context->named.x1 = r.value;

    debug(SUBSYS_SYSCALL, "syscall: Resuming; dcb->disabled=%d, disp->disabled=%d\n",
          dcb_current->disabled, disp->disabled);

    resume(context);
}
