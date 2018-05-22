/**
 * \file
 * \brief System calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <sys_debug.h>
#include <syscall.h>
#include <barrelfish_kpi/syscalls.h>
#include <capabilities.h>
#include <mdb/mdb.h>
#include <dispatch.h>
#include <paging_kernel_arch.h>
#include <exec.h>
#include <arch/x86/apic.h>
#include <arch/x86/perfmon_intel.h>
#include <arch/x86/global.h>
#include <barrelfish_kpi/sys_debug.h>
#include <barrelfish_kpi/lmp.h>
#include <barrelfish_kpi/dispatcher_shared_target.h>
#include <barrelfish_kpi/syscall_overflows_arch.h>
#include <trace/trace.h>
#include <arch/x86/debugregs.h>
#include <arch/x86/syscall.h>
#include <arch/x86/timing.h>
#include <mdb/mdb_tree.h>
#include <useraccess.h>
#include <arch/x86/perfmon_amd.h>
#include <arch/x86/ipi_notify.h>

/* FIXME: lots of missing argument checks in this function */
static struct sysret handle_dispatcher_setup(struct capability *to,
                                             int cmd, uintptr_t *args)
{
    capaddr_t odptr = args[0];
    capaddr_t cptr = args[1];
    uintptr_t rundepth = args[2];
    int depth = rundepth & 0xff;
    bool run = rundepth >> 8;
    capaddr_t vptr = args[3];
    capaddr_t dptr = args[4];

    return sys_dispatcher_setup(to, cptr, depth, vptr, dptr, run, odptr);

}

static struct sysret handle_dispatcher_properties(struct capability *to,
                                                  int cmd, uintptr_t *args)
{
    enum task_type type = args[0] >> 16;
    unsigned short weight = args[0] & 0xffff;
    unsigned long deadline = args[1];
    unsigned long wcet = args[2];
    unsigned long period = args[3];
    unsigned long release = args[4];

    return sys_dispatcher_properties(to, type, deadline, wcet, period,
                                     release, weight);
}

// XXX: FIXME: cleanup and handle errors!
static struct sysret handle_dispatcher_perfmon(struct capability *to,
                                               int cmd, uintptr_t *args)
{
#if 1
    return SYSRET(SYS_ERR_PERFMON_NOT_AVAILABLE);

#else
    perfmon_counter_t idx = (perfmon_counter_t)(args[0]);

    // Currently only AMD perfmon is supported
    if(!perfmon_amd_supported()) {
        return SYSRET(SYS_ERR_PERFMON_NOT_AVAILABLE);
    }

    uint64_t operation = args[1];
    switch(operation) {
    case 0: {
        perfmon_event_t event = args[2];
        perfmon_mask_t umask = args[3];
        bool os = args[4];
        perfmon_amd_measure_start(event, umask, os ? true : false, idx);
        break;
    }

    case 1: {
        uint64_t val = args[2];
        perfmon_amd_measure_write(val, idx);
        break;
    }

    default:
        panic("Unknown performance monitoring function!");
    }

    return SYSRET(SYS_ERR_OK);
#endif
}

static struct sysret handle_retype_common(struct capability *root,
                                          uintptr_t *args,
                                          bool from_monitor)
{
    // Source capability cptr
    capaddr_t source_cptr      = args[0];
    // Type to retype to
    enum objtype type        = args[1] >> 16;
    // Object bits for variable-sized types
    uint8_t objbits          = (args[1] >> 8) & 0xff;
    // Destination cnode cptr
    capaddr_t  dest_cnode_cptr = args[2];
    // Destination slot number
    capaddr_t dest_slot        = args[3];
    // Valid bits in destination cnode cptr
    uint64_t dest_vbits      = args[1] & 0xff;

    return sys_retype(root, source_cptr, type, objbits, dest_cnode_cptr,
                      dest_slot, dest_vbits, from_monitor);
}

static struct sysret handle_retype(struct capability *root, int cmd, uintptr_t *args)
{
    return handle_retype_common(root, args, false);
}

static struct sysret handle_create(struct capability *root, int cmd,
                                   uintptr_t *args)
{
    /* Retrieve arguments */
    enum objtype type         = args[0] >> 16;
    uint8_t objbits           = (args[0] >> 8) & 0xff;
    capaddr_t dest_cnode_cptr = args[1];
    capaddr_t dest_slot       = args[2];
    uint8_t dest_vbits        = args[0] & 0xff;

    return sys_create(root, type, objbits, dest_cnode_cptr, dest_slot,
                      dest_vbits);
}

/**
 * Common code for copying and minting except the mint flag and param passing
 */
static struct sysret copy_or_mint(struct capability *root,
                                  uintptr_t *args, bool mint)
{
    /* Retrive arguments */
    capaddr_t  destcn_cptr   = args[0];
    capaddr_t  source_cptr   = args[1];
    capaddr_t dest_slot      = args[2] >> 16;
    int      destcn_vbits  = (args[2] >> 8) & 0xff;
    int      source_vbits  = args[2] & 0xff;
    uintptr_t param1, param2;
    // params only sent if mint operation
    if (mint) {
        param1 = args[3];
        param2 = args[4];
    } else {
        param1 = param2 = 0;
    }

    return sys_copy_or_mint(root, destcn_cptr, dest_slot, source_cptr,
                            destcn_vbits, source_vbits, param1, param2, mint);
}

static struct sysret handle_mint(struct capability *root, int cmd, uintptr_t *args)
{
    return copy_or_mint(root, args, true);
}

static struct sysret handle_copy(struct capability *root,
                                 int cmd, uintptr_t *args)
{
    return copy_or_mint(root, args, false);
}

static struct sysret handle_delete(struct capability *root, int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    int bits     = args[1];
    return  sys_delete(root, cptr, bits);
}

static struct sysret handle_revoke(struct capability *root,
                                   int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    int bits     = args[1];
    return  sys_revoke(root, cptr, bits);
}

static struct sysret handle_get_state(struct capability *root,
                                      int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    int bits = args[1];
    return sys_get_state(root, cptr, bits);
}

static struct sysret handle_map(struct capability *pgtable,
                                int cmd, uintptr_t *args)
{
    /* Retrieve arguments */
    capaddr_t  source_cptr  = args[0];
    int        source_vbits = args[1] & 0xff;
    int        mcn_vbits    = (args[1] >> 8) & 0xff;
    cslot_t    mapping_slot = args[1] >> 16;
    capaddr_t  mcn_addr     = args[2];
    cslot_t    dest_slot    = args[3] >> 16;
    uintptr_t  pte_count    = args[3] & 0xffff;
    uint64_t  *overflow     = (uint64_t *)args[4];
    uint64_t   offset       = overflow[0];
    uint64_t   flags        = overflow[1];

    return sys_map(pgtable, dest_slot, source_cptr, source_vbits,
                   flags, offset, pte_count, mcn_addr, mcn_vbits, mapping_slot);
}

static struct sysret handle_unmap(struct capability *pgtable,
                                  int cmd, uintptr_t *args)
{
    size_t mapping_caddr = args[0];
    int    mapping_bits  = args[1];

    errval_t err;
    struct cte *mapping = NULL;
    err = caps_lookup_slot(&dcb_current->cspace.cap, mapping_caddr, mapping_bits,
                           &mapping, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_CAP_NOT_FOUND));
    }

    err = page_mappings_unmap(pgtable, mapping);
    return SYSRET(err);
}

static struct sysret handle_mapping_destroy(struct capability *mapping,
                                            int cmd, uintptr_t *args)
{
    panic("NYI!");
    return SYSRET(SYS_ERR_OK);
}

static struct sysret handle_mapping_modify(struct capability *mapping,
                                           int cmd, uintptr_t *args)
{
    // Modify flags of (part of) mapped region of frame
    assert(type_is_mapping(mapping->type));

    // unpack arguments
    size_t offset = args[0]; // in pages; of first page to modify from first
                             // page in mapped region
    size_t pages  = args[1]; // #pages to modify
    size_t flags  = args[2]; // new flags
    genvaddr_t va = args[3]; // virtual addr hint

    errval_t err = page_mappings_modify_flags(mapping, offset, pages, flags, va);

    return (struct sysret) {
        .error = err,
        .value = 0,
    };
}


/// Different handler for cap operations performed by the monitor
static struct sysret monitor_handle_retype(struct capability *kernel_cap,
                                           int cmd, uintptr_t *args)
{
    errval_t err;

    struct remote_retype_syscall_overflow *rootcap = (void*)args[0];

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, rootcap->rootcap_addr,
            rootcap->rootcap_vbits, &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    /* XXX: this hides the first argument which retype_common doesn't know
     * about */
    return handle_retype_common(root, &args[1], true);
}

static struct sysret monitor_handle_has_descendants(struct capability *kernel_cap,
                                                    int cmd, uintptr_t *args)
{
    // check access to user pointer
    if (!access_ok(ACCESS_READ, args[0], sizeof(struct capability))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }

    struct capability *src = (struct capability *)args[0];

    struct cte *next = mdb_find_greater(src, false);

    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = (next && is_ancestor(&next->cap, src)),
    };
}

static struct sysret monitor_handle_delete_last(struct capability *kernel_cap,
                                                int cmd, uintptr_t *args)
{
    capaddr_t root_caddr = args[0];
    capaddr_t target_caddr = args[1];
    capaddr_t retcn_caddr = args[2];
    cslot_t retcn_slot = args[3];
    uint8_t target_vbits = (args[4]>>16)&0xff;
    uint8_t root_vbits = (args[4]>>8)&0xff;
    uint8_t retcn_vbits = args[4]&0xff;

    return sys_monitor_delete_last(root_caddr, root_vbits, target_caddr,
                                   target_vbits, retcn_caddr, retcn_vbits, retcn_slot);
}

static struct sysret monitor_handle_delete_foreigns(struct capability *kernel_cap,
                                                    int cmd, uintptr_t *args)
{
    capaddr_t caddr = args[0];
    uint8_t bits = args[1];
    return sys_monitor_delete_foreigns(caddr, bits);
}

static struct sysret monitor_handle_revoke_mark_tgt(struct capability *kernel_cap,
                                                    int cmd, uintptr_t *args)
{
    capaddr_t root_caddr = args[0];
    uint8_t root_vbits = args[1];
    capaddr_t target_caddr = args[2];
    uint8_t target_vbits = args[3];

    return sys_monitor_revoke_mark_tgt(root_caddr, root_vbits,
                                       target_caddr, target_vbits);
}

static struct sysret monitor_handle_revoke_mark_rels(struct capability *kernel_cap,
                                                     int cmd, uintptr_t *args)
{
    // user pointer to src cap, check access
    if (!access_ok(ACCESS_READ, args[0], sizeof(struct capability))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }
    struct capability *base = (struct capability*)args[0];

    return sys_monitor_revoke_mark_rels(base);
}

static struct sysret monitor_handle_delete_step(struct capability *kernel_cap,
                                                int cmd, uintptr_t *args)
{
    capaddr_t ret_cn_addr = args[0];
    capaddr_t ret_cn_bits = args[1];
    capaddr_t ret_slot = args[2];
    return sys_monitor_delete_step(ret_cn_addr, ret_cn_bits, ret_slot);
}

static struct sysret monitor_handle_clear_step(struct capability *kernel_cap,
                                               int cmd, uintptr_t *args)
{
    capaddr_t ret_cn_addr = args[0];
    capaddr_t ret_cn_bits = args[1];
    capaddr_t ret_slot = args[2];
    return sys_monitor_clear_step(ret_cn_addr, ret_cn_bits, ret_slot);
}


static struct sysret monitor_handle_register(struct capability *kernel_cap,
                                             int cmd, uintptr_t *args)
{
    capaddr_t ep_caddr = args[0];
    return sys_monitor_register(ep_caddr);
}

static struct sysret monitor_get_core_id(struct capability *kernel_cap,
                                         int cmd, uintptr_t *args)
{
    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = my_core_id
    };
}

static struct sysret monitor_get_arch_id(struct capability *kernel_cap,
                                         int cmd, uintptr_t *args)
{
    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = apic_id
    };
}

static struct sysret monitor_identify_cap_common(struct capability *kernel_cap,
                                                 struct capability *root,
                                                 uintptr_t *args)
{
    capaddr_t cptr = args[0];
    int bits = args[1];
    struct capability *retbuf = (void *)args[2];

    return sys_monitor_identify_cap(root, cptr, bits, retbuf);
}

static struct sysret monitor_identify_cap(struct capability *kernel_cap,
                                          int cmd, uintptr_t *args)
{
    return monitor_identify_cap_common(kernel_cap, &dcb_current->cspace.cap, args);
}

static struct sysret monitor_identify_domains_cap(struct capability *kernel_cap,
                                                  int cmd, uintptr_t *args)
{
    errval_t err;

    capaddr_t root_caddr = args[0];
    capaddr_t root_vbits = args[1];

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    /* XXX: conceal first two words of arguments */
    return monitor_identify_cap_common(kernel_cap, root, &args[2]);
}

static struct sysret monitor_cap_has_relations(struct capability *kernel_cap,
                                               int cmd, uintptr_t *args)
{
    capaddr_t caddr = args[0];
    uint8_t vbits = args[1];
    uint8_t mask = args[2];

    return sys_cap_has_relations(caddr, vbits, mask);
}

static struct sysret monitor_remote_relations(struct capability *kernel_cap,
                                              int cmd, uintptr_t *args)
{
    capaddr_t root_addr = args[0];
    int root_bits = args[1];
    capaddr_t cptr = args[2];
    int bits = args[3];
    uint8_t relations = args[4] & 0xFF;
    uint8_t mask = (args[4] >> 8) & 0xFF;

    return sys_monitor_remote_relations(root_addr, root_bits, cptr, bits,
                                        relations, mask);
}


static struct sysret monitor_create_cap(struct capability *kernel_cap,
                                        int cmd, uintptr_t *args)
{
    /* Create the cap in the destination */
    capaddr_t cnode_cptr = args[0];
    int cnode_vbits      = args[1];
    size_t slot          = args[2];
    coreid_t owner       = args[3];
    struct capability *src =
        (struct capability*)args[4];

    /* Cannot create null caps */
    if (src->type == ObjType_Null) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    /* For certain types, only foreign copies can be created here */
    if ((src->type == ObjType_EndPoint || src->type == ObjType_Dispatcher
         || src->type == ObjType_Kernel || src->type == ObjType_IRQTable)
        && owner == my_core_id)
    {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    return SYSRET(caps_create_from_existing(&dcb_current->cspace.cap,
                                            cnode_cptr, cnode_vbits,
                                            slot, owner, src));
}

static struct sysret monitor_copy_existing(struct capability *kernel_cap,
                                        int cmd, uintptr_t *args)
{
    capaddr_t cnode_cptr = args[0];
    int cnode_vbits    = args[1];
    size_t slot        = args[2];

    // user pointer to src cap, check access
    if (!access_ok(ACCESS_READ, args[3], sizeof(struct capability))) {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }
    /* Get the raw metadata of the capability to create from user pointer */
    struct capability *src = (struct capability *)args[3];

    return sys_monitor_copy_existing(src, cnode_cptr, cnode_vbits, slot);
}


static struct sysret monitor_nullify_cap(struct capability *kernel_cap,
                                         int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    int bits = args[1];

    return sys_monitor_nullify_cap(cptr, bits);
}

static struct sysret monitor_handle_sync_timer(struct capability *kern_cap,
                                               int cmd, uintptr_t *args)
{
    uint64_t synctime = (uint64_t)args[0] << 32;
    synctime |= (uint64_t)args[1];
    return sys_monitor_handle_sync_timer(synctime);
}

static struct sysret handle_frame_identify(struct capability *to,
                                           int cmd, uintptr_t *args)
{
    // Return with physical base address of frame
    // XXX: pack size into bottom bits of base address
    assert(to->type == ObjType_Frame || to->type == ObjType_DevFrame);
    assert((to->u.frame.base & BASE_PAGE_MASK) == 0);
    assert(to->u.frame.bits < BASE_PAGE_SIZE);
    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = to->u.frame.base | to->u.frame.bits,
    };
}

static struct sysret handle_io(struct capability *to, int cmd, uintptr_t *args)
{
    uint32_t    port = args[0];
    uint32_t    data = args[1];

    return sys_io(to, cmd, port, data);
}

static struct sysret monitor_handle_domain_id(struct capability *monitor_cap,
                                              int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    domainid_t domain_id = args[1];

    return sys_monitor_domain_id(cptr, domain_id);
}

static struct sysret monitor_get_cap_owner(struct capability *monitor_cap,
                                           int cmd, uintptr_t *args)
{
    capaddr_t root_addr = args[0];
    uint8_t root_bits = args[1];
    capaddr_t cptr = args[2];
    uint8_t bits = args[3];

    return sys_get_cap_owner(root_addr, root_bits, cptr, bits);
}

static struct sysret monitor_set_cap_owner(struct capability *monitor_cap,
                                           int cmd, uintptr_t *args)
{
    capaddr_t root_addr = args[0];
    uint8_t root_bits = args[1];
    capaddr_t cptr = args[2];
    uint8_t bits = args[3];
    coreid_t owner = args[4];

    return sys_set_cap_owner(root_addr, root_bits, cptr, bits, owner);
}

static struct sysret monitor_lock_cap(struct capability *monitor_cap,
                                      int cmd, uintptr_t *args)
{
    capaddr_t root_addr = args[0];
    uint8_t root_bits = args[1];
    capaddr_t cptr = args[2];
    uint8_t bits = args[3];

    return sys_lock_cap(root_addr, root_bits, cptr, bits);
}

static struct sysret monitor_unlock_cap(struct capability *monitor_cap,
                                        int cmd, uintptr_t *args)
{
    capaddr_t root_addr = args[0];
    uint8_t root_bits = args[1];
    capaddr_t cptr = args[2];
    uint8_t bits = args[3];

    return sys_unlock_cap(root_addr, root_bits, cptr, bits);
}

/**
 * \brief Set up tracing in the kernel
 */
static struct sysret handle_trace_setup(struct capability *cap,
                                        int cmd, uintptr_t *args)
{
    struct capability *frame;
    errval_t err;

    /* lookup passed cap */
    capaddr_t cptr = args[0];
    err = caps_lookup_cap(&dcb_current->cspace.cap, cptr, CPTR_BITS, &frame,
                          CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    lpaddr_t lpaddr = gen_phys_to_local_phys(frame->u.frame.base);
    kernel_trace_buf = local_phys_to_mem(lpaddr);
    //printf("kernel.%u: handle_trace_setup at %lx\n", apic_id, kernel_trace_buf);

    // Copy boot applications.
	trace_copy_boot_applications();

    return SYSRET(SYS_ERR_OK);
}

static struct sysret handle_irqsrc_get_vector(struct capability * to, int cmd,
        uintptr_t *args)
{
    struct sysret ret;
    ret.error = SYS_ERR_OK;
    ret.value = to->u.irqsrc.vector;
    return ret;

}


static struct sysret handle_irqdest_get_vector(struct capability *to, int cmd,
                                            uintptr_t *args)
{
    struct sysret ret;
    ret.error = SYS_ERR_OK;
    ret.value = to->u.irqdest.vector;
    return ret;
}

static struct sysret handle_irqdest_connect(struct capability *to, int cmd,
                                            uintptr_t *args)
{
    return SYSRET(irq_connect(to, args[0]));
}

static struct sysret handle_irq_table_alloc(struct capability *to, int cmd,
                                            uintptr_t *args)
{
    struct sysret ret;
    int outvec;
    ret.error = irq_table_alloc(&outvec);
    ret.value = outvec;
    return ret;
}

static struct sysret handle_irq_table_alloc_dest_cap(struct capability *to, int cmd,
                                            uintptr_t *args)
{
    return SYSRET(irq_table_alloc_dest_cap(args[0],args[1],args[2]));
}

static struct sysret handle_irq_table_set(struct capability *to, int cmd, uintptr_t *args)
{
    return SYSRET(irq_table_set(args[0], args[1]));
}

static struct sysret handle_irq_table_delete(struct capability *to, int cmd, uintptr_t *args)
{
    return SYSRET(irq_table_delete(args[0]));
}

/**
 * \brief Return system-wide unique ID of this ID cap.
 */
static struct sysret handle_idcap_identify(struct capability *cap, int cmd,
                                           uintptr_t *args)
{
    idcap_id_t *idp = (idcap_id_t *) args[0];

    // Check validity of user space pointer
    if (!access_ok(ACCESS_WRITE, (lvaddr_t) idp, sizeof(*idp)))  {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }

    return sys_idcap_identify(cap, idp);
}

static struct sysret kernel_send_init_ipi(struct capability *cap, int cmd,
                                          uintptr_t *args)
{
    coreid_t destination = args[0];
    apic_send_init_assert(destination, xapic_none);
    apic_send_init_deassert();

    return SYSRET(SYS_ERR_OK);
}

static struct sysret kernel_send_start_ipi(struct capability *cap,
                                           int cmd,
                                           uintptr_t *args)
{
    coreid_t destination = args[0];
    genvaddr_t start_vector = X86_32_REAL_MODE_SEGMENT_TO_REAL_MODE_PAGE(X86_32_REAL_MODE_SEGMENT);
    apic_send_start_up(destination, xapic_none, start_vector);

    return SYSRET(SYS_ERR_OK);
}


static struct sysret kernel_get_global_phys(struct capability *cap,
                                           int cmd,
                                           uintptr_t *args)
{

    struct sysret sysret;
    sysret.value = mem_to_local_phys((lvaddr_t)global);
    sysret.error = SYS_ERR_OK;

    return sysret;
}

static struct sysret kernel_ipi_register(struct capability *cap,
                                         int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Kernel);
    capaddr_t ep = args[0];
    int chanid = args[1];
    return SYSRET(ipi_register_notification(ep, chanid));
}

static struct sysret kernel_ipi_delete(struct capability *cap,
                                       int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Kernel);
    assert(!"NYI");
    return SYSRET(SYS_ERR_OK);
}

static struct sysret handle_ipi_notify_send(struct capability *cap,
                                            int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Notify_IPI);
    return ipi_raise_notify(cap->u.notify_ipi.coreid, cap->u.notify_ipi.chanid);
}

static struct sysret dispatcher_dump_ptables(struct capability *cap,
                                             int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Dispatcher);

    printf("kernel_dump_ptables\n");

    struct dcb *dispatcher = cap->u.dispatcher.dcb;

    paging_dump_tables(dispatcher);

    return SYSRET(SYS_ERR_OK);
}

static struct sysret dispatcher_dump_capabilities(struct capability *cap,
                                             int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Dispatcher);
    struct dcb *dispatcher = cap->u.dispatcher.dcb;
    errval_t err = debug_print_cababilities(dispatcher);
    return SYSRET(err);
}


static struct sysret kernel_add_kcb(struct capability *kern_cap,
                                    int cmd, uintptr_t *args)
{
    uintptr_t kcb_addr = args[0];
    struct kcb *new_kcb = (struct kcb *)kcb_addr;

    return sys_kernel_add_kcb(new_kcb);
}

static struct sysret kernel_remove_kcb(struct capability *kern_cap,
                                       int cmd, uintptr_t *args)
{
    printk(LOG_NOTE, "in kernel_remove_kcb invocation!\n");
    uintptr_t kcb_addr = args[0];
    struct kcb *to_remove = (struct kcb *)kcb_addr;

    return sys_kernel_remove_kcb(to_remove);
}

static struct sysret kernel_suspend_kcb_sched(struct capability *kern_cap,
                                              int cmd, uintptr_t *args)
{
    printk(LOG_NOTE, "in kernel_suspend_kcb_sched invocation!\n");
    return sys_kernel_suspend_kcb_sched((bool)args[0]);
}

static struct sysret handle_kcb_identify(struct capability *to,
                                         int cmd, uintptr_t *args)
{
    return sys_handle_kcb_identify(to);
}

typedef struct sysret (*invocation_handler_t)(struct capability *to,
                                              int cmd, uintptr_t *args);

static invocation_handler_t invocations[ObjType_Num][CAP_MAX_CMD] = {
    [ObjType_Dispatcher] = {
        [DispatcherCmd_Setup]        = handle_dispatcher_setup,
        [DispatcherCmd_Properties]   = handle_dispatcher_properties,
        [DispatcherCmd_PerfMon]      = handle_dispatcher_perfmon,
        [DispatcherCmd_DumpPTables]  = dispatcher_dump_ptables,
        [DispatcherCmd_DumpCapabilities] = dispatcher_dump_capabilities
    },
    [ObjType_KernelControlBlock] = {
        [FrameCmd_Identify] = handle_kcb_identify,
    },
    [ObjType_Frame] = {
        [FrameCmd_Identify] = handle_frame_identify,
    },
    [ObjType_DevFrame] = {
        [FrameCmd_Identify] = handle_frame_identify,
    },
    [ObjType_L1CNode] = {
        [CNodeCmd_Copy]   = handle_copy,
        [CNodeCmd_Mint]   = handle_mint,
        [CNodeCmd_Retype] = handle_retype,
        [CNodeCmd_Create] = handle_create,
        [CNodeCmd_Delete] = handle_delete,
        [CNodeCmd_Revoke] = handle_revoke,
        [CNodeCmd_GetState] = handle_get_state,
    },
    [ObjType_VNode_x86_32_pdpt] = {
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_x86_32_pdir] = {
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_x86_32_ptable] = {
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
    [ObjType_VNode_x86_32_pdpt_Mapping] = {
        [MappingCmd_Destroy] = handle_mapping_destroy,
        [MappingCmd_Modify] = handle_mapping_modify,
    },
    [ObjType_VNode_x86_32_pdir_Mapping] = {
        [MappingCmd_Destroy] = handle_mapping_destroy,
        [MappingCmd_Modify] = handle_mapping_modify,
    },
    [ObjType_VNode_x86_32_ptable_Mapping] = {
        [MappingCmd_Destroy] = handle_mapping_destroy,
        [MappingCmd_Modify] = handle_mapping_modify,
    },
    [ObjType_Kernel] = {
        [KernelCmd_Get_core_id]  = monitor_get_core_id,
        [KernelCmd_Get_arch_id]  = monitor_get_arch_id,
        [KernelCmd_Identify_cap] = monitor_identify_cap,
        [KernelCmd_Identify_domains_cap] = monitor_identify_domains_cap,
        [KernelCmd_Remote_relations] = monitor_remote_relations,
        [KernelCmd_Cap_has_relations] = monitor_cap_has_relations,
        [KernelCmd_Create_cap]   = monitor_create_cap,
        [KernelCmd_Copy_existing] = monitor_copy_existing,
        [KernelCmd_Nullify_cap]  = monitor_nullify_cap,
        [KernelCmd_Setup_trace]  = handle_trace_setup,
        [KernelCmd_Register]     = monitor_handle_register,
        [KernelCmd_Domain_Id]    = monitor_handle_domain_id,
        [KernelCmd_Get_cap_owner] = monitor_get_cap_owner,
        [KernelCmd_Set_cap_owner] = monitor_set_cap_owner,
        [KernelCmd_Lock_cap]     = monitor_lock_cap,
        [KernelCmd_Unlock_cap]   = monitor_unlock_cap,
        [KernelCmd_Retype]       = monitor_handle_retype,
        [KernelCmd_Has_descendants] = monitor_handle_has_descendants,
        [KernelCmd_Delete_last]  = monitor_handle_delete_last,
        [KernelCmd_Delete_foreigns] = monitor_handle_delete_foreigns,
        [KernelCmd_Revoke_mark_target] = monitor_handle_revoke_mark_tgt,
        [KernelCmd_Revoke_mark_relations] = monitor_handle_revoke_mark_rels,
        [KernelCmd_Delete_step] = monitor_handle_delete_step,
        [KernelCmd_Clear_step] = monitor_handle_clear_step,
        [KernelCmd_Sync_timer]   = monitor_handle_sync_timer,
        [KernelCmd_IPI_Register] = kernel_ipi_register,
        [KernelCmd_IPI_Delete]   = kernel_ipi_delete,
        [KernelCmd_GetGlobalPhys] = kernel_get_global_phys,
        [KernelCmd_Add_kcb]      = kernel_add_kcb,
        [KernelCmd_Remove_kcb]   = kernel_remove_kcb,
        [KernelCmd_Suspend_kcb_sched]   = kernel_suspend_kcb_sched
    },
    [ObjType_IPI] = {
        [IPICmd_Send_Start] = kernel_send_start_ipi,
        [IPICmd_Send_Init] = kernel_send_init_ipi,
    },
    [ObjType_IRQDest] = {
        [IRQDestCmd_Connect] = handle_irqdest_connect,
        [IRQDestCmd_GetVector] = handle_irqdest_get_vector
    },
    [ObjType_IRQSrc] = {
        [IRQSrcCmd_GetVector] = handle_irqsrc_get_vector,
    },
    [ObjType_IRQTable] = {
        [IRQTableCmd_Alloc] = handle_irq_table_alloc,
        [IRQTableCmd_AllocDestCap] = handle_irq_table_alloc_dest_cap,
        [IRQTableCmd_Set] = handle_irq_table_set,
        [IRQTableCmd_Delete] = handle_irq_table_delete
    },
    [ObjType_IO] = {
        [IOCmd_Outb] = handle_io,
        [IOCmd_Outw] = handle_io,
        [IOCmd_Outd] = handle_io,
        [IOCmd_Inb] = handle_io,
        [IOCmd_Inw] = handle_io,
        [IOCmd_Ind] = handle_io
    },
    [ObjType_ID] = {
        [IDCmd_Identify] = handle_idcap_identify
    },
    [ObjType_Notify_IPI] = {
        [NotifyCmd_Send] = handle_ipi_notify_send
    }
};

/* syscall C entry point; called only from entry.S so no prototype in header */
struct sysret sys_syscall(uintptr_t arg0, uintptr_t arg1, uintptr_t *args,
                          uintptr_t *cpu_save_frame);
struct sysret sys_syscall(uintptr_t arg0, uintptr_t arg1, uintptr_t *args,
                          uintptr_t *cpu_save_frame)
{
    struct sysret retval = { .error = SYS_ERR_OK, .value = 0 };
    uint8_t syscall = arg0 & 0xff;

    switch(syscall) {
    case SYSCALL_INVOKE: ; /* Handle capability invocation */
        uint8_t flags = (arg0 >> 24) & 0xf;
        uint8_t invoke_bits = (arg0 >> 16) & 0xff;
        capaddr_t invoke_cptr = arg1;

        debug(SUBSYS_SYSCALL, "sys_invoke(0x%"PRIxCADDR"(%d))\n",
              invoke_cptr, invoke_bits);

        // Capability to invoke
        struct capability *to = NULL;
        retval.error = caps_lookup_cap(&dcb_current->cspace.cap, invoke_cptr,
                                       invoke_bits, &to, CAPRIGHTS_READ);
        if (err_is_fail(retval.error)) {
            break;
        }
        assert(to != NULL);
        assert(to->type < ObjType_Num);

        // Endpoint cap, do LMP
        if (to->type == ObjType_EndPoint) {
            struct dcb *listener = to->u.endpoint.listener;
            assert(listener != NULL);

            if (listener->disp == 0) {
                retval.error = SYS_ERR_LMP_NO_TARGET;
                break;
            }

            uint8_t length_words = (arg0 >> 28) & 0xf;
            uint8_t send_bits = (arg0 >> 8) & 0xff;
            capaddr_t send_cptr = args[0];

            /* limit length of message from buggy/malicious sender */
            length_words = min(length_words, LMP_MSG_LENGTH);

            // does the sender want to yield their timeslice on success?
            bool sync = flags & LMP_FLAG_SYNC;
            // does the sender want to yield to the target if undeliverable?
            bool yield = flags & LMP_FLAG_YIELD;
            // is the cap (if present) to be deleted on send?
            bool give_away = flags & LMP_FLAG_GIVEAWAY;

            // try to deliver message
            retval.error = lmp_deliver(to, dcb_current, &args[1], length_words,
                                       send_cptr, send_bits, give_away);

            /* Switch to reciever upon successful delivery with sync flag,
             * or (some cases of) unsuccessful delivery with yield flag */
            enum err_code err_code = err_no(retval.error);
            if ((sync && err_is_ok(retval.error)) ||
                (yield && (err_code == SYS_ERR_LMP_BUF_OVERFLOW
                           || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_LOOKUP
                           || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID
                           || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_SLOT_OCCUPIED))
                    ) {
                if (err_is_fail(retval.error)) {
                    struct dispatcher_shared_generic *current_disp =
                        get_dispatcher_shared_generic(dcb_current->disp);
                    struct dispatcher_shared_generic *listener_disp =
                        get_dispatcher_shared_generic(listener->disp);
                    debug(SUBSYS_DISPATCH, "LMP failed; %.*s yields to %.*s: %u\n",
                          DISP_NAME_LEN, current_disp->name,
                          DISP_NAME_LEN, listener_disp->name, err_code);
                }

                // special-case context switch: ensure correct state in current DCB
                dispatcher_handle_t handle = dcb_current->disp;
                struct dispatcher_shared_x86_32 *disp =
                    get_dispatcher_shared_x86_32(handle);
                dcb_current->disabled = dispatcher_is_disabled_ip(handle, cpu_save_frame[0]);
                struct registers_x86_32 *save_area;
                if (dcb_current->disabled) {
                    save_area = &disp->disabled_save_area;
                } else {
                    save_area = &disp->enabled_save_area;
                }

                // save calling dispatcher's registers, so that when the dispatcher
                // next runs, it has a valid state in the relevant save area.
                // Save EIP, EFLAGS, ESP and set EAX (return value) for later resume
                save_area->eax = retval.error; // x86 1st return register
                // save frame contains: eip, cs, eflags, esp, ss
                save_area->eip = cpu_save_frame[0];
                save_area->cs = cpu_save_frame[1];
                save_area->eflags = cpu_save_frame[2];
                save_area->esp = cpu_save_frame[3];
                save_area->ss = cpu_save_frame[4];

                /* save FS/GS selectors (they're unmodified by the syscall path) */
                __asm ("mov     %%fs, %[fs]     \n\t"
                       "mov     %%gs, %[gs]     \n\t"
                       : /* No output */
                       :
                       [fs] "m" (save_area->fs),
                       [gs] "m" (save_area->gs)
                       );

                dispatch(to->u.endpoint.listener);
                panic("dispatch returned");
            }
        } else { // not endpoint cap, call kernel handler through dispatch table
            uint8_t cmd = arg0 >> 8;
            if (cmd >= CAP_MAX_CMD) {
                retval.error = SYS_ERR_ILLEGAL_INVOCATION;
                break;
            }

            // Call the invocation
            invocation_handler_t invocation = invocations[to->type][cmd];
            if(invocation == NULL) {
                printf("No invocation handler for type = %d, cmd = %d\n", to->type, cmd);
                retval.error = SYS_ERR_ILLEGAL_INVOCATION;
                break;
            } else {
                retval = invocation(to, cmd, args);
            }
        }
        break;

        // Yield the CPU to the next dispatcher
    case SYSCALL_YIELD:
        retval = sys_yield((capaddr_t)arg1);
        break;

        // NOP system call for benchmarking purposes
    case SYSCALL_NOP:
        break;

        // Debug print system call
    case SYSCALL_PRINT:
        retval.error = sys_print((char *)arg1, args[0]);
        break;

        // Reboot!
        // FIXME: this should be a kernel cap invocation or similarly restricted
    case SYSCALL_REBOOT:
        reboot();
        break;

    case SYSCALL_DEBUG:
        switch(arg1) {
        case DEBUG_CONTEXT_COUNTER_RESET:
            dispatch_csc_reset();
            break;

        case DEBUG_CONTEXT_COUNTER_READ:
            retval.value = dispatch_get_csc();
            break;

        case DEBUG_TIMESLICE_COUNTER_READ:
            retval.value = kernel_now;
            break;

        case DEBUG_FLUSH_CACHE:
            wbinvd();
            break;

        case DEBUG_SEND_IPI:
            apic_send_std_ipi(args[0], args[1], args[2]);
            break;

        case DEBUG_SET_BREAKPOINT:
            debugregs_set_breakpoint(args[0], args[1], args[2]);
            break;

        case DEBUG_GET_TSC_PER_MS:
            retval.value = timing_get_tsc_per_ms();
            break;

        case DEBUG_FEIGN_FRAME_CAP:
            {
                uint8_t bits = args[2] & 0xff;
                uint8_t cap_bits = (args[2] >> 8) & 0xff;
                uint8_t recv_slot = (args[2] >> 16) & 0xff;
                struct cte *slot;
                struct capability *recv_cnode_cap;

/*                printf("arg1 = %" PRIx64 ", arg2 = %" PRIx64 "\n",
                        (uint64_t)args[1], (uint64_t)args[2]);
*/
                errval_t err = caps_lookup_cap(&dcb_current->cspace.cap,
                      args[0], cap_bits, &recv_cnode_cap, CAPRIGHTS_READ_WRITE);
                if(err_is_fail(err)) {
                    retval.error = err;
                    break;
                }

                // Check for cnode type
                if (recv_cnode_cap->type != ObjType_CNode) {
                    retval.error = SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID;
                    break;
                }
                // The slot within the cnode
                slot = caps_locate_slot(recv_cnode_cap->u.cnode.cnode,
                                        recv_slot);

                retval.error = caps_create_new(ObjType_DevFrame, args[1], bits,
                        bits, my_core_id, slot);
            }
            break;

        default:
            printk(LOG_ERR, "invalid sys_debug msg type\n");
        }
        break;

    default:
        printk(LOG_ERR, "sys_syscall: Illegal system call! "
               "(0x%x, 0x%"PRIxPTR")\n", syscall, arg1);
        retval.error = SYS_ERR_ILLEGAL_SYSCALL;
        break;
    }

    // If dcb_current got removed, dispatch someone else
    if (dcb_current == NULL) {
        assert(err_is_ok(retval.error));
        dispatch(schedule());
    }

    if (syscall == SYSCALL_INVOKE) {
        debug(SUBSYS_SYSCALL, "invoke returning 0x%"PRIxERRV" 0x%"PRIxPTR"\n",
              retval.error, retval.value);
    }

    return retval;
}
