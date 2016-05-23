/**
 * \file
 * \brief Kernel capability management implementation.
 */

/*
 * Copyright (c) 2007-2012,2015, ETH Zurich.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <kernel.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/lmp.h>
#include <offsets.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <distcaps.h>
#include <dispatch.h>
#include <kcb.h>
#include <paging_kernel_arch.h>
#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <wakeup.h>
#include <bitmacros.h>

#ifdef TRACE_PMEM_CAPS
uint64_t   trace_types_enabled = TRACE_TYPES_ENABLED_INITIAL;
genpaddr_t TRACE_PMEM_BEGIN    = TRACE_PMEM_BEGIN_INITIAL;
gensize_t  TRACE_PMEM_SIZE     = TRACE_PMEM_SIZE_INITIAL;

void caps_trace_ctrl(uint64_t types, genpaddr_t start, gensize_t size)
{
    if (types) {
        trace_types_enabled = types;
        TRACE_PMEM_BEGIN = start;
        TRACE_PMEM_SIZE = size;
    } else {
        trace_types_enabled = 0;
    }
}
#endif

struct capability monitor_ep;

STATIC_ASSERT(46 == ObjType_Num, "Knowledge of all cap types");
int sprint_cap(char *buf, size_t len, struct capability *cap)
{
    switch (cap->type) {
    case ObjType_PhysAddr:
        return snprintf(buf, len,
                        "physical address range cap (0x%" PRIxGENPADDR ":0x%zx)",
                        cap->u.physaddr.base, cap->u.physaddr.bytes);

    case ObjType_RAM:
        return snprintf(buf, len, "RAM cap (0x%" PRIxGENPADDR ":0x%zx)",
                        cap->u.ram.base, cap->u.ram.bytes);

    case ObjType_CNode: {
        int ret = snprintf(buf, len, "CNode cap "
                           "(bits %u, rights mask 0x%" PRIxCAPRIGHTS ")",
                           cap->u.cnode.bits, cap->u.cnode.rightsmask);
        if (cap->u.cnode.guard_size != 0 && ret < len) {
            ret += snprintf(&buf[ret], len - ret, " (guard 0x%" PRIxCADDR ":%u)",
                            cap->u.cnode.guard, cap->u.cnode.guard_size);
        }
        return ret;
    }

    case ObjType_Dispatcher:
        return snprintf(buf, len, "Dispatcher cap %p", cap->u.dispatcher.dcb);

    case ObjType_Frame:
        return snprintf(buf, len, "Frame cap (0x%" PRIxGENPADDR ":0x%zx)",
                        cap->u.frame.base, cap->u.frame.bytes);

    case ObjType_DevFrame:
        return snprintf(buf, len, "Device Frame cap (0x%" PRIxGENPADDR ":0x%zx)",
                        cap->u.frame.base, cap->u.devframe.bytes);

    case ObjType_VNode_ARM_l1:
        return snprintf(buf, len, "ARM L1 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_arm_l1.base);

    case ObjType_VNode_ARM_l2:
        return snprintf(buf, len, "ARM L2 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_arm_l2.base);

    case ObjType_VNode_AARCH64_l1:
        return snprintf(buf, len, "AARCH64 L1 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_aarch64_l1.base);

    case ObjType_VNode_AARCH64_l2:
        return snprintf(buf, len, "AARCH64 L2 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_aarch64_l2.base);

    case ObjType_VNode_AARCH64_l3:
        return snprintf(buf, len, "AARCH64 L3 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_aarch64_l3.base);

    case ObjType_VNode_x86_32_ptable:
        return snprintf(buf, len, "x86_32 Page table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_32_ptable.base);

    case ObjType_VNode_x86_32_pdir:
        return snprintf(buf, len, "x86_32 Page directory at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_32_pdir.base);

    case ObjType_VNode_x86_32_pdpt:
        return snprintf(buf, len, "x86_32 PDPT at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_32_pdpt.base);

    case ObjType_VNode_x86_64_ptable:
        return snprintf(buf, len, "x86_64 Page table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_ptable.base);

    case ObjType_VNode_x86_64_pdir:
        return snprintf(buf, len, "x86_64 Page directory at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_pdir.base);

    case ObjType_VNode_x86_64_pdpt:
        return snprintf(buf, len, "x86_64 PDPT at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_pdpt.base);

    case ObjType_VNode_x86_64_pml4:
        return snprintf(buf, len, "x86_64 PML4 at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_pml4.base);

    case ObjType_Frame_Mapping:
        return snprintf(buf, len, "Frame Mapping (Frame cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.frame_mapping.frame,
                                  cap->u.frame_mapping.pte,
                                  cap->u.frame_mapping.pte_count);

    case ObjType_DevFrame_Mapping:
        return snprintf(buf, len, "DevFrame Mapping (DevFrame cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.devframe_mapping.frame,
                                  cap->u.devframe_mapping.pte,
                                  cap->u.devframe_mapping.pte_count);

    case ObjType_VNode_x86_64_pml4_Mapping:
        return snprintf(buf, len, "x86_64 PML4 Mapping (x86_64 PML4 cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_x86_64_pml4_mapping.frame,
                                  cap->u.vnode_x86_64_pml4_mapping.pte,
                                  cap->u.vnode_x86_64_pml4_mapping.pte_count);

    case ObjType_VNode_x86_64_pdpt_Mapping:
        return snprintf(buf, len, "x86_64 PDPT Mapping (x86_64 PDPT cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_x86_64_pdpt_mapping.frame,
                                  cap->u.vnode_x86_64_pdpt_mapping.pte,
                                  cap->u.vnode_x86_64_pdpt_mapping.pte_count);

    case ObjType_VNode_x86_64_pdir_Mapping:
        return snprintf(buf, len, "x86_64 PDIR Mapping (x86_64 PDIR cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_x86_64_pdir_mapping.frame,
                                  cap->u.vnode_x86_64_pdir_mapping.pte,
                                  cap->u.vnode_x86_64_pdir_mapping.pte_count);

    case ObjType_VNode_x86_64_ptable_Mapping:
        return snprintf(buf, len, "x86_64 PTABLE Mapping (x86_64 PTABLE cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_x86_64_ptable_mapping.frame,
                                  cap->u.vnode_x86_64_ptable_mapping.pte,
                                  cap->u.vnode_x86_64_ptable_mapping.pte_count);

    case ObjType_VNode_x86_32_pdpt_Mapping:
        return snprintf(buf, len, "x86_32 PDPT Mapping (x86_32 PDPT cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_x86_32_pdpt_mapping.frame,
                                  cap->u.vnode_x86_32_pdpt_mapping.pte,
                                  cap->u.vnode_x86_32_pdpt_mapping.pte_count);

    case ObjType_VNode_x86_32_pdir_Mapping:
        return snprintf(buf, len, "x86_32 PDIR Mapping (x86_32 PDIR cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_x86_32_pdir_mapping.frame,
                                  cap->u.vnode_x86_32_pdir_mapping.pte,
                                  cap->u.vnode_x86_32_pdir_mapping.pte_count);

    case ObjType_VNode_x86_32_ptable_Mapping:
        return snprintf(buf, len, "x86_32 PTABLE Mapping (x86_32 PTABLE cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_x86_32_ptable_mapping.frame,
                                  cap->u.vnode_x86_32_ptable_mapping.pte,
                                  cap->u.vnode_x86_32_ptable_mapping.pte_count);

    case ObjType_VNode_ARM_l1_Mapping:
        return snprintf(buf, len, "ARM l1 Mapping (ARM l1 cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_arm_l1_mapping.frame,
                                  cap->u.vnode_arm_l1_mapping.pte,
                                  cap->u.vnode_arm_l1_mapping.pte_count);

    case ObjType_VNode_ARM_l2_Mapping:
        return snprintf(buf, len, "ARM l2 Mapping (ARM l2 cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_arm_l2_mapping.frame,
                                  cap->u.vnode_arm_l2_mapping.pte,
                                  cap->u.vnode_arm_l2_mapping.pte_count);

    case ObjType_VNode_AARCH64_l1_Mapping:
        return snprintf(buf, len, "AARCH64 l1 Mapping (AARCH64 l1 cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_aarch64_l1_mapping.frame,
                                  cap->u.vnode_aarch64_l1_mapping.pte,
                                  cap->u.vnode_aarch64_l1_mapping.pte_count);

    case ObjType_VNode_AARCH64_l2_Mapping:
        return snprintf(buf, len, "AARCH64 l2 Mapping (AARCH64 l2 cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_aarch64_l2_mapping.frame,
                                  cap->u.vnode_aarch64_l2_mapping.pte,
                                  cap->u.vnode_aarch64_l2_mapping.pte_count);

    case ObjType_VNode_AARCH64_l3_Mapping:
        return snprintf(buf, len, "AARCH64 l3 Mapping (AARCH64 l3 cap @%p, "
                                  "pte @0x%"PRIxLVADDR", pte_count=%hu)",
                                  cap->u.vnode_aarch64_l3_mapping.frame,
                                  cap->u.vnode_aarch64_l3_mapping.pte,
                                  cap->u.vnode_aarch64_l3_mapping.pte_count);

    case ObjType_IRQTable:
        return snprintf(buf, len, "IRQTable cap");

    case ObjType_IRQDest:
        return snprintf(buf, len, "IRQDest cap (vec: %"PRIu64", ctrl: %"PRIu64")",
                cap->u.irqdest.vector, cap->u.irqdest.controller);

    case ObjType_EndPoint:
        return snprintf(buf, len, "EndPoint cap (disp %p offset 0x%" PRIxLVADDR ")",
                        cap->u.endpoint.listener, cap->u.endpoint.epoffset);

    case ObjType_IO:
        return snprintf(buf, len, "IO cap (0x%hx-0x%hx)",
                        cap->u.io.start, cap->u.io.end);

    case ObjType_Kernel:
        return snprintf(buf, len, "Kernel cap");

    case ObjType_ID:
        return snprintf(buf, len, "ID capability (coreid 0x%" PRIxCOREID
                        " core_local_id 0x%" PRIx32 ")", cap->u.id.coreid,
                        cap->u.id.core_local_id);

    case ObjType_PerfMon:
        return snprintf(buf, len, "PerfMon cap");

    case ObjType_Null:
        return snprintf(buf, len, "Null capability (empty slot)");

    case ObjType_IPI:
        return snprintf(buf, len, "IPI cap");

    default:
        return snprintf(buf, len, "UNKNOWN TYPE! (%d)", cap->type);
    }
}

void caps_trace(const char *func, int line, struct cte *cte, const char *msg)
{
    char cap_buf[512];
    sprint_cap(cap_buf, 512, &cte->cap);

    char disp_buf[64];
    if (dcb_current) {
        dispatcher_handle_t handle = dcb_current->disp;
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(handle);
        snprintf(disp_buf, 64, "from %.*s", DISP_NAME_LEN, disp->name);
    }
    else {
        strcpy(disp_buf, "no disp");
    }

    printk(LOG_WARN, "%s: %s:%d: %s %p %s"
           " (owner:%" PRIuCOREID ", rc:%d/ra:%d/rd:%d)\n",
           disp_buf, func, line, (msg ? : ""), cte, cap_buf, cte->mdbnode.owner,
           cte->mdbnode.remote_copies, cte->mdbnode.remote_ancs,
           cte->mdbnode.remote_descs);
}

/**
 * ID capability core_local_id counter.
 */
static uint32_t id_cap_counter = 1;

/**
 *  Sets #dest equal to #src
 *
 * #dest cannot be in use.
 */
static errval_t set_cap(struct capability *dest, struct capability *src)
{
    /* Parameter checking */
    assert(src  != NULL);
    assert(dest != NULL);

    debug(SUBSYS_CAPS, "Copying cap from %#"PRIxLPADDR" to %#"PRIxLPADDR"\n",
            mem_to_local_phys((lvaddr_t)cte_for_cap(src)),
            mem_to_local_phys((lvaddr_t)cte_for_cap(dest)));

    // Reserved object bits must always be greater/equal to actual object size
    assert((1UL << OBJBITS_CTE) >= sizeof(struct cte));

    // Cannot overwrite an already existing cap
    if (dest->type != ObjType_Null) {
        return SYS_ERR_SLOT_IN_USE;
    }

    memcpy(dest, src, sizeof(struct capability));
    return SYS_ERR_OK;
}

/**
 * \brief Determine how many objects can be created in a specified region.
 *
 * This function computes the number of objects that can be created by a call
 * to caps_create().
 *
 * \param type          Type of objects to create.
 * \param srcsize       Size of memory area in bytes
 * \param objsize       For variable-sized objects, size multiplier
 *
 * \return Number of objects to be created, or zero on error
 */

// If you create more capability types you need to deal with them
// in the table below.
STATIC_ASSERT(46 == ObjType_Num, "Knowledge of all cap types");
static size_t caps_max_numobjs(enum objtype type, gensize_t srcsize, gensize_t objsize)
{
    switch(type) {
    case ObjType_PhysAddr:
    case ObjType_RAM:
    case ObjType_Frame:
    case ObjType_DevFrame:
        if (objsize > srcsize) {
            return 0;
        } else {
            return srcsize / objsize;
        }

    case ObjType_CNode:
        if (srcsize < sizeof(struct cte) || objsize > (srcsize / sizeof(struct cte))) {
            return 0;
        } else {
            return srcsize / objsize / (1UL << OBJBITS_CTE);
        }

    case ObjType_VNode_x86_64_pml4:
    case ObjType_VNode_x86_64_pdpt:
    case ObjType_VNode_x86_64_pdir:
    case ObjType_VNode_x86_64_ptable:
    case ObjType_VNode_x86_32_pdpt:
    case ObjType_VNode_x86_32_pdir:
    case ObjType_VNode_x86_32_ptable:
    case ObjType_VNode_ARM_l1:
    case ObjType_VNode_ARM_l2:
    case ObjType_VNode_AARCH64_l1:
    case ObjType_VNode_AARCH64_l2:
    case ObjType_VNode_AARCH64_l3:
    {
        if (srcsize < vnode_objsize(type)) {
            return 0;
        } else {
            return srcsize / vnode_objsize(type);
        }
    }

    case ObjType_Dispatcher:
        if (srcsize < 1UL << OBJBITS_DISPATCHER) {
            return 0;
        } else {
            return srcsize / (1UL << OBJBITS_DISPATCHER);
        }

    case ObjType_KernelControlBlock:
        if (srcsize < 1UL << OBJBITS_KCB) {
            return 0;
        } else {
            return srcsize / (1UL << OBJBITS_KCB);
        }

    case ObjType_Kernel:
    case ObjType_IRQTable:
    case ObjType_IRQDest:
    case ObjType_IRQSrc:
    case ObjType_IO:
    case ObjType_EndPoint:
    case ObjType_ID:
    case ObjType_Notify_RCK:
    case ObjType_Notify_IPI:
    case ObjType_PerfMon:
    case ObjType_IPI:
    case ObjType_VNode_ARM_l1_Mapping:
    case ObjType_VNode_ARM_l2_Mapping:
    case ObjType_VNode_AARCH64_l1_Mapping:
    case ObjType_VNode_AARCH64_l2_Mapping:
    case ObjType_VNode_AARCH64_l3_Mapping:
    case ObjType_VNode_x86_64_pml4_Mapping:
    case ObjType_VNode_x86_64_pdpt_Mapping:
    case ObjType_VNode_x86_64_pdir_Mapping:
    case ObjType_VNode_x86_64_ptable_Mapping:
    case ObjType_VNode_x86_32_pdpt_Mapping:
    case ObjType_VNode_x86_32_pdir_Mapping:
    case ObjType_VNode_x86_32_ptable_Mapping:
    case ObjType_DevFrame_Mapping:
    case ObjType_Frame_Mapping:
        return 1;

    default:
        panic("invalid type");
        return 0;
    }
}

/**
 * \brief Initialize the objects for which local caps are about to be created.
 *
 * For the meaning of the parameters, see the 'caps_create' function.
 */
STATIC_ASSERT(46 == ObjType_Num, "Knowledge of all cap types");

static errval_t caps_zero_objects(enum objtype type, lpaddr_t lpaddr,
                                  gensize_t objsize, size_t count)
{
    assert(type < ObjType_Num);

    // Virtual address of the memory the kernel object resides in
    // XXX: A better of doing this,
    // this is creating caps that the kernel cannot address.
    // It assumes that the cap is not of the type which will have to zeroed out.
    lvaddr_t lvaddr;
    if(lpaddr < PADDR_SPACE_LIMIT) {
        lvaddr = local_phys_to_mem(lpaddr);
    } else {
        lvaddr = 0;
    }

    switch (type) {

    case ObjType_Frame:
        debug(SUBSYS_CAPS, "Frame: zeroing %zu bytes @%#"PRIxLPADDR"\n",
                (size_t)objsize * count, lpaddr);
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, objsize * count);
        TRACE(KERNEL, BZERO, 0);
        break;

    case ObjType_CNode:
        // scale objsize by size of slot for CNodes; objsize for CNodes given
        // in slots.
        objsize *= sizeof(struct cte);
        debug(SUBSYS_CAPS, "CNode: zeroing %zu bytes @%#"PRIxLPADDR"\n",
                (size_t)objsize * count, lpaddr);
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, objsize * count);
        TRACE(KERNEL, BZERO, 0);
        break;

    case ObjType_VNode_ARM_l1:
    case ObjType_VNode_ARM_l2:
    case ObjType_VNode_AARCH64_l1:
    case ObjType_VNode_AARCH64_l2:
    case ObjType_VNode_AARCH64_l3:
    case ObjType_VNode_x86_32_ptable:
    case ObjType_VNode_x86_32_pdir:
    case ObjType_VNode_x86_32_pdpt:
    case ObjType_VNode_x86_64_ptable:
    case ObjType_VNode_x86_64_pdir:
    case ObjType_VNode_x86_64_pdpt:
    case ObjType_VNode_x86_64_pml4:
        // objsize is size of VNode; but not given as such
        objsize = vnode_objsize(type);
        debug(SUBSYS_CAPS, "VNode: zeroing %zu bytes @%#"PRIxLPADDR"\n",
                (size_t)objsize * count, lpaddr);
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, objsize * count);
        TRACE(KERNEL, BZERO, 0);
        break;

    case ObjType_Dispatcher:
        debug(SUBSYS_CAPS, "Dispatcher: zeroing %zu bytes @%#"PRIxLPADDR"\n",
                ((size_t)1 << OBJBITS_DISPATCHER) * count, lpaddr);
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, (1UL << OBJBITS_DISPATCHER) * count);
        TRACE(KERNEL, BZERO, 0);
        break;

    case ObjType_KernelControlBlock:
        debug(SUBSYS_CAPS, "KCB: zeroing %zu bytes @%#"PRIxLPADDR"\n",
                ((size_t)1 << OBJBITS_KCB) * count, lpaddr);
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, (1UL << OBJBITS_KCB) * count);
        TRACE(KERNEL, BZERO, 0);
        break;

    default:
        debug(SUBSYS_CAPS, "Not zeroing %zu bytes @%#"PRIxLPADDR" for type %d\n",
                (size_t)objsize * count, lpaddr, (int)type);
        break;

    }

    return SYS_ERR_OK;
}

/**
 * \brief Create capabilities to kernel objects.
 *
 * This function creates 'count' kernel objects of 'type' into the memory
 * area, based at 'addr' and of size 'objsize'. For each created kernel
 * object, a capability is created to it and put consecutively into the array
 * of CTEs pointed to by 'caps'. The array needs to have the appropriate size
 * to hold all created caps. Some kernel objects can have a variable size. In
 * that case, 'objsize' should be non-zero. and give the size multiplier. *
 *
 * \param type          Type of objects to create.
 * \param lpaddr        Base address in the local address space.
 * \param size          Size of memory area as bytes.
 * \param objsize       For variable-sized objects, size in bytes.
 * \param count         Number of objects to be created
 *                      (count <= caps_max_numobjs(type, size, objsize))
 * \param dest_caps     Pointer to array of CTEs to hold created caps.
 *
 * \return Error code
 */
// If you create more capability types you need to deal with them
// in the table below.
STATIC_ASSERT(46 == ObjType_Num, "Knowledge of all cap types");

static errval_t caps_create(enum objtype type, lpaddr_t lpaddr, gensize_t size,
                            gensize_t objsize, size_t count, coreid_t owner,
                            struct cte *dest_caps)
{
    errval_t err;

    /* Parameter checking */
    assert(dest_caps != NULL);
    assert(type != ObjType_Null);
    assert(type < ObjType_Num);
    assert(count > 0);
    // objsize is 0 for non-sized types (e.g. VNodes)
    // TODO cleanup semantics for type == CNode
    //assert(objsize % BASE_PAGE_SIZE == 0);
    assert(!type_is_mapping(type));

    genpaddr_t genpaddr = local_phys_to_gen_phys(lpaddr);

    debug(SUBSYS_CAPS, "creating caps for %#"PRIxGENPADDR
                       ", %zu bytes, objsize=%"PRIuGENSIZE
                       ", count=%zu, owner=%d, type=%d\n",
            genpaddr, size, objsize, count, (int)owner, (int)type);

    // Virtual address of the memory the kernel object resides in
    // XXX: A better of doing this,
    // this is creating caps that the kernel cannot address.
    // It assumes that the cap is not of the type which will have to zeroed out.
    lvaddr_t lvaddr;
    if(lpaddr < PADDR_SPACE_LIMIT) {
        lvaddr = local_phys_to_mem(lpaddr);
    } else {
        lvaddr = 0;
    }

    /* Initialize the created capability */
    struct capability temp_cap;
    memset(&temp_cap, 0, sizeof(struct capability));
    temp_cap.type = type;
    // XXX: Handle rights!
    temp_cap.rights = CAPRIGHTS_ALLRIGHTS;

    debug(SUBSYS_CAPS, "owner = %d, my_core_id = %d\n", owner, my_core_id);
    if (owner == my_core_id) {
        // If we're creating new local objects, they need to be cleared
        err = caps_zero_objects(type, lpaddr, objsize, count);
        if (err_is_fail(err)) {
            return err;
        }
    }

    size_t dest_i = 0;
    err = SYS_ERR_OK;

    /* Set the type specific fields and insert into #dest_caps */
    switch(type) {
    case ObjType_Frame:
        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.frame.base = genpaddr + dest_i * objsize;
            temp_cap.u.frame.bytes = objsize;
            assert((get_size(&temp_cap) & BASE_PAGE_MASK) == 0);
            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_PhysAddr:
        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.physaddr.base = genpaddr + dest_i * objsize;
            temp_cap.u.physaddr.bytes = objsize;
            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_RAM:
        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.ram.base = genpaddr + dest_i * objsize;
            temp_cap.u.ram.bytes = objsize;
            // Insert the capabilities
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_DevFrame:
        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.devframe.base = genpaddr + dest_i * objsize;
            temp_cap.u.devframe.bytes = objsize;
            // Insert the capabilities
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_CNode:
        assert((1UL << OBJBITS_CTE) >= sizeof(struct cte));
        // TODO: make CNodes not be power-of-two sized
        // (deferred to new CSpace layout)
        assert((1UL << log2cl(objsize)) == objsize);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.cnode.cnode =
                lpaddr + dest_i * sizeof(struct cte) * objsize;
            temp_cap.u.cnode.bits = log2cl(objsize);
            temp_cap.u.cnode.guard = 0;
            temp_cap.u.cnode.guard_size = 0;
            // XXX: Handle rights!
            temp_cap.u.cnode.rightsmask = CAPRIGHTS_ALLRIGHTS;
            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_VNode_ARM_l1:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_arm_l1.base =
                genpaddr + dest_i * objsize_vnode;

#ifdef __arm__
            // Insert kernel/mem mappings into new table.
            paging_make_good(
                gen_phys_to_local_phys(
                    local_phys_to_mem(temp_cap.u.vnode_arm_l1.base)
                ),
                objsize_vnode
                );
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }

        break;
    }

    case ObjType_VNode_ARM_l2:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_arm_l2.base =
                genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_AARCH64_l1:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_aarch64_l1.base =
                genpaddr + dest_i * objsize_vnode;

#ifdef __aarch64__
            // Insert kernel/mem mappings into new table.
            lpaddr_t var = gen_phys_to_local_phys(temp_cap.u.vnode_aarch64_l1.base);
            paging_make_good(var);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }

        break;
    }

    case ObjType_VNode_AARCH64_l2:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_aarch64_l2.base =
                genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);

            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_AARCH64_l3:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_aarch64_l3.base =
                genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_32_ptable:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_x86_32_ptable.base =
                genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_32_pdir:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_x86_32_pdir.base =
                genpaddr + dest_i * objsize_vnode;

#if defined(__i386__) && !defined(CONFIG_PAE)
            // Make it a good PDE by inserting kernel/mem VSpaces
            lpaddr = gen_phys_to_local_phys(temp_cap.u.vnode_x86_32_pdir.base);
            paging_x86_32_make_good_pdir(lpaddr);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_32_pdpt:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_x86_32_pdir.base =
                genpaddr + dest_i * objsize_vnode;

#if defined(__i386__) && defined(CONFIG_PAE)
            // Make it a good PDPTE by inserting kernel/mem VSpaces
            lpaddr_t var =
                gen_phys_to_local_phys(temp_cap.u.vnode_x86_32_pdpt.base);
            paging_x86_32_make_good_pdpte(var);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_64_ptable:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_x86_64_ptable.base =
                genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_64_pdir:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_x86_64_pdir.base =
                genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_64_pdpt:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_x86_64_pdpt.base =
                genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_64_pml4:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_x86_64_pml4.base =
                genpaddr + dest_i * objsize_vnode;

#if defined(__x86_64__) || defined(__k1om__)
            // Make it a good PML4 by inserting kernel/mem VSpaces
            lpaddr_t var = gen_phys_to_local_phys(temp_cap.u.vnode_x86_64_pml4.base);
            paging_x86_64_make_good_pml4(var);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }

        break;
    }

    case ObjType_Dispatcher:
        assert((1UL << OBJBITS_DISPATCHER) >= sizeof(struct dcb));

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.dispatcher.dcb = (struct dcb *)
                (lvaddr + dest_i * (1UL << OBJBITS_DISPATCHER));
            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_ID:
        // ID type does not refer to a kernel object
        assert(lpaddr  == 0);
        assert(size    == 0);
        assert(objsize == 0);
        assert(count   == 1);

        // Prevent wrap around
        if (id_cap_counter >= UINT32_MAX) {
            return SYS_ERR_ID_SPACE_EXHAUSTED;
        }

        // Generate a new ID, core_local_id monotonically increases
        temp_cap.u.id.coreid = my_core_id;
        temp_cap.u.id.core_local_id = id_cap_counter++;

        // Insert the capability
        err = set_cap(&dest_caps->cap, &temp_cap);
        break;

    case ObjType_IO:
        temp_cap.u.io.start = 0;
        temp_cap.u.io.end   = 65535;
        /* fall through */

    case ObjType_Kernel:
    case ObjType_IPI:
    case ObjType_IRQTable:
    case ObjType_IRQDest:
    case ObjType_IRQSrc:
    case ObjType_EndPoint:
    case ObjType_Notify_RCK:
    case ObjType_Notify_IPI:
    case ObjType_PerfMon:
        // These types do not refer to a kernel object
        assert(lpaddr  == 0);
        assert(size    == 0);
        assert(objsize == 0);
        assert(count   == 1);

        // Insert the capability
        err = set_cap(&dest_caps->cap, &temp_cap);
        if (err_is_ok(err)) {
            dest_i = 1;
        }
        break;

    case ObjType_KernelControlBlock:
        assert((1UL << OBJBITS_KCB) >= sizeof(struct dcb));

        for(size_t i = 0; i < count; i++) {
            // Initialize type specific fields
            temp_cap.u.kernelcontrolblock.kcb = (struct kcb *)
                (lvaddr + i * (1UL << OBJBITS_KCB));
            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &temp_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;

    default:
        panic("Unhandled capability type or capability of this type cannot"
              " be created");
    }

    if (err_is_fail(err)) {
        // Revert the partially initialized caps to zero
        for (size_t i = 0; i < dest_i; i++) {
            memset(&dest_caps[i], 0, sizeof(dest_caps[i]));
        }
        return err;
    }
    else {
        // Set the owner for all the new caps
        for (size_t i = 0; i < dest_i; i++) {
            dest_caps[i].mdbnode.owner = owner;
        }
    }

    return SYS_ERR_OK;
}

/**
 * Look up a capability.
 *
 * Starting from #cnode_cap, recursively lookup the capability at #cptr
 * with #vbits.
 *
 * \bug Handle rights
 */
errval_t caps_lookup_slot(struct capability *cnode_cap, capaddr_t cptr,
                          uint8_t vbits, struct cte **ret, CapRights rights)
{
    TRACE(KERNEL, CAP_LOOKUP_SLOT, 0);
    /* parameter checking */
    assert(cnode_cap != NULL);

    /* Can only resolve CNode type */
    if (cnode_cap->type != ObjType_CNode) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: Cap to lookup not of type CNode\n"
              "cnode_cap->type = %u\n", cnode_cap->type);
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_CNODE_TYPE;
    }

    /* Apply rights to this CNode */
    if ((cnode_cap->rights & rights) != rights) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: Rights mismatch\n"
              "Passed rights = %u, cnode_cap->rights = %u\n",
              rights, cnode_cap->rights);
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_CNODE_RIGHTS;
    }

    /* Number of bits resolved by this cnode (guard and bits) */
    uint8_t bits_resolved = cnode_cap->u.cnode.bits +
        cnode_cap->u.cnode.guard_size;
    // All CNodes must resolve at least one bit
    assert(bits_resolved > 0);
    // If lookup exceeded expected depth then table is malformed
    if (bits_resolved > vbits) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: Lookup exceeded valid bits\n"
              "Cnode bits = %u, guard size = %u, valid bits = %u, bits_resolved = %u\n",
              cnode_cap->u.cnode.bits, cnode_cap->u.cnode.guard_size,
              vbits, bits_resolved);
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_DEPTH_EXCEEDED;
    }

    /* Guard-check (bit-mask of guard in cptr must match guard in cnode cap) */
    capaddr_t cptr_guard = (cptr >> (vbits - cnode_cap->u.cnode.guard_size))
        & MASK(cnode_cap->u.cnode.guard_size);
    if (cptr_guard != cnode_cap->u.cnode.guard) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: guard check failed\n"
              "Computed guard = %"PRIuCADDR", "
              "Cnode guard = %"PRIxCADDR", bits = %u\n",
              cptr_guard, cnode_cap->u.cnode.guard,
              cnode_cap->u.cnode.guard_size);
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_GUARD_MISMATCH;
    }

    /* Locate capability in this cnode */
    // Offset into the cnode
    size_t offset = (cptr >> (vbits - bits_resolved)) &
        MASK(cnode_cap->u.cnode.bits);
    // The capability at the offset
    struct cte *next_slot = caps_locate_slot(cnode_cap->u.cnode.cnode, offset);
    // Do not return NULL type capability
    if (next_slot->cap.type == ObjType_Null) {
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_CAP_NOT_FOUND;
    }

    /* Number of bits left to resolve */
    int bitsleft = vbits - bits_resolved;
    // If all bits have been resolved, return the capability
    if(bitsleft == 0) {
        *ret = next_slot;
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_OK;
    }

    /* If next capability is not of type cnode, return it */
    // XXX: Is this consistent?
    if (next_slot->cap.type != ObjType_CNode) {
        *ret = next_slot;
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_OK;
    }

    /* Descend to next level */
    return caps_lookup_slot(&next_slot->cap, cptr, bitsleft, ret, rights);
}

/**
 * Wrapper for caps_lookup_slot returning capability instead of cte.
 */
errval_t caps_lookup_cap(struct capability *cnode_cap, capaddr_t cptr,
                         uint8_t vbits, struct capability **ret, CapRights rights)
{
    TRACE(KERNEL, CAP_LOOKUP_CAP, 0);
    struct cte *ret_cte;
    errval_t err = caps_lookup_slot(cnode_cap, cptr, vbits, &ret_cte, rights);
    if (err_is_fail(err)) {
        return err;
    }
    *ret = &ret_cte->cap;
    TRACE(KERNEL, CAP_LOOKUP_CAP, 1);
    return SYS_ERR_OK;
}

/**
 * \brief Create a capability from an existing capability metadata.
 *
 * Used when sending capabilities across cores. The metadata is sent across
 * cores and the receiving monitor can create the new capability on its core.
 *
 * \bug Does not check that supplied owner matches existing copies of cap.
 */
errval_t caps_create_from_existing(struct capability *root, capaddr_t cnode_cptr,
                                   int cnode_vbits, cslot_t dest_slot, coreid_t owner,
                                   struct capability *src)
{
    TRACE(KERNEL, CAP_CREATE_FROM_EXISTING, 0);
    errval_t err;
    struct capability *cnode;
    err = caps_lookup_cap(root, cnode_cptr, cnode_vbits, &cnode,
                          CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_SLOT_LOOKUP_FAIL);
    }
    if (cnode->type != ObjType_CNode) {
        return SYS_ERR_CNODE_TYPE;
    }

    struct cte *dest = caps_locate_slot(cnode->u.cnode.cnode, dest_slot);

    err = set_cap(&dest->cap, src);
    if (err_is_fail(err)) {
        return err;
    }

    dest->mdbnode.owner = owner;

    err = mdb_insert(dest);
    assert(err_is_ok(err));

    struct cte *neighbour = NULL;
    if (!neighbour
        && (neighbour = mdb_predecessor(dest))
        && !is_copy(&dest->cap, &neighbour->cap))
    {
        neighbour = NULL;
    }
    if (!neighbour
        && (neighbour = mdb_successor(dest))
        && !is_copy(&dest->cap, &neighbour->cap))
    {
        neighbour = NULL;
    }

    if (neighbour) {
        assert(!neighbour->mdbnode.in_delete);
        assert(neighbour->mdbnode.owner == owner);
#define CP_ATTR(a) dest->mdbnode.a = neighbour->mdbnode.a
        CP_ATTR(locked);
        CP_ATTR(remote_copies);
        CP_ATTR(remote_ancs);
        CP_ATTR(remote_descs);
#undef CP_ATTR
    }
    else {
        dest->mdbnode.locked = false;
        if (owner != my_core_id) {
            // For foreign caps it does not really matter if ancestors or
            // descendants exist
            dest->mdbnode.remote_copies = true;
            dest->mdbnode.remote_ancs = false;
            dest->mdbnode.remote_descs = false;
        }
        else {
            // We just created a new copy of a owned capability from nothing.
            // This is either caused by a retype, or by sharing a capability
            // that does not care about locality.
            // XXX: This should probably be done more explicitly -MN
            if (distcap_needs_locality(dest->cap.type)) {
                // Retype, so have ancestors and no descendants
                dest->mdbnode.remote_copies = false;
                dest->mdbnode.remote_ancs = true;
                dest->mdbnode.remote_descs = false;
            }
            else {
                dest->mdbnode.remote_copies = false;
                dest->mdbnode.remote_ancs = false;
                dest->mdbnode.remote_descs = false;
            }
        }
    }

    TRACE_CAP_MSG("created", dest);

    TRACE(KERNEL, CAP_CREATE_FROM_EXISTING, 1);
    return SYS_ERR_OK;
}

//{{{1 Capability creation

/// check arguments, return true iff ok
static bool check_arguments(enum objtype type, size_t bytes, size_t objsize, bool exact)
{
    /* special case Dispatcher which is 1kB right now */
    if (type == ObjType_Dispatcher) {
        if (bytes & 0x3FF) {
            return false;
        }
        if (objsize > 0 && objsize != 1UL << OBJBITS_DISPATCHER) {
            return false;
        }

        if (exact && bytes % (1UL << OBJBITS_DISPATCHER)) {
            return false;
        }

        return true;
    }

    /* Adjust objsize to be in bytes for CNodes */
    if (type == ObjType_CNode) {
        objsize *= sizeof(struct cte);
    }

    /* source size not multiple of BASE_PAGE_SIZE */
    if (bytes & BASE_PAGE_MASK) {
        debug(SUBSYS_CAPS, "source size not multiple of BASE_PAGE_SIZE\n");
        return false;
    }
    /* objsize > 0 and not multiple of BASE_PAGE_SIZE */
    if (objsize > 0 && objsize & BASE_PAGE_MASK) {
        debug(SUBSYS_CAPS, "object size not multiple of BASE_PAGE_SIZE\n");
        return false;
    }

    /* check that bytes can be evenly divided into objsize sized chunks */
    if (exact && bytes > 0 && objsize > 0) {
        if (bytes % objsize) {
            debug(SUBSYS_CAPS, "source size cannot be evenly divided into object size-sized chunks\n");
        }
        return bytes % objsize == 0;
    }

    return true;
}

/** Create caps to new kernel objects.
 * This takes the size of the memory region in bytes, and the size of
 * individual objects in bytes. The following needs to hold:
 *      bytes % objbytes == 0
 */
errval_t caps_create_new(enum objtype type, lpaddr_t addr, size_t bytes,
                         size_t objsize, coreid_t owner, struct cte *caps)
{
    TRACE(KERNEL, CAP_CREATE_NEW, 0);
    /* Parameter checking */
    assert(type != ObjType_EndPoint); // Cap of this type cannot be created
    debug(SUBSYS_CAPS, "caps_create_new: type = %d, addr = %#"PRIxLPADDR
            ", bytes=%zu, objsize=%zu\n", type, addr, bytes, objsize);
    assert(check_arguments(type, bytes, objsize, false));
    assert(addr == 0 || check_arguments(type, bytes, objsize, true));

    size_t numobjs = caps_max_numobjs(type, bytes, objsize);
    assert(numobjs > 0);
    // XXX: Dispatcher creation is kind of hacky right now :(
    // Consider allowing non-mappable types to be < BASE_PAGE_SIZE
    if (type == ObjType_Dispatcher) {
        numobjs = 1;
    }

    /* Create the new capabilities */
    errval_t err = caps_create(type, addr, bytes, objsize, numobjs, owner, caps);
    if (err_is_fail(err)) {
        return err;
    }

    // Handle the mapping database
    set_init_mapping(caps, numobjs);

    TRACE_CAP_MSG("created", &caps[0]);

    TRACE(KERNEL, CAP_CREATE_NEW, 1);
    return SYS_ERR_OK;
}

STATIC_ASSERT(46 == ObjType_Num, "Knowledge of all cap types");
/// Retype caps
/// Create `count` new caps of `type` from `offset` in src, and put them in
/// `dest_cnode` starting at `dest_slot`.
/// Note: currently objsize is in slots for type == ObjType_CNode
errval_t caps_retype(enum objtype type, gensize_t objsize, size_t count,
                     struct capability *dest_cnode, cslot_t dest_slot,
                     struct cte *src_cte, gensize_t offset,
                     bool from_monitor)
{
    // TODO List for this:
    //  * do not complain if there's non-overlapping descendants,
    //    only complain about overlapping descendants
    TRACE(KERNEL, CAP_RETYPE, 0);
    size_t maxobjs;
    genpaddr_t base = 0;
    gensize_t size = 0;
    errval_t err;
    bool do_range_check = false;

    /* Parameter checking */
    assert(type != ObjType_Null);
    assert(type < ObjType_Num);
    if (type == ObjType_Null || type >= ObjType_Num) {
        return SYS_ERR_INVALID_RETYPE;
    }

    /* check that offset into source cap is multiple of BASE_PAGE_SIZE */
    if (offset % BASE_PAGE_SIZE != 0) {
        return SYS_ERR_RETYPE_INVALID_OFFSET;
    }
    assert(offset % BASE_PAGE_SIZE == 0);

    // check that size is multiple of BASE_PAGE_SIZE
    // (or zero, for fixed-size types)
    if (type != ObjType_CNode && objsize % BASE_PAGE_SIZE != 0) {
        printk(LOG_WARN, "%s: objsize = %zu\n", __FUNCTION__, objsize);
        return SYS_ERR_INVALID_SIZE;
    } else if ((objsize * sizeof(struct cte)) % BASE_PAGE_SIZE != 0) {
        printk(LOG_WARN, "%s: CNode: objsize = %zu\n", __FUNCTION__, objsize);
        return SYS_ERR_INVALID_SIZE;
    }
    // TODO: clean up semantics for type == ObjType_CNode
    assert((type == ObjType_CNode
            && ((objsize * sizeof(struct cte)) % BASE_PAGE_SIZE == 0)) ||
           (type != ObjType_CNode && objsize % BASE_PAGE_SIZE == 0));

    /* No explicit retypes to Mapping allowed */
    if (type_is_mapping(type)) {
        return SYS_ERR_RETYPE_MAPPING_EXPLICIT;
    }

    struct capability *src_cap = &src_cte->cap;

    TRACE_CAP_MSG("retyping", src_cte);

    /* Check retypability */
    err = is_retypeable(src_cte, src_cap->type, type, from_monitor);
    if (err_is_fail(err)) {
        if (err_no(err) != SYS_ERR_REVOKE_FIRST) {
            printk(LOG_NOTE, "caps_retype: is_retypeable failed: %"PRIuERRV"\n", err);
            debug(SUBSYS_CAPS, "caps_retype: is_retypeable failed\n");
            return err;
        } else {
            debug(SUBSYS_CAPS,
                    "caps_retype: is_retypeable() returned SYS_ERR_REVOKE_FIRST, doing range check\n");
            // We handle err_revoke_first fine-grained checking below, as it
            // might happen for non-overlapping regions.

            // TODO: move the range checking into is_retypeable() or even
            // is_revoked_first(), -SG 2016-04-18
            do_range_check = true;
        }
    }
    // from here: src cap type is one of these.
    assert(src_cap->type == ObjType_PhysAddr ||
           src_cap->type == ObjType_RAM ||
           src_cap->type == ObjType_Dispatcher ||
           src_cap->type == ObjType_Frame ||
           src_cap->type == ObjType_DevFrame);

    if (src_cap->type != ObjType_Dispatcher) {
        base = get_address(src_cap);
        size = get_size(src_cap);
    }

    maxobjs = caps_max_numobjs(type, get_size(src_cap), objsize);
    debug(SUBSYS_CAPS, "maximum possible new object count: %zu\n", maxobjs);

    if (maxobjs == 0) {
        debug(SUBSYS_CAPS, "caps_retype: maxobjs == 0\n");
        return SYS_ERR_INVALID_SIZE;
    }

    if (count > maxobjs) {
        debug(SUBSYS_CAPS, "caps_retype: maxobjs = %zu, count = %zu\n", maxobjs, count);
        return SYS_ERR_RETYPE_INVALID_COUNT;
    }
    // from here: count <= maxobjs
    assert(count <= maxobjs);
    // make sure nobody calls with the old behaviour
    if (count == 0) {
        return SYS_ERR_RETYPE_INVALID_COUNT;
    }
    assert(count > 0);

    /* check that we can create `count` objs from `offset` in source, and
     * update base accordingly */
    if (src_cap->type != ObjType_Dispatcher) {
        // TODO: convince ourselves that this is the only condition on offset
        if (offset + count * objsize > get_size(src_cap)) {
            debug(SUBSYS_CAPS, "caps_retype: cannot create all %zu objects"
                    " of size 0x%zx from offset 0x%zx\n", count, objsize, offset);
            return SYS_ERR_RETYPE_INVALID_OFFSET;
        }
        // adjust base address for new objects
        base += offset;

        // Check whether we got SYS_ERR_REVOKE_FIRST because of
        // non-overlapping child
        if (do_range_check) {
            int find_range_result = 0;
            struct cte *found_cte = NULL;
            err = mdb_find_range(get_type_root(src_cap->type), base, objsize * count,
                    MDB_RANGE_FOUND_SURROUNDING, &found_cte, &find_range_result);
            // this should never return an error unless we mess up the
            // non-user supplied arguments
            if (err_is_fail(err)) {
                printk(LOG_WARN, "mdb_find_range returned: %"PRIuERRV"\n", err);
            }
            assert(err_is_ok(err));
            // return REVOKE_FIRST, if we found a cap inside the region
            // (FOUND_INNER == 2) or overlapping the region (FOUND_PARTIAL == 3)
            if (find_range_result >= MDB_RANGE_FOUND_INNER) {
                printf("found existing region inside, or overlapping requested region\n");
                return SYS_ERR_REVOKE_FIRST;
            }
            // return REVOKE_FIRST, if we found a cap that isn't our source
            // (or a copy of our source) covering the whole requested region.
            else if (find_range_result == MDB_RANGE_FOUND_SURROUNDING &&
                     !is_copy(&found_cte->cap, src_cap))
            {
                printf("found non source region fully covering requested region");
                return SYS_ERR_REVOKE_FIRST;
            }
        }
    }

    /* check that destination slots all fit within target cnode */
    // TODO: fix this with new cspace layout (should be easier)
    if (dest_slot + count > (1UL << dest_cnode->u.cnode.bits)) {
        debug(SUBSYS_CAPS, "caps_retype: dest slots don't fit in cnode\n");
        return SYS_ERR_SLOTS_INVALID;
    }

    /* check that destination slots are all empty */
    debug(SUBSYS_CAPS, "caps_retype: dest cnode is %#" PRIxLPADDR
          " dest_slot %d\n",
          dest_cnode->u.cnode.cnode, (int)dest_slot);
    for (cslot_t i = 0; i < count; i++) {
        if (caps_locate_slot(dest_cnode->u.cnode.cnode, dest_slot + i)->cap.type
            != ObjType_Null) {
            debug(SUBSYS_CAPS, "caps_retype: dest slot %d in use\n",
                  (int)(dest_slot + i));
            return SYS_ERR_SLOTS_IN_USE;
        }
    }

    /* create new caps */
    struct cte *dest_cte =
        caps_locate_slot(dest_cnode->u.cnode.cnode, dest_slot);
    err = caps_create(type, base, size, objsize, count, my_core_id, dest_cte);
    if (err_is_fail(err)) {
        debug(SUBSYS_CAPS, "caps_retype: failed to create a dest cap\n");
        return err_push(err, SYS_ERR_RETYPE_CREATE);
    }

    /* special initialisation for endpoint caps */
    if (type == ObjType_EndPoint) {
        assert(src_cap->type == ObjType_Dispatcher);
        assert(count == 1);
        struct capability *dest_cap = &dest_cte->cap;
        dest_cap->u.endpoint.listener = src_cap->u.dispatcher.dcb;
    }

    /* Handle mapping */
    for (size_t i = 0; i < count; i++) {
        mdb_insert(&dest_cte[i]);
    }

#ifdef TRACE_PMEM_CAPS
    for (size_t i = 0; i < count; i++) {
        TRACE_CAP_MSG("created", &dest_cte[i]);
    }
#endif

    TRACE(KERNEL, CAP_RETYPE, 1);
    return SYS_ERR_OK;
}


/// Check the validity of a retype operation
errval_t is_retypeable(struct cte *src_cte, enum objtype src_type,
                       enum objtype dest_type, bool from_monitor)
{
    if (!is_well_founded(src_type, dest_type)) {
        return SYS_ERR_INVALID_RETYPE;
    } else if (!is_revoked_first(src_cte, src_type)){
        //printf("err_revoke_first: (%p, %d, %d)\n", src_cte, src_type, dest_type);
        return SYS_ERR_REVOKE_FIRST;
    } else if (dest_type == ObjType_EndPoint && src_cte->mdbnode.owner == my_core_id) {
        // XXX: because of the current "multi-retype" hack for endpoints, a
        // dispatcher->endpoint retype can happen irrespective of the existence
        // of descendants on any core.
        // However, we only do this for locally owned caps as the owner should
        // be notified that the cap has remote descendants
        return SYS_ERR_OK;
    } else if (!from_monitor && (src_cte->mdbnode.owner != my_core_id
                                 || src_cte->mdbnode.remote_descs)) {
        return SYS_ERR_RETRY_THROUGH_MONITOR;
    } else {
        return SYS_ERR_OK;
    }
}

/// Create copies to a slot within a cnode
errval_t caps_copy_to_cnode(struct cte *dest_cnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, bool mint, uintptr_t param1,
                            uintptr_t param2)
{
    /* Parameter Checking */
    assert(dest_cnode_cte->cap.type == ObjType_CNode);

    struct cte *dest_cte;
    dest_cte = caps_locate_slot(dest_cnode_cte->cap.u.cnode.cnode, dest_slot);
    return caps_copy_to_cte(dest_cte, src_cte, mint, param1, param2);

}

/// Create copies to a cte
errval_t caps_copy_to_cte(struct cte *dest_cte, struct cte *src_cte, bool mint,
                          uintptr_t param1, uintptr_t param2)
{
    errval_t err;
    /* Parameter checking */
    // Null checking
    assert(dest_cte != NULL);
    assert(src_cte != NULL);

    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_cte->cap;
    // NULL caps cannot be copied/minted
    if (src_cap->type == ObjType_Null) {
        return SYS_ERR_CAP_NOT_FOUND;
    }
    // Parameters should be 0 if not minting
    if (!mint) {
        assert(param1 == 0);
        assert(param2 == 0);
    }

    assert(!src_cte->mdbnode.in_delete);

    /* Insert #source_cap into #dest_cap */
    err = set_cap(dest_cap, src_cap);
    if (err_is_fail(err)) {
        return err;
    }

    /* Transfer MDB attributes that must be equal for all copies */
#define CP_ATTR(at) dest_cte->mdbnode.at = src_cte->mdbnode.at
    CP_ATTR(owner);
    CP_ATTR(locked);
    CP_ATTR(remote_copies);
    CP_ATTR(remote_ancs);
    CP_ATTR(remote_descs);
#undef CP_ATTR

    /* Copy is done */
    if(!mint) {
        TRACE_CAP_MSG("copied to", dest_cte);
        // Handle mapping here only for non-mint operations
        // (mint can change eq fields which would make the early insertion
        // invalid in some cases)
        mdb_insert(dest_cte);
        return SYS_ERR_OK;
    }
    else {
        TRACE_CAP_MSG("minting to", dest_cte);
    }

    /* For minting, set the specified parameters */
    // Process source-specific parameters for minting
    // XXX: If failure, revert the insertion
    switch(src_cap->type) {
    case ObjType_CNode:
        if (param2 > CPTR_BITS) {
            return SYS_ERR_GUARD_SIZE_OVERFLOW;
        }
        dest_cap->u.cnode.guard      = param1;
        dest_cap->u.cnode.guard_size = param2;
        break;

    case ObjType_EndPoint:
        // XXX: FIXME: check that buffer offset lies wholly within the disp frame
        // can't easily enforce this here, because the dispatcher frame may not
        // yet be setup
/*        if (param1 < sizeof(struct dispatcher) ||
            dest_cap->u.endpoint.listener->disp == NULL ||
            param2 < IDC_RECV_LENGTH ||
            param1 + sizeof(struct idc_endpoint) + param2 * sizeof(uintptr_t) >
            (1UL << dest_cap->u.endpoint.listener->disp_cte.cap.u.frame.bits)) {
            return SYS_ERR_INVALID_EPBUF;
        }*/
        if (param2 < LMP_RECV_HEADER_LENGTH) {
            return SYS_ERR_INVALID_EPLEN;
        }
        dest_cap->u.endpoint.epoffset = param1;
        dest_cap->u.endpoint.epbuflen = param2;
        break;

    case ObjType_IO:
        if(src_cap->u.io.start  <= param1) {
            dest_cap->u.io.start = param1;
        }
        if(src_cap->u.io.end  >= param2) {
            dest_cap->u.io.end = param2;
        }
        break;

    default:
        // Unhandled source type for mint
        return SYS_ERR_INVALID_SOURCE_TYPE;
    }

    // Insert after doing minting operation
    mdb_insert(dest_cte);

    return SYS_ERR_OK;
}
