/**
 * \file
 * \brief Kernel capability management implementation.
 */

/*
 * Copyright (c) 2007-2012,2015,2016 ETH Zurich.
 * Copyright (c) 2015, 2016 Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
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

// XXX: remove
#pragma GCC diagnostic ignored "-Wsuggest-attribute=noreturn"

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

STATIC_ASSERT(68 == ObjType_Num, "Knowledge of all cap types");
int sprint_cap(char *buf, size_t len, struct capability *cap)
{
    char *mappingtype;
    switch (cap->type) {
    case ObjType_PhysAddr:
        return snprintf(buf, len,
                        "physical address range cap (0x%" PRIxGENPADDR ":0x%" PRIxGENSIZE ")",
                        cap->u.physaddr.base, cap->u.physaddr.bytes);

    case ObjType_RAM:
        return snprintf(buf, len, "RAM cap (0x%" PRIxGENPADDR ":0x%" PRIxGENSIZE ")",
                        cap->u.ram.base, cap->u.ram.bytes);

    case ObjType_L1CNode: {
        int ret = snprintf(buf, len, "L1 CNode cap "
                           "(base=%#"PRIxGENPADDR", allocated bytes %#"PRIxGENSIZE
                           ", rights mask %#"PRIxCAPRIGHTS")",
                           get_address(cap), get_size(cap),
                           cap->u.l1cnode.rightsmask);
        return ret;
    }

    case ObjType_L2CNode: {
        int ret = snprintf(buf, len, "L2 CNode cap "
                           "(base=%#"PRIxGENPADDR", rights mask %#"PRIxCAPRIGHTS")",
                           get_address(cap), cap->u.l1cnode.rightsmask);
        return ret;
    }

    case ObjType_Dispatcher:
        return snprintf(buf, len, "Dispatcher cap %p", cap->u.dispatcher.dcb);

    case ObjType_Frame:
        return snprintf(buf, len, "Frame cap (0x%" PRIxGENPADDR ":0x%" PRIxGENSIZE ")",
                        cap->u.frame.base, cap->u.frame.bytes);

    case ObjType_EndPointUMP:
        return snprintf(buf, len, "EndPointUMP cap (0x%" PRIxGENPADDR ":0x%"
                                   PRIxGENSIZE ") If:%" PRIu32,
                        cap->u.endpointump.base, cap->u.endpointump.bytes,
                        cap->u.endpointump.iftype);

    case ObjType_DevFrame:
        return snprintf(buf, len, "Device Frame cap (0x%" PRIxGENPADDR ":0x%" PRIxGENSIZE ")",
                        cap->u.devframe.base, cap->u.devframe.bytes);

    case ObjType_VNode_ARM_l1:
        return snprintf(buf, len, "ARM L1 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_arm_l1.base);

    case ObjType_VNode_ARM_l2:
        return snprintf(buf, len, "ARM L2 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_arm_l2.base);

    case ObjType_VNode_AARCH64_l0:
        return snprintf(buf, len, "AARCH64 L0 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_aarch64_l0.base);

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
    case ObjType_VNode_x86_64_pml5:
        return snprintf(buf, len, "x86_64 PML5 at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_pml4.base);
    case ObjType_VNode_VTd_root_table:
        return snprintf(buf, len, "VTd root table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_pml4.base);
    case ObjType_VNode_VTd_ctxt_table:
        return snprintf(buf, len, "VTd ctxt table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_pml4.base);

    case ObjType_VNode_x86_64_ept_ptable:
        return snprintf(buf, len, "x86_64 EPT Page table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_ept_ptable.base);

    case ObjType_VNode_x86_64_ept_pdir:
        return snprintf(buf, len, "x86_64 EPT Page directory at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_ept_pdir.base);

    case ObjType_VNode_x86_64_ept_pdpt:
        return snprintf(buf, len, "x86_64 EPT PDPT at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_ept_pdpt.base);

    case ObjType_VNode_x86_64_ept_pml4:
        return snprintf(buf, len, "x86_64 EPT PML4 at 0x%" PRIxGENPADDR,
                        cap->u.vnode_x86_64_ept_pml4.base);

    case ObjType_Frame_Mapping:
        mappingtype = "Frame";
        goto ObjType_Mapping;
    case ObjType_DevFrame_Mapping:
        mappingtype = "DevFrame";
        goto ObjType_Mapping;
    case ObjType_EndPointUMP_Mapping:
        mappingtype = "EndPointUMP";
        goto ObjType_Mapping;

    case ObjType_VNode_x86_64_pml5_Mapping:
        mappingtype = "x86_64 PML4";
        goto ObjType_Mapping;
    case ObjType_VNode_x86_64_pml4_Mapping:
        mappingtype = "x86_64 PML4";
        goto ObjType_Mapping;
    case ObjType_VNode_x86_64_pdpt_Mapping:
        mappingtype = "x86_64 PDPT";
        goto ObjType_Mapping;
    case ObjType_VNode_x86_64_pdir_Mapping:
        mappingtype = "x86_64 PDIR";
        goto ObjType_Mapping;
    case ObjType_VNode_x86_64_ptable_Mapping:
        mappingtype = "x86_64 PTABLE";
        goto ObjType_Mapping;

    case ObjType_VNode_x86_64_ept_pml4_Mapping:
        mappingtype = "x86_64 EPT PML4";
        goto ObjType_Mapping;
    case ObjType_VNode_x86_64_ept_pdpt_Mapping:
        mappingtype = "x86_64 EPT PDPT";
        goto ObjType_Mapping;
    case ObjType_VNode_x86_64_ept_pdir_Mapping:
        mappingtype = "x86_64 EPT PDIR";
        goto ObjType_Mapping;
    case ObjType_VNode_x86_64_ept_ptable_Mapping:
        mappingtype = "x86_64 EPT PTABLE";
        goto ObjType_Mapping;

    case ObjType_VNode_x86_32_pdpt_Mapping:
        mappingtype = "x86_32 PDPT";
        goto ObjType_Mapping;
    case ObjType_VNode_x86_32_pdir_Mapping:
        mappingtype = "x86_32 PDIR";
        goto ObjType_Mapping;
    case ObjType_VNode_x86_32_ptable_Mapping:
        mappingtype = "x86_32 PTABLE";
        goto ObjType_Mapping;

    case ObjType_VNode_ARM_l1_Mapping:
        mappingtype = "ARM l1";
        goto ObjType_Mapping;
    case ObjType_VNode_ARM_l2_Mapping:
        mappingtype = "ARM l2";
        goto ObjType_Mapping;

    case ObjType_VNode_AARCH64_l0_Mapping:
        mappingtype = "AARCH64 l0";
        goto ObjType_Mapping;
    case ObjType_VNode_AARCH64_l1_Mapping:
        mappingtype = "AARCH64 l1";
        goto ObjType_Mapping;
    case ObjType_VNode_AARCH64_l2_Mapping:
        mappingtype = "AARCH64 l2";
        goto ObjType_Mapping;
    case ObjType_VNode_AARCH64_l3_Mapping:
        mappingtype = "AARCH64 l3";
        goto ObjType_Mapping;

    case ObjType_VNode_VTd_root_table_Mapping:
        mappingtype = "VTd root table";
        goto ObjType_Mapping;
    case ObjType_VNode_VTd_ctxt_table_Mapping:
        mappingtype = "VTd ctxt table";
        goto ObjType_Mapping;
ObjType_Mapping:
        return snprintf(buf, len, "%s Mapping (%s cap @%p, "
                                  "ptable cap @0x%p, entry=%hu, pte_count=%hu)",
                                  mappingtype, mappingtype,
                                  cap->u.frame_mapping.cap,
                                  cap->u.frame_mapping.ptable,
                                  cap->u.frame_mapping.entry,
                                  cap->u.frame_mapping.pte_count);

    case ObjType_IRQTable:
        return snprintf(buf, len, "IRQTable cap");

    case ObjType_IRQDest:
        return snprintf(buf, len, "IRQDest cap (vec: %"PRIu64", cpu: %"PRIu64")",
                cap->u.irqdest.vector, cap->u.irqdest.cpu);

    case ObjType_EndPointLMP:
        return snprintf(buf, len, "EndPoint cap (disp %p offset 0x%" PRIxLVADDR ")",
                        cap->u.endpointlmp.listener, cap->u.endpointlmp.epoffset);

    case ObjType_IO:
        return snprintf(buf, len, "IO cap (0x%hx-0x%hx)",
                        cap->u.io.start, cap->u.io.end);

    case ObjType_Kernel:
        return snprintf(buf, len, "Kernel cap");

    case ObjType_KernelControlBlock:
        return snprintf(buf, len, "Kernel control block");

    case ObjType_ID:
        return snprintf(buf, len, "ID capability (coreid 0x%" PRIxCOREID
                        " core_local_id 0x%" PRIx32 ")", cap->u.id.coreid,
                        cap->u.id.core_local_id);
    case ObjType_ProcessManager:
        return snprintf(buf, len, "Process manager capability");

    case ObjType_Domain:
        return snprintf(buf, len, "Domain capability (coreid 0x%" PRIxCOREID
                        " core_local_id 0x%" PRIx32 ")", cap->u.domain.coreid,
                        cap->u.domain.core_local_id);

    case ObjType_PerfMon:
        return snprintf(buf, len, "PerfMon cap");

    case ObjType_Null:
        return snprintf(buf, len, "Null capability (empty slot)");

    case ObjType_IPI:
        return snprintf(buf, len, "IPI cap");

    case ObjType_DeviceID:
        return snprintf(buf, len, "DeviceID %u.%u.%u",
                        cap->u.deviceid.bus, cap->u.deviceid.device,
                        cap->u.deviceid.function);
    case ObjType_DeviceIDManager:
        return snprintf(buf, len, "DeviceID Manager cap");


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
 * Domain capability core_local_id counter.
 */
static uint32_t domain_cap_counter = 1;

/**
 * Tracing sequence number for retypes
 */
static uint64_t retype_seqnum = 0;

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
STATIC_ASSERT(68 == ObjType_Num, "Knowledge of all cap types");
static size_t caps_max_numobjs(enum objtype type, gensize_t srcsize, gensize_t objsize)
{
    switch(type) {
    case ObjType_PhysAddr:
    case ObjType_RAM:
    case ObjType_Frame:
    case ObjType_EndPointUMP:
    case ObjType_DevFrame:
        if (objsize > srcsize) {
            return 0;
        } else {
            return srcsize / objsize;
        }

    case ObjType_L1CNode:
        if (srcsize < OBJSIZE_L2CNODE || objsize < OBJSIZE_L2CNODE) {
            // disallow L1 CNode to be smaller than 16kB.
            return 0;
        } else {
            return srcsize / objsize;
        }

    case ObjType_L2CNode:
        if (srcsize < OBJSIZE_L2CNODE || objsize != OBJSIZE_L2CNODE) {
            // disallow L2 CNode creation if source too small or objsize wrong
            return 0;
        } else {
            return srcsize / objsize;
        }
    case ObjType_VNode_VTd_root_table :
    case ObjType_VNode_VTd_ctxt_table :
    case ObjType_VNode_x86_64_pml5:
    case ObjType_VNode_x86_64_pml4:
    case ObjType_VNode_x86_64_pdpt:
    case ObjType_VNode_x86_64_pdir:
    case ObjType_VNode_x86_64_ptable:
    case ObjType_VNode_x86_64_ept_pml4:
    case ObjType_VNode_x86_64_ept_pdpt:
    case ObjType_VNode_x86_64_ept_pdir:
    case ObjType_VNode_x86_64_ept_ptable:
    case ObjType_VNode_x86_32_pdpt:
    case ObjType_VNode_x86_32_pdir:
    case ObjType_VNode_x86_32_ptable:
    case ObjType_VNode_ARM_l1:
    case ObjType_VNode_ARM_l2:
    case ObjType_VNode_AARCH64_l0:
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
        if (srcsize < OBJSIZE_DISPATCHER) {
            return 0;
        } else {
            return srcsize / OBJSIZE_DISPATCHER;
        }

    case ObjType_KernelControlBlock:
        if (srcsize < OBJSIZE_KCB) {
            return 0;
        } else {
            return srcsize / OBJSIZE_KCB;
        }

    case ObjType_Domain:
        return L2_CNODE_SLOTS;

    case ObjType_Kernel:
    case ObjType_IRQTable:
    case ObjType_IRQDest:
    case ObjType_IRQSrc:
    case ObjType_IO:
    case ObjType_EndPointLMP:
    case ObjType_ID:
    case ObjType_Notify_IPI:
    case ObjType_PerfMon:
    case ObjType_IPI:
    case ObjType_ProcessManager:
    case ObjType_DeviceID:
    case ObjType_DeviceIDManager:
    case ObjType_VNode_ARM_l1_Mapping:
    case ObjType_VNode_ARM_l2_Mapping:
    case ObjType_VNode_AARCH64_l0_Mapping:
    case ObjType_VNode_AARCH64_l1_Mapping:
    case ObjType_VNode_AARCH64_l2_Mapping:
    case ObjType_VNode_AARCH64_l3_Mapping:
    case ObjType_VNode_x86_64_pml4_Mapping:
    case ObjType_VNode_x86_64_pdpt_Mapping:
    case ObjType_VNode_x86_64_pdir_Mapping:
    case ObjType_VNode_x86_64_ptable_Mapping:
    case ObjType_VNode_x86_64_ept_pml4_Mapping:
    case ObjType_VNode_x86_64_ept_pdpt_Mapping:
    case ObjType_VNode_x86_64_ept_pdir_Mapping:
    case ObjType_VNode_x86_64_ept_ptable_Mapping:
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
STATIC_ASSERT(68 == ObjType_Num, "Knowledge of all cap types");
static errval_t caps_zero_objects(enum objtype type, lpaddr_t lpaddr,
                                  gensize_t objsize, size_t count)
{
    TRACE(KERNEL_CAPOPS, ZERO_OBJECTS, retype_seqnum);
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
    case ObjType_EndPointUMP :
        debug(SUBSYS_CAPS, "Frame: zeroing %zu bytes @%#"PRIxLPADDR"\n",
                (size_t)objsize * count, lpaddr);
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, objsize * count);
        TRACE(KERNEL, BZERO, 0);
        break;

    case ObjType_L1CNode:
    case ObjType_L2CNode:
        debug(SUBSYS_CAPS, "L%dCNode: zeroing %zu bytes @%#"PRIxLPADDR"\n",
                type == ObjType_L1CNode ? 1 : 2, (size_t)objsize * count,
                lpaddr);
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, objsize * count);
        TRACE(KERNEL, BZERO, 0);
        break;

    case ObjType_VNode_ARM_l1:
    case ObjType_VNode_ARM_l2:
    case ObjType_VNode_AARCH64_l0:
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
    case ObjType_VNode_x86_64_ept_ptable:
    case ObjType_VNode_x86_64_ept_pdir:
    case ObjType_VNode_x86_64_ept_pdpt:
    case ObjType_VNode_x86_64_ept_pml4:
    case ObjType_VNode_x86_64_pml5:
    case ObjType_VNode_VTd_root_table:
    case ObjType_VNode_VTd_ctxt_table:
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
                ((size_t) OBJSIZE_DISPATCHER) * count, lpaddr);
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, OBJSIZE_DISPATCHER * count);
        TRACE(KERNEL, BZERO, 0);
        break;

    case ObjType_KernelControlBlock:
        debug(SUBSYS_CAPS, "KCB: zeroing %zu bytes @%#"PRIxLPADDR"\n",
                ((size_t) OBJSIZE_KCB) * count, lpaddr);
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, OBJSIZE_KCB * count);
        TRACE(KERNEL, BZERO, 0);
        break;

    default:
        debug(SUBSYS_CAPS, "Not zeroing %zu bytes @%#"PRIxLPADDR" for type %d\n",
                (size_t)objsize * count, lpaddr, (int)type);
        break;

    }

    TRACE(KERNEL_CAPOPS, ZERO_OBJECTS_DONE, retype_seqnum);
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
STATIC_ASSERT(68 == ObjType_Num, "Knowledge of all cap types");

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
                       ", %" PRIuGENSIZE " bytes, objsize=%"PRIuGENSIZE
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
    bool is_ept = false;

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
    case ObjType_EndPointUMP:
        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.endpointump.base = genpaddr + dest_i * objsize;
            temp_cap.u.endpointump.bytes = objsize;
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

    case ObjType_L1CNode:
        for (dest_i = 0; dest_i < count; dest_i++) {
            assert(objsize >= OBJSIZE_L2CNODE);
            assert(objsize % OBJSIZE_L2CNODE == 0);
            temp_cap.u.l1cnode.cnode = lpaddr + dest_i * objsize;
            temp_cap.u.l1cnode.allocated_bytes = objsize;
            // XXX: implement CNode cap rights
            temp_cap.u.l1cnode.rightsmask = CAPRIGHTS_ALLRIGHTS;
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_L2CNode:
        for (dest_i = 0; dest_i < count; dest_i++) {
            temp_cap.u.l2cnode.cnode = lpaddr + dest_i * objsize;
            // XXX: implement CNode cap rights
            temp_cap.u.l2cnode.rightsmask = CAPRIGHTS_ALLRIGHTS;
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

    case ObjType_VNode_AARCH64_l0:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_aarch64_l0.base =
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
        is_ept = true;
    case ObjType_VNode_x86_64_ept_ptable:
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
        is_ept = true;
    case ObjType_VNode_x86_64_ept_pdir:
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
        is_ept = true;
    case ObjType_VNode_x86_64_ept_pdpt:
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

    case ObjType_VNode_x86_64_ept_pml4:
        is_ept = true;
    case ObjType_VNode_x86_64_pml4:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_x86_64_pml4.base =
                genpaddr + dest_i * objsize_vnode;

#if defined(__x86_64__) || defined(__k1om__)
            // Make it a good PML4 by inserting kernel/mem VSpaces
            lpaddr_t var = gen_phys_to_local_phys(get_address(&temp_cap));
            paging_x86_64_make_good_pml4(var);
            if (is_ept) {
                paging_x86_64_make_good_ept_pml4(var);
            } else {
                paging_x86_64_make_good_pml4(var);
            }
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }

        break;
    }
    case ObjType_VNode_x86_64_pml5:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_x86_64_pml5.base =
                    genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }

        break;
    }
    case ObjType_VNode_VTd_root_table:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_vtd_root_table.base =
                    genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }

        break;
    }
    case ObjType_VNode_VTd_ctxt_table:
    {
        size_t objsize_vnode = vnode_objsize(type);

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.vnode_vtd_ctxt_table.base =
                    genpaddr + dest_i * objsize_vnode;

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }

        break;
    }

    case ObjType_Dispatcher:
        assert(OBJSIZE_DISPATCHER >= sizeof(struct dcb));

        for(dest_i = 0; dest_i < count; dest_i++) {
            // Initialize type specific fields
            temp_cap.u.dispatcher.dcb = (struct dcb *)
                (lvaddr + dest_i * OBJSIZE_DISPATCHER);
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

    case ObjType_Domain:
        // Domain type does not refer to a kernel object
        assert(lpaddr  == 0);
        assert(size    == 0);
        assert(objsize == 0);
        assert(count   <= L2_CNODE_SLOTS);

        // Prevent wrap around
        if (domain_cap_counter + count >= UINT32_MAX) {
            return SYS_ERR_DOMAIN_SPACE_EXHAUSTED;
        }

        for(size_t i = 0; i < count; i++) {
            // Initialize type specific fields
            temp_cap.u.domain.coreid = my_core_id;
            temp_cap.u.domain.core_local_id = domain_cap_counter++;
            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &temp_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    case ObjType_IO:
        temp_cap.u.io.start = 0;
        temp_cap.u.io.end   = 65535;
        /* fall through */

    case ObjType_IRQSrc:
        /* Caller has to set vec_start and vec_end */
    case ObjType_Kernel:
    case ObjType_IPI:
    case ObjType_IRQTable:
    case ObjType_IRQDest:
    case ObjType_EndPointLMP:
    case ObjType_Notify_IPI:
    case ObjType_PerfMon:
    case ObjType_ProcessManager:
    case ObjType_DeviceID :
    case ObjType_DeviceIDManager :
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
        assert(OBJSIZE_KCB >= sizeof(struct kcb));

        for(size_t i = 0; i < count; i++) {
            // Initialize type specific fields
            temp_cap.u.kernelcontrolblock.kcb = (struct kcb *)
                (lvaddr + i * OBJSIZE_KCB);
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
 * Look up a capability in two-level cspace rooted at `rootcn`.
 */
errval_t caps_lookup_slot(struct capability *rootcn, capaddr_t cptr,
                          uint8_t level, struct cte **ret, CapRights rights)
{
    TRACE(KERNEL, CAP_LOOKUP_SLOT, 0);

    cslot_t l1index, l2index;
    l1index = (cptr >> L2_CNODE_BITS) & MASK(CPTR_BITS-L2_CNODE_BITS);
    l2index = cptr & MASK(L2_CNODE_BITS);

    assert(ret != NULL);
    assert(rootcn != NULL);

    if (level > 2) {
        debug(SUBSYS_CAPS, "%s called with level=%hhu, from %p\n",
                __FUNCTION__, level,
                (void*)kernel_virt_to_elf_addr(__builtin_return_address(0)));
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_CAP_LOOKUP_DEPTH;
    }
    assert(level <= 2);

    // level 0 means that we do not do any resolution and just return the cte
    // for rootcn.
    if (level == 0) {
        *ret = cte_for_cap(rootcn);
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_OK;
    }

    if (rootcn->type != ObjType_L1CNode) {
        debug(SUBSYS_CAPS, "%s: rootcn->type = %d, called from %p\n",
                __FUNCTION__, rootcn->type,
                (void*)kernel_virt_to_elf_addr(__builtin_return_address(0)));
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        // XXX: think about errors
        return SYS_ERR_CNODE_TYPE;
    }
    assert(rootcn->type == ObjType_L1CNode);

    if (l1index >= cnode_get_slots(rootcn)) {
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        debug(SUBSYS_CAPS, "%s: l1index = %"PRIuCSLOT", slots= %zu\n",
                __FUNCTION__, l1index, cnode_get_slots(rootcn));
        return SYS_ERR_L1_CNODE_INDEX;
    }

    /* Apply rights to L1 CNode */
    if ((rootcn->rights & rights) != rights) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: Rights mismatch\n"
              "Passed rights = %u, cnode_cap->rights = %u\n",
              rights, rootcn->rights);
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_CNODE_RIGHTS;
    }

    struct cte *l2cnode = caps_locate_slot(get_address(rootcn), l1index);

    // level == 1 means that we terminate after looking up the slot in the L1
    // cnode.
    if (level == 1) {
        if (l2cnode->cap.type == ObjType_Null) {
            TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
            return SYS_ERR_CAP_NOT_FOUND;
        }
        *ret = l2cnode;
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_OK;
    }

    // L2 CNode in given L1 slot does not exist
    if (l2cnode->cap.type == ObjType_Null) {
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        debug(SUBSYS_CAPS, "%s: l2cnode is NULL\n", __FUNCTION__);
        return SYS_ERR_CNODE_NOT_FOUND;
    }
    if (l2cnode->cap.type != ObjType_L2CNode) {
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        debug(SUBSYS_CAPS, "%s: l2cnode->type = %d\n", __FUNCTION__,
               l2cnode->cap.type);
        return SYS_ERR_CNODE_TYPE;
    }
    assert(l2cnode->cap.type == ObjType_L2CNode);

    assert(l2index < L2_CNODE_SLOTS);

    /* Apply rights to L2 CNode */
    if ((l2cnode->cap.rights & rights) != rights) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: Rights mismatch\n"
              "Passed rights = %u, cnode_cap->rights = %u\n",
              rights, l2cnode->cap.rights);
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_CNODE_RIGHTS;
    }

    struct cte *cte = caps_locate_slot(get_address(&l2cnode->cap), l2index);
    if (cte->cap.type == ObjType_Null) {
        TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
        return SYS_ERR_CAP_NOT_FOUND;
    }

    *ret = cte;

    TRACE(KERNEL, CAP_LOOKUP_SLOT, 1);
    return SYS_ERR_OK;
}

/**
 * Wrapper for caps_lookup_slot returning capability instead of cte.
 */
errval_t caps_lookup_cap(struct capability *cnode_cap, capaddr_t cptr,
                         uint8_t level, struct capability **ret, CapRights rights)
{
    TRACE(KERNEL, CAP_LOOKUP_CAP, 0);

    struct cte *ret_cte;
    errval_t err = caps_lookup_slot(cnode_cap, cptr, level, &ret_cte, rights);
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
                                   int cnode_level, cslot_t dest_slot, coreid_t owner,
                                   struct capability *src)
{
    TRACE(KERNEL, CAP_CREATE_FROM_EXISTING, 0);
    errval_t err;
    struct capability *cnode;
    err = caps_lookup_cap(root, cnode_cptr, cnode_level, &cnode,
                          CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_SLOT_LOOKUP_FAIL);
    }
    if (cnode->type != ObjType_L1CNode &&
        cnode->type != ObjType_L2CNode)
    {
        return SYS_ERR_CNODE_TYPE;
    }

    struct cte *dest = caps_locate_slot(get_address(cnode), dest_slot);

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
STATIC_ASSERT(68 == ObjType_Num, "Knowledge of all cap types");
#ifndef NDEBUG
static bool check_caps_create_arguments(enum objtype type,
                                        size_t bytes, size_t objsize,
                                        bool exact)
{
    gensize_t base_mask = BASE_PAGE_MASK;
    if (type_is_vnode(type)) {
        base_mask = vnode_objsize(type) - 1;
    }
    /* mappable types need to be at least BASE_PAGE_SIZEd */
    if (type_is_mappable(type)) {
        /* source size not multiple of or not aligned to BASE_PAGE_SIZE */
        if (bytes & base_mask) {
            debug(SUBSYS_CAPS, "source size not multiple of BASE_PAGE_SIZE\n");
            return false;
        }
        /* objsize > 0 and not multiple of BASE_PAGE_SIZE */
        if (objsize > 0 && objsize & base_mask) {
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

    if (type == ObjType_L1CNode) {
        /* L1 CNode minimum size is OBJSIZE_L2CNODE */
        if (bytes < OBJSIZE_L2CNODE || objsize < OBJSIZE_L2CNODE) {
            debug(SUBSYS_CAPS, "source size or L1 CNode objsize < OBJSIZE_L2CNODE\n");
            return false;
        }
        /* check that bytes can be evenly divided into L1 CNodes of objsize */
        if (exact && (bytes % objsize != 0)) {
            debug(SUBSYS_CAPS, "source not evenly divisible into L1 CNodes of objsize\n");
            return false;
        }
        /* L1 CNode size must be multiple of 1UL << OBJBITS_CTE */
        return objsize % (1UL << OBJBITS_CTE) == 0;
    }

    if (type == ObjType_L2CNode) {
        /* L2 CNode size must be OBJSIZE_L2CNODE */
        if (bytes < OBJSIZE_L2CNODE || objsize != OBJSIZE_L2CNODE) {
            debug(SUBSYS_CAPS, "source size < or L2 CNode objsize != OBJSIZE_L2CNODE\n");
            return false;
        }
        if (exact && (bytes % objsize != 0)) {
            debug(SUBSYS_CAPS, "source not evenly divisible into L2 CNodes of objsize\n");
            return false;
        }
        return true;
    }

    /* special case Dispatcher which is 1kB right now */
    if (type == ObjType_Dispatcher) {
        if (bytes & (OBJSIZE_DISPATCHER - 1)) {
            return false;
        }
        if (objsize > 0 && objsize != OBJSIZE_DISPATCHER) {
            return false;
        }

        return true;
    }

    // All other types do not need special alignments/offsets
    return true;
}
#else
#define check_caps_create_arguments(a,b,c,d) 0
#endif

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
    assert(type != ObjType_EndPointLMP); // Cap of this type cannot be created
    debug(SUBSYS_CAPS, "caps_create_new: type = %d, addr = %#"PRIxLPADDR
            ", bytes=%zu, objsize=%zu\n", type, addr, bytes, objsize);

    assert(check_caps_create_arguments(type, bytes, objsize, false));
    assert(addr == 0 || check_caps_create_arguments(type, bytes, objsize, true));

    size_t numobjs = caps_max_numobjs(type, bytes, objsize);
    assert(numobjs > 0);
    // XXX: Dispatcher creation is kind of hacky right now :(
    // Consider allowing non-mappable types to be < BASE_PAGE_SIZE
    //if (type == ObjType_Dispatcher) {
    //    numobjs = 1;
    //}

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

STATIC_ASSERT(68 == ObjType_Num, "Knowledge of all cap types");
/// Retype caps
/// Create `count` new caps of `type` from `offset` in src, and put them in
/// `dest_cnode` starting at `dest_slot`.
errval_t caps_retype(enum objtype type, gensize_t objsize, size_t count,
                     struct capability *dest_cnode, cslot_t dest_slot,
                     struct cte *src_cte, gensize_t offset,
                     bool from_monitor)
{
    TRACE(KERNEL, CAP_RETYPE, 0);
    TRACE(KERNEL_CAPOPS, RETYPE_ENTER, ++retype_seqnum);
    size_t maxobjs;
    genpaddr_t base = 0;
    gensize_t size = 0;
    errval_t err;
    bool do_range_check = false;
    struct capability *src_cap = &src_cte->cap;

    /* Parameter checking */
    assert(type != ObjType_Null);
    assert(type < ObjType_Num);
    if (type == ObjType_Null || type >= ObjType_Num) {
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_INVALID_RETYPE;
    }

    debug(SUBSYS_CAPS, "%s: Retyping to type=%d, from offset=%" PRIuGENSIZE
            ", objsize=%" PRIuGENSIZE ", count=%zu\n",
            __FUNCTION__, type, offset, objsize, count);

    /*
     * check that offset into source cap is multiple of destination object
     * size, or base page size, whichever is smaller.
     */
    gensize_t dest_obj_alignment = BASE_PAGE_SIZE;
    if (type_is_vnode(type) && vnode_objsize(type) < BASE_PAGE_SIZE) {
        dest_obj_alignment = vnode_objsize(type);
    } else if (type == ObjType_Dispatcher) {
        dest_obj_alignment = OBJSIZE_DISPATCHER;
    }
    if (src_cap->type != ObjType_IRQSrc && offset % dest_obj_alignment != 0) {
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_RETYPE_INVALID_OFFSET;
    }
    assert(offset % dest_obj_alignment == 0 || src_cap->type == ObjType_IRQSrc);

    // check that size is multiple of BASE_PAGE_SIZE for mappable types
    gensize_t base_size = BASE_PAGE_SIZE;
    if (type_is_vnode(type)) {
        base_size = vnode_objsize(type);
    }
    if (type_is_mappable(type) && objsize % base_size != 0) {
        debug(SUBSYS_CAPS, "%s: objsize = %"PRIuGENSIZE"\n", __FUNCTION__, objsize);
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_INVALID_SIZE;
    }
    else if (type == ObjType_L1CNode && objsize % OBJSIZE_L2CNODE != 0)
    {
        printk(LOG_WARN, "%s: CNode: objsize = %" PRIuGENSIZE "\n", __FUNCTION__, objsize);
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_INVALID_SIZE;
    }
    else if (type == ObjType_L2CNode && objsize != OBJSIZE_L2CNODE)
    {
        printk(LOG_WARN, "%s: L2CNode: objsize = %"PRIuGENSIZE"\n", __FUNCTION__, objsize);
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_INVALID_SIZE;
    }
    assert((type_is_mappable(type) && objsize % base_size == 0) ||
           (type == ObjType_L1CNode && objsize % OBJSIZE_L2CNODE == 0 &&
            objsize >= OBJSIZE_L2CNODE) ||
           (type == ObjType_L2CNode && objsize == OBJSIZE_L2CNODE) ||
           !type_is_mappable(type));

    /* No explicit retypes to Mapping allowed */
    if (type_is_mapping(type)) {
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_RETYPE_MAPPING_EXPLICIT;
    }


    TRACE_CAP_MSG("retyping", src_cte);

    TRACE(KERNEL_CAPOPS, RETYPE_IS_RETYPEABLE, retype_seqnum);
    /* Check retypability */
    err = is_retypeable(src_cte, src_cap->type, type, from_monitor);
    if (err_is_fail(err)) {
        if (err_no(err) != SYS_ERR_REVOKE_FIRST) {
            debug(SUBSYS_CAPS, "caps_retype: is_retypeable failed\n");
            TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
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
    TRACE(KERNEL_CAPOPS, RETYPE_IS_RETYPEABLE_DONE, retype_seqnum);
    // from here: src cap type is one of these.
    assert(src_cap->type == ObjType_PhysAddr ||
           src_cap->type == ObjType_RAM ||
           src_cap->type == ObjType_Dispatcher ||
           src_cap->type == ObjType_Frame ||
           src_cap->type == ObjType_DevFrame ||
           src_cap->type == ObjType_IRQSrc ||
           src_cap->type == ObjType_ProcessManager ||
           src_cap->type == ObjType_DeviceIDManager);

    if (src_cap->type != ObjType_Dispatcher && src_cap->type != ObjType_IRQSrc) {
        base = get_address(src_cap);
        size = get_size(src_cap);
    }

    maxobjs = caps_max_numobjs(type, get_size(src_cap), objsize);
    debug(SUBSYS_CAPS, "maximum possible new object count: %zu\n", maxobjs);

    if (maxobjs == 0) {
        debug(SUBSYS_CAPS, "caps_retype: maxobjs == 0\n");
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_INVALID_SIZE;
    }

    if (count > maxobjs) {
        debug(SUBSYS_CAPS, "caps_retype: maxobjs = %zu, count = %zu\n", maxobjs, count);
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_RETYPE_INVALID_COUNT;
    }
    // from here: count <= maxobjs
    assert(count <= maxobjs);
    // make sure nobody calls with the old behaviour
    if (count == 0) {
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_RETYPE_INVALID_COUNT;
    }
    assert(count > 0);

    /* check that we can create `count` objs from `offset` in source, and
     * update base accordingly */
    if (src_cap->type != ObjType_Dispatcher && src_cap->type != ObjType_IRQSrc
            && src_cap->type != ObjType_Domain) {
        // TODO: convince ourselves that this is the only condition on offset
        if (offset + count * objsize > get_size(src_cap)) {
            debug(SUBSYS_CAPS, "caps_retype: cannot create all %zu objects"
                    " of size 0x%" PRIxGENSIZE " from offset 0x%" PRIxGENSIZE "\n",
                    count, objsize, offset);
            TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
            return SYS_ERR_RETYPE_INVALID_OFFSET;
        }
        // adjust base address for new objects
        base += offset;

        // Check whether we got SYS_ERR_REVOKE_FIRST because of
        // non-overlapping child
        if (do_range_check) {
            TRACE(KERNEL_CAPOPS, RETYPE_RANGE_CHECK, retype_seqnum);
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
                debug(SUBSYS_CAPS,
                    "%s: found existing region inside, or overlapping requested region:\n",
                    __FUNCTION__);
                debug(SUBSYS_CAPS, "%s: our region: %#"PRIxGENPADDR"--%#"PRIxGENPADDR"\n",
                        __FUNCTION__, base, base+objsize*count);
                if (found_cte && kernel_loglevel >= LOG_DEBUG &&
                    kernel_log_subsystem_mask & SUBSYS_CAPS)
                {
                    char capbuf[128];
                    sprint_cap(capbuf, 128, &found_cte->cap);
                    printk(LOG_NOTE, "%s: cap=%s\n", __FUNCTION__, capbuf);
                    if (type_is_mapping(found_cte->cap.type)) {
                        sprint_cap(capbuf, 128, found_cte->cap.u.frame_mapping.cap);
                        printk(LOG_NOTE, "%s: ... is mapping for cap=%s\n",
                                __FUNCTION__, capbuf);
                    }
                    assert(get_address(&found_cte->cap) >= base &&
                           get_address(&found_cte->cap) < base+objsize*count);
                }
                TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
                return SYS_ERR_REVOKE_FIRST;
            }
            // return REVOKE_FIRST, if we found a cap that isn't our source
            // (or a copy of our source) covering the whole requested region.
            else if (find_range_result == MDB_RANGE_FOUND_SURROUNDING &&
                     !is_copy(&found_cte->cap, src_cap))
            {
                debug(SUBSYS_CAPS,
                       "%s: found non source region fully covering requested region\n",
                       __FUNCTION__);
                TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
                return SYS_ERR_REVOKE_FIRST;
            }
            TRACE(KERNEL_CAPOPS, RETYPE_RANGE_CHECK_DONE, retype_seqnum);
        }
    }

    /* check that destination slots all fit within target cnode */
    if (dest_slot + count > cnode_get_slots(dest_cnode)) {
        debug(SUBSYS_CAPS, "caps_retype: dest slots don't fit in cnode\n");
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return SYS_ERR_SLOTS_INVALID;
    }

    /* check that destination slots are all empty */
    debug(SUBSYS_CAPS, "caps_retype: dest cnode is %#" PRIxLPADDR
          " dest_slot %d\n",
          get_address(dest_cnode), (int)dest_slot);
    for (cslot_t i = 0; i < count; i++) {
        if (caps_locate_slot(get_address(dest_cnode), dest_slot + i)->cap.type
            != ObjType_Null) {
            debug(SUBSYS_CAPS, "caps_retype: dest slot %d in use\n",
                  (int)(dest_slot + i));
            TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
            return SYS_ERR_SLOTS_IN_USE;
        }
    }

    /* Check that L1 CNode is destination when creating L2 CNode */
    if (type == ObjType_L2CNode) {
        debug(SUBSYS_CAPS, "caps_retype: check that dest cnode is L1"
                           " when creating L2 CNodes\n");
        if (dest_cnode->type != ObjType_L1CNode &&
            dest_cnode->type != ObjType_L2CNode)
        {
            panic("L2 CNode can only be created in L1 or L2 CNode\n");
        }
    }

    // IRQSrc specific checks
    uint64_t vec_start_new = offset;
    uint64_t vec_end_new = objsize;
    if(src_cap->type == ObjType_IRQSrc){

        // Check new range is valid
        if(vec_start_new > vec_end_new){
            TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
            return SYS_ERR_RETYPE_INVALID_OFFSET;
        }
        
        // Check vec_start_new in range
        if(!(src_cap->u.irqsrc.vec_start <= vec_start_new &&
                vec_start_new <= src_cap->u.irqsrc.vec_end)){
            TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
            return SYS_ERR_RETYPE_INVALID_OFFSET;
        }

        // Check vec_end_new in range
        if(!(src_cap->u.irqsrc.vec_start <= vec_end_new &&
                vec_end_new <= src_cap->u.irqsrc.vec_end)){
            TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
            return SYS_ERR_RETYPE_INVALID_OBJSIZE;
        }
    }


    TRACE(KERNEL_CAPOPS, RETYPE_CREATE_CAPS, retype_seqnum);
    /* create new caps */
    struct cte *dest_cte =
        caps_locate_slot(get_address(dest_cnode), dest_slot);
    if(type == ObjType_IRQSrc){
        // Pass special arguments
        err = caps_create(type, 0, 0, 0, 1, my_core_id, dest_cte);
        if(err_is_ok(err)){
            dest_cte->cap.u.irqsrc.vec_start = vec_start_new;
            dest_cte->cap.u.irqsrc.vec_end = vec_end_new;
        }
    } else {
        err = caps_create(type, base, size, objsize, count, my_core_id, dest_cte);
    }
    if (err_is_fail(err)) {
        debug(SUBSYS_CAPS, "caps_retype: failed to create a dest cap\n");
        TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
        return err_push(err, SYS_ERR_RETYPE_CREATE);
    }
    TRACE(KERNEL_CAPOPS, RETYPE_CREATE_CAPS_DONE, retype_seqnum);

    /* special initialisation for endpoint caps */
    if (type == ObjType_EndPointLMP) {
        assert(src_cap->type == ObjType_Dispatcher);
        assert(count == 1);
        struct capability *dest_cap = &dest_cte->cap;
        dest_cap->u.endpointlmp.listener = src_cap->u.dispatcher.dcb;
    }

    // XXX: Treat full object retypes to same type as copies as calling
    // is_copy(dst, src) will return true for such retypes.
    if (count == 1 && objsize == get_size(src_cap) && type == src_cap->type) {
        // sanity check: is_copy() really returns true for the two caps
        assert(is_copy(&dest_cte[0].cap, src_cap));
        // If we're not owner, and type needs locality
        if (src_cte->mdbnode.owner != my_core_id &&
            distcap_needs_locality(dest_cte[0].cap.type))
        {
            // fix owner for new cap and set remote_copies bit
            dest_cte[0].mdbnode.owner = src_cte->mdbnode.owner;
            dest_cte[0].mdbnode.remote_copies = true;
        }
    }

    TRACE(KERNEL_CAPOPS, RETYPE_MDB_INSERT, retype_seqnum);
    /* Handle mapping */
    for (size_t i = 0; i < count; i++) {
        mdb_insert(&dest_cte[i]);
    }
    TRACE(KERNEL_CAPOPS, RETYPE_MDB_INSERT_DONE, retype_seqnum);

#ifdef TRACE_PMEM_CAPS
    for (size_t i = 0; i < count; i++) {
        TRACE_CAP_MSG("created", &dest_cte[i]);
    }
#endif

    TRACE(KERNEL, CAP_RETYPE, 1);
    TRACE(KERNEL_CAPOPS, RETYPE_DONE, retype_seqnum);
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
    } else if (dest_type == ObjType_EndPointLMP && src_cte->mdbnode.owner == my_core_id) {
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
    assert(dest_cnode_cte->cap.type == ObjType_L1CNode ||
           dest_cnode_cte->cap.type == ObjType_L2CNode);

    // only allow L2 CNodes and BSP KCB in L1 CNode
    // XXX: BSPKCB should not be in rootcn...
    if (dest_cnode_cte->cap.type == ObjType_L1CNode &&
        src_cte->cap.type != ObjType_L2CNode &&
        src_cte->cap.type != ObjType_KernelControlBlock)
    {
        printk(LOG_WARN, "trying to copy cap type %d into cap type %d\n",
                src_cte->cap.type, dest_cnode_cte->cap.type);
        return SYS_ERR_DEST_TYPE_INVALID;
    }

    struct cte *dest_cte;
    dest_cte = caps_locate_slot(get_address(&dest_cnode_cte->cap), dest_slot);
    return caps_copy_to_cte(dest_cte, src_cte, mint, param1, param2);

}

/// Create copies to a cte
STATIC_ASSERT(68 == ObjType_Num, "Knowledge of all cap types");
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
    case ObjType_EndPointLMP:
        // XXX: FIXME: check that buffer offset lies wholly within the disp frame
        // can't easily enforce this here, because the dispatcher frame may not
        // yet be setup
/*        if (param1 < sizeof(struct dispatcher) ||
            dest_cap->u.endpointlmp.endpointlmp->disp == NULL ||
            param2 < IDC_RECV_LENGTH ||
            param1 + sizeof(struct idc_endpoint) + param2 * sizeof(uintptr_t) >
            (1UL << dest_cap->u.endpointlmp.listener->disp_cte.cap.u.frame.bits)) {
            return SYS_ERR_INVALID_EPBUF;
        }*/
        if (param2 < LMP_RECV_HEADER_LENGTH) {
            return SYS_ERR_INVALID_EPLEN;
        }
        uint16_t iftype = param2 >> 16;
        uint16_t buflen = param2 & 0xFFFF;
        dest_cap->u.endpointlmp.epoffset = param1;
        dest_cap->u.endpointlmp.epbuflen = buflen;
        dest_cap->u.endpointlmp.iftype = iftype;
        break;

    case ObjType_EndPointUMP:
        assert(param2 == 0);
        dest_cap->u.endpointump.iftype = param1;
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
        // Mint the caprights by default
        dest_cap->rights = src_cap->rights & param1;
    }

    // Insert after doing minting operation
    mdb_insert(dest_cte);

    return SYS_ERR_OK;
}

STATIC_ASSERT(68 == ObjType_Num, "Knowledge of all cap types");
errval_t redact_capability(struct capability *cap)
{
    // TODO: figure out which other types need redacting
    switch (cap->type) {
        case ObjType_KernelControlBlock:
            // don't leak KCB kernel pointer in KCB cap
            cap->u.kernelcontrolblock.kcb = NULL;
        default:
            // Don't redact all other capability types
            break;
    }
    return SYS_ERR_OK;
}
