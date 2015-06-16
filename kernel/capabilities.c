/**
 * \file
 * \brief Kernel capability management implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
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

/// Sets the specified number of low-order bits to 1
#define MASK(bits)      ((1UL << bits) - 1)

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
        trace_types_enabled = false;
    }
}
#endif

struct capability monitor_ep;

int sprint_cap(char *buf, size_t len, struct capability *cap)
{
    switch (cap->type) {
    case ObjType_PhysAddr:
        return snprintf(buf, len,
                        "physical address range cap (0x%" PRIxGENPADDR ":%u)",
                        cap->u.physaddr.base, cap->u.physaddr.bits);

    case ObjType_RAM:
        return snprintf(buf, len, "RAM cap (0x%" PRIxGENPADDR ":%u)",
                        cap->u.ram.base, cap->u.ram.bits);

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
        return snprintf(buf, len, "Frame cap (0x%" PRIxGENPADDR ":%u)",
                        cap->u.frame.base, cap->u.frame.bits);

    case ObjType_DevFrame:
        return snprintf(buf, len, "Device Frame cap (0x%" PRIxGENPADDR ":%u)",
                        cap->u.frame.base, cap->u.devframe.bits);

    case ObjType_VNode_ARM_l1:
        return snprintf(buf, len, "ARM L1 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_arm_l1.base);

    case ObjType_VNode_ARM_l2:
        return snprintf(buf, len, "ARM L2 table at 0x%" PRIxGENPADDR,
                        cap->u.vnode_arm_l2.base);

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

    case ObjType_IRQTable:
        return snprintf(buf, len, "IRQTable cap");

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
 * \param bits          Size of memory area as 2^bits.
 * \param objbits       For variable-sized objects, size multiplier as 2^bits.
 *
 * \return Number of objects to be created, or zero on error
 */

// If you create more capability types you need to deal with them
// in the table below.
STATIC_ASSERT(27 == ObjType_Num, "Knowledge of all cap types");

static size_t caps_numobjs(enum objtype type, uint8_t bits, uint8_t objbits)
{
    switch(type) {
    case ObjType_PhysAddr:
    case ObjType_RAM:
    case ObjType_Frame:
    case ObjType_DevFrame:
        if (objbits > bits) {
            return 0;
        } else {
            return 1UL << (bits - objbits);
        }

    case ObjType_CNode:
        if (bits < OBJBITS_CTE || objbits > bits - OBJBITS_CTE) {
            return 0;
        } else {
            return 1UL << (bits - OBJBITS_CTE - objbits);
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
    {
        size_t objbits_vnode = vnode_objbits(type);
        if (bits < objbits_vnode) {
            return 0;
        } else {
            return 1UL << (bits - objbits_vnode);
        }
    }

    case ObjType_Dispatcher:
        if (bits < OBJBITS_DISPATCHER) {
            return 0;
        } else {
            return 1UL << (bits - OBJBITS_DISPATCHER);
        }

    case ObjType_KernelControlBlock:
        if (bits < OBJBITS_KCB) {
            return 0;
        } else {
            return 1UL << (bits - OBJBITS_KCB);
        }

    case ObjType_Kernel:
    case ObjType_IRQTable:
    case ObjType_IO:
    case ObjType_EndPoint:
    case ObjType_ID:
    case ObjType_Notify_RCK:
    case ObjType_Notify_IPI:
    case ObjType_PerfMon:
    case ObjType_IPI:
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
STATIC_ASSERT(ObjType_Num == 27, "Knowledge of all cap types");

static errval_t caps_init_objects(enum objtype type, lpaddr_t lpaddr, uint8_t
                                  bits, uint8_t objbits, size_t numobjs)
{
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
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        // XXX: SCC hack, while we don't have a devframe allocator
        if(lpaddr + ((lpaddr_t)1 << bits) < PADDR_SPACE_LIMIT) {
            memset((void*)lvaddr, 0, (lvaddr_t)1 << bits);
        } else {
            printk(LOG_WARN, "Allocating RAM at 0x%" PRIxLPADDR
                   " uninitialized\n", lpaddr);
        }
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);
        break;

    case ObjType_CNode:
    case ObjType_VNode_ARM_l1:
    case ObjType_VNode_ARM_l2:
    case ObjType_VNode_x86_32_ptable:
    case ObjType_VNode_x86_32_pdir:
    case ObjType_VNode_x86_32_pdpt:
    case ObjType_VNode_x86_64_ptable:
    case ObjType_VNode_x86_64_pdir:
    case ObjType_VNode_x86_64_pdpt:
    case ObjType_VNode_x86_64_pml4:
    case ObjType_Dispatcher:
    case ObjType_KernelControlBlock:
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);
        break;

    default:
        break;

    }

    return SYS_ERR_OK;
}

/**
 * \brief Create capabilities to kernel objects.
 *
 * This function creates kernel objects of 'type' into the memory
 * area, based at 'addr' and of size 2^'bits', so they completely fill the
 * area. For each created kernel object, a capability is created to it and
 * put consecutively into the array of CTEs pointed to by 'caps'. The array
 * needs to have the appropriate size to hold all created caps. Some kernel
 * objects can have a variable size. In that case, 'objbits' should be non-zero
 * and give the a size multiplier as 2^'objbits'.
 *
 * \param type          Type of objects to create.
 * \param addr          Base address in the local address space.
 * \param bits          Size of memory area as 2^bits.
 * \param objbits       For variable-sized objects, size multiplier as 2^bits.
 * \param numobjs       Number of objects to be created, from caps_numobjs()
 * \param dest_caps     Pointer to array of CTEs to hold created caps.
 *
 * \return Error code
 */
// If you create more capability types you need to deal with them
// in the table below.
STATIC_ASSERT(27 == ObjType_Num, "Knowledge of all cap types");

static errval_t caps_create(enum objtype type, lpaddr_t lpaddr, uint8_t bits,
                            uint8_t objbits, size_t numobjs, coreid_t owner,
                            struct cte *dest_caps)
{
    errval_t err;

    /* Parameter checking */
    assert(dest_caps != NULL);
    assert(type != ObjType_Null);
    assert(type < ObjType_Num);
    assert(numobjs > 0);

    genpaddr_t genpaddr = local_phys_to_gen_phys(lpaddr);

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
    struct capability src_cap;
    memset(&src_cap, 0, sizeof(struct capability));
    src_cap.type = type;
    // XXX: Handle rights!
    src_cap.rights = CAPRIGHTS_ALLRIGHTS;

    if (owner == my_core_id) {
        // If we're creating new local objects, they need to be initialized
        err = caps_init_objects(type, lpaddr, bits, objbits, numobjs);
        if (err_is_fail(err)) {
            return err;
        }
    }

    size_t dest_i = 0;
    err = SYS_ERR_OK;

    /* Set the type specific fields and insert into #dest_caps */
    switch(type) {
    case ObjType_Frame:
        TRACE(KERNEL, BZERO, 1);
        // XXX: SCC hack, while we don't have a devframe allocator
        if(lpaddr + ((lpaddr_t)1 << bits) < PADDR_SPACE_LIMIT) {
            memset((void*)lvaddr, 0, (lvaddr_t)1 << bits);
        } else {
            printk(LOG_WARN, "Allocating RAM at 0x%" PRIxLPADDR
                   " uninitialized\n", lpaddr);
        }
        TRACE(KERNEL, BZERO, 0);
        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.frame.base = genpaddr + dest_i * ((genpaddr_t)1 << objbits);
            src_cap.u.frame.bits = objbits;
            // Insert the capabilities
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_PhysAddr:
        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.physaddr.base = genpaddr + dest_i * ((genpaddr_t)1 << objbits);
            src_cap.u.physaddr.bits = objbits;
            // Insert the capabilities
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_RAM:
        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.ram.base = genpaddr + dest_i * ((genpaddr_t)1 << objbits);
            src_cap.u.ram.bits = objbits;
            // Insert the capabilities
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_DevFrame:
        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.devframe.base = genpaddr + dest_i * ((genpaddr_t)1 << objbits);
            src_cap.u.devframe.bits = objbits;
            // Insert the capabilities
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_CNode:
        assert((1UL << OBJBITS_CTE) >= sizeof(struct cte));
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.cnode.cnode =
                lpaddr + dest_i * ((lpaddr_t)1 << (objbits + OBJBITS_CTE));
            src_cap.u.cnode.bits = objbits;
            src_cap.u.cnode.guard = 0;
            src_cap.u.cnode.guard_size = 0;
            // XXX: Handle rights!
            src_cap.u.cnode.rightsmask = CAPRIGHTS_ALLRIGHTS;
            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_VNode_ARM_l1:
    {
        size_t objbits_vnode = vnode_objbits(type);

        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.vnode_arm_l1.base =
                genpaddr + dest_i * ((genpaddr_t)1 << objbits_vnode);

#ifdef __arm__
            // Insert kernel/mem mappings into new table.
            paging_make_good(
                gen_phys_to_local_phys(
                    local_phys_to_mem(src_cap.u.vnode_arm_l1.base)
                ),
                1u << objbits_vnode
                );
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }

        break;
    }

    case ObjType_VNode_ARM_l2:
    {
        size_t objbits_vnode = vnode_objbits(type);

        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.vnode_arm_l2.base =
                genpaddr + dest_i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_32_ptable:
    {
        size_t objbits_vnode = vnode_objbits(type);

        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_32_ptable.base =
                genpaddr + dest_i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_32_pdir:
    {
        size_t objbits_vnode = vnode_objbits(type);

        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_32_pdir.base =
                genpaddr + dest_i * ((genpaddr_t)1 << objbits_vnode);

#if defined(__i386__) && !defined(CONFIG_PAE)
            // Make it a good PDE by inserting kernel/mem VSpaces
            lpaddr = gen_phys_to_local_phys(src_cap.u.vnode_x86_32_pdir.base);
            paging_x86_32_make_good_pdir(lpaddr);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_32_pdpt:
    {
        size_t objbits_vnode = vnode_objbits(type);

        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_32_pdir.base =
                genpaddr + dest_i * ((genpaddr_t)1 << objbits_vnode);

#if defined(__i386__) && defined(CONFIG_PAE)
            // Make it a good PDPTE by inserting kernel/mem VSpaces
            lpaddr_t var =
                gen_phys_to_local_phys(src_cap.u.vnode_x86_32_pdpt.base);
            paging_x86_32_make_good_pdpte(var);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_64_ptable:
    {
        size_t objbits_vnode = vnode_objbits(type);

        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_64_ptable.base =
                genpaddr + dest_i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_64_pdir:
    {
        size_t objbits_vnode = vnode_objbits(type);

        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_64_pdir.base =
                genpaddr + dest_i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_64_pdpt:
    {
        size_t objbits_vnode = vnode_objbits(type);

        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_64_pdpt.base =
                genpaddr + dest_i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;
    }

    case ObjType_VNode_x86_64_pml4:
    {
        size_t objbits_vnode = vnode_objbits(type);

        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_64_pml4.base =
                genpaddr + dest_i * ((genpaddr_t)1 << objbits_vnode);

#if defined(__x86_64__) || defined(__k1om__)
            // Make it a good PML4 by inserting kernel/mem VSpaces
            lpaddr_t var = gen_phys_to_local_phys(src_cap.u.vnode_x86_64_pml4.base);
            paging_x86_64_make_good_pml4(var);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }

        break;
    }

    case ObjType_Dispatcher:
        assert((1UL << OBJBITS_DISPATCHER) >= sizeof(struct dcb));
        TRACE(KERNEL, BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        TRACE(KERNEL, BZERO, 0);

        for(dest_i = 0; dest_i < numobjs; dest_i++) {
            // Initialize type specific fields
            src_cap.u.dispatcher.dcb = (struct dcb *)
                (lvaddr + dest_i * (1UL << OBJBITS_DISPATCHER));
            // Insert the capability
            err = set_cap(&dest_caps[dest_i].cap, &src_cap);
            if (err_is_fail(err)) {
                break;
            }
        }
        break;

    case ObjType_ID:
        // ID type does not refer to a kernel object
        assert(lpaddr  == 0);
        assert(bits    == 0);
        assert(objbits == 0);
        assert(numobjs == 1);

        // Prevent wrap around
        if (id_cap_counter >= UINT32_MAX) {
            return SYS_ERR_ID_SPACE_EXHAUSTED;
        }

        // Generate a new ID, core_local_id monotonically increases
        src_cap.u.id.coreid = my_core_id;
        src_cap.u.id.core_local_id = id_cap_counter++;

        // Insert the capability
        err = set_cap(&dest_caps->cap, &src_cap);
        break;

    case ObjType_IO:
        src_cap.u.io.start = 0;
        src_cap.u.io.end   = 65535;
        /* fall through */

    case ObjType_Kernel:
    case ObjType_IPI:
    case ObjType_IRQTable:
    case ObjType_EndPoint:
    case ObjType_Notify_RCK:
    case ObjType_Notify_IPI:
    case ObjType_PerfMon:
        // These types do not refer to a kernel object
        assert(lpaddr  == 0);
        assert(bits    == 0);
        assert(objbits == 0);
        assert(numobjs == 1);

        // Insert the capability
        err = set_cap(&dest_caps->cap, &src_cap);
        if (err_is_ok(err)) {
            dest_i = 1;
        }
        break;

    case ObjType_KernelControlBlock:
        assert((1UL << OBJBITS_KCB) >= sizeof(struct dcb));

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.kernelcontrolblock.kcb = (struct kcb *)
                (lvaddr + i * (1UL << OBJBITS_KCB));
            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
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

/// Create caps to new kernel objects.
errval_t caps_create_new(enum objtype type, lpaddr_t addr, size_t bits,
                         size_t objbits, coreid_t owner, struct cte *caps)
{
    TRACE(KERNEL, CAP_CREATE_NEW, 0);
    /* Parameter checking */
    assert(type != ObjType_EndPoint); // Cap of this type cannot be created

    size_t numobjs = caps_numobjs(type, bits, objbits);
    assert(numobjs > 0);

    /* Create the new capabilities */
    errval_t err = caps_create(type, addr, bits, objbits, numobjs, owner, caps);
    if (err_is_fail(err)) {
        return err;
    }

    // Handle the mapping database
    set_init_mapping(caps, numobjs);

    TRACE_CAP_MSG("created", &caps[0]);

    TRACE(KERNEL, CAP_CREATE_NEW, 1);
    return SYS_ERR_OK;
}


/// Retype caps
errval_t caps_retype(enum objtype type, size_t objbits,
                     struct capability *dest_cnode, cslot_t dest_slot,
                     struct cte *src_cte, bool from_monitor)
{
    TRACE(KERNEL, CAP_RETYPE, 0);
    size_t numobjs;
    uint8_t bits = 0;
    genpaddr_t base = 0;
    errval_t err;

    /* Parameter checking */
    assert(type != ObjType_Null);
    assert(type < ObjType_Num);

    struct capability *src_cap = &src_cte->cap;

    TRACE_CAP_MSG("retyping", src_cte);

    /* Check retypability */
    err = is_retypeable(src_cte, src_cap->type, type, from_monitor);
    if (err_is_fail(err)) {
        debug(SUBSYS_CAPS, "caps_retype: is_retypeable failed\n");
        return err;
    }

    /* Create Destination caps as per source and destination type */
    switch(src_cap->type) {
    case ObjType_PhysAddr:
        bits = src_cap->u.physaddr.bits;
        base = src_cap->u.physaddr.base;
        break;

    case ObjType_RAM:
        bits = src_cap->u.ram.bits;
        base = src_cap->u.ram.base;
        break;

    case ObjType_Dispatcher:
        bits = base = 0;
        break;

    case ObjType_Frame:
        bits = src_cap->u.frame.bits;
        base = src_cap->u.frame.base;
        break;

    case ObjType_DevFrame:
        bits = src_cap->u.devframe.bits;
        base = src_cap->u.devframe.base;
        break;

    default:
        panic("Unreachable case");
    }

    /* determine number of objects to be created */
    numobjs = caps_numobjs(type, bits, objbits);

    if (numobjs == 0) {
        debug(SUBSYS_CAPS, "caps_retype: numobjs == 0\n");
        return SYS_ERR_INVALID_SIZE_BITS;
    }
   // debug(SUBSYS_CAPS, "caps_retype: numobjs == %d\n", (int)numobjs);

    /* check that destination slots all fit within target cnode */
    if (dest_slot + numobjs > (1UL << dest_cnode->u.cnode.bits)) {
        debug(SUBSYS_CAPS, "caps_retype: dest slots don't fit in cnode\n");
        return SYS_ERR_SLOTS_INVALID;
    }

    /* check that destination slots are all empty */
    debug(SUBSYS_CAPS, "caps_retype: dest cnode is %#" PRIxLPADDR
          " dest_slot %d\n",
          dest_cnode->u.cnode.cnode, (int)dest_slot);
    for (cslot_t i = 0; i < numobjs; i++) {
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
    err = caps_create(type, base, bits, objbits, numobjs, my_core_id, dest_cte);
    if (err_is_fail(err)) {
        debug(SUBSYS_CAPS, "caps_retype: failed to create a dest cap\n");
        return err_push(err, SYS_ERR_RETYPE_CREATE);
    }

    /* special initialisation for endpoint caps */
    if (type == ObjType_EndPoint) {
        assert(src_cap->type == ObjType_Dispatcher);
        assert(numobjs == 1);
        struct capability *dest_cap = &dest_cte->cap;
        dest_cap->u.endpoint.listener = src_cap->u.dispatcher.dcb;
    }

    /* Handle mapping */
    for (size_t i = 0; i < numobjs; i++) {
        mdb_insert(&dest_cte[i]);
    }

#ifdef TRACE_PMEM_CAPS
    for (size_t i = 0; i < numobjs; i++) {
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
        printf("err_revoke_first: (%p, %d, %d)\n", src_cte, src_type, dest_type);
        return SYS_ERR_REVOKE_FIRST;
    } else if (dest_type == ObjType_EndPoint && src_cte->mdbnode.owner == my_core_id) {
        // XXX: because of the current "multi-retype" hack for endpoints, a
        // dispatcher->endpoint retype can happen irrespective of the existence
        // of descendents on any core.
        // Howevery, we only do this for locally owned caps as the owner should
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
