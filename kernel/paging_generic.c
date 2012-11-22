/**
 * \file
 * \brief Kernel memory management.
 */

/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <paging_generic.h>
#include <barrelfish_kpi/paging_arch.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <mdb/mdb_tree.h>
#include <stdio.h>


static size_t get_next_size(enum objtype type)
{
    if (!type_is_vnode(type)) {
        assert(!"makes no sense on non-vnode type");
    }

    size_t size;
    switch (type) {
        case ObjType_VNode_x86_64_pdpt:
            size = 1ULL<<vnode_objbits(ObjType_VNode_x86_64_pml4);
            break;
        case ObjType_VNode_x86_64_pdir:
            size = 1ULL<<vnode_objbits(ObjType_VNode_x86_64_pdpt);
            break;
        case ObjType_VNode_x86_64_ptable:
            size = 1ULL<<vnode_objbits(ObjType_VNode_x86_64_pdir);
            break;
#ifdef PAE
        case ObjType_VNode_x86_32_pdir:
            size = 1ULL<<vnode_objbits(ObjType_VNode_x86_32_pdpt);
            break;
#endif
        case ObjType_VNode_x86_32_ptable:
            size = 1ULL<<vnode_objbits(ObjType_VNode_x86_32_pdir);
            break;
        case ObjType_VNode_ARM_l1:
            size = 1ULL<<vnode_objbits(ObjType_VNode_ARM_l2);
            break;
        default:
            assert(!"makes no sense on top level vnode type");
    }

    return size;
}

/*
 * compile_vaddr returns the lowest address that is addressed by entry 'entry'
 * in page table 'ptable'
 */
genvaddr_t compile_vaddr(struct cte *ptable, size_t entry)
{
    if (!type_is_vnode(ptable->cap.type)) {
        return SYS_ERR_VNODE_TYPE;
    }

    genvaddr_t vaddr = 0;
    size_t sw = BASE_PAGE_BITS;
    // figure out how much we need to shift (assuming that
    // compile_vaddr can be used on arbitrary page table types)
    // A couple of cases have fallthroughs in order to avoid having
    // multiple calls to vnode_objbits with the same type argument.
    switch (ptable->cap.type) {
        case ObjType_VNode_x86_64_pml4:
            sw += vnode_objbits(ObjType_VNode_x86_64_pdpt);
        case ObjType_VNode_x86_64_pdpt:
            sw += vnode_objbits(ObjType_VNode_x86_64_pdir);
        case ObjType_VNode_x86_64_pdir:
            sw += vnode_objbits(ObjType_VNode_x86_64_ptable);
        case ObjType_VNode_x86_64_ptable:
            break;
        case ObjType_VNode_x86_32_pdpt:
            sw += vnode_objbits(ObjType_VNode_x86_32_pdir);
        case ObjType_VNode_x86_32_pdir:
            sw += vnode_objbits(ObjType_VNode_x86_32_ptable);
        case ObjType_VNode_x86_32_ptable:
            break;
        case ObjType_VNode_ARM_l2:
            sw += vnode_objbits(ObjType_VNode_ARM_l1);
        case ObjType_VNode_ARM_l1:
            break;
        default:
            assert(0);
    }
    vaddr = (entry & vnode_objbits(ptable->cap.type)) << sw;

    // add next piece of virtual address until we are at root page table
    struct cte *old = ptable;
    struct cte *next;
    errval_t err;
    while (!(ptable->cap.type == ObjType_VNode_x86_64_pml4 ||
#ifdef PAE
             ptable->cap.type == ObjType_VNode_x86_32_pdpt ||
#else
             ptable->cap.type == ObjType_VNode_x86_32_pdir ||
#endif
             ptable->cap.type == ObjType_VNode_ARM_l2))
    {
        int result;
        err = mdb_find_range(0,
                local_phys_to_gen_phys((lpaddr_t)old->mapping_info.pte),
                get_next_size(old->cap.type), MDB_RANGE_FOUND_SURROUNDING, &next,
                &result);
        if (err_is_fail(err)) {
            printf("error in compile_vaddr: mdb_find_range: 0x%"PRIxERRV"\n", err);
            return 0;
        }
        sw += vnode_objbits(old->cap.type);
        size_t offset = old->mapping_info.pte - get_address(&next->cap);
        vaddr |= ((offset & vnode_objbits(next->cap.type)) << sw);
        old = next;
    }

    return vaddr;
}
