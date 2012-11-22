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
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <mdb/mdb_tree.h>
#include <stdio.h>

static inline errval_t find_next_ptable(struct cte *old, struct cte **next)
{
    int result;
    errval_t err;
    err = mdb_find_range(get_type_root(ObjType_RAM),
            local_phys_to_gen_phys((lpaddr_t)old->mapping_info.pte),
            0, MDB_RANGE_FOUND_INNER, next,
            &result);
    if (err_is_fail(err)) {
        printf("error in compile_vaddr: mdb_find_range: 0x%"PRIxERRV"\n", err);
        return err;
    }
    if (result != MDB_RANGE_FOUND_SURROUNDING) {
        printf("(%d) could not find cap associated with 0x%"PRIxLVADDR"\n", result, old->mapping_info.pte);
        return SYS_ERR_VNODE_LOOKUP_NEXT;
    }
    return SYS_ERR_OK;
}

static inline size_t get_offset(struct cte *old, struct cte *next)
{
    return (old->mapping_info.pte - get_address(&next->cap)) / get_pte_size();
}

/*
 * compile_vaddr returns the lowest address that is addressed by entry 'entry'
 * in page table 'ptable'
 */
errval_t compile_vaddr(struct cte *ptable, size_t entry, genvaddr_t *retvaddr)
{
    if (!type_is_vnode(ptable->cap.type)) {
        return SYS_ERR_VNODE_TYPE;
    }

    genvaddr_t vaddr = 0;
    // shift at least by BASE_PAGE_BITS for first vaddr part
    size_t shift = BASE_PAGE_BITS;

    // figure out how much we need to shift (assuming that
    // compile_vaddr can be used on arbitrary page table types)
    // A couple of cases have fallthroughs in order to avoid having
    // multiple calls to vnode_objbits with the same type argument.
    switch (ptable->cap.type) {
        case ObjType_VNode_x86_64_pml4:
            shift += vnode_objbits(ObjType_VNode_x86_64_pdpt);
        case ObjType_VNode_x86_64_pdpt:
            shift += vnode_objbits(ObjType_VNode_x86_64_pdir);
        case ObjType_VNode_x86_64_pdir:
            shift += vnode_objbits(ObjType_VNode_x86_64_ptable);
        case ObjType_VNode_x86_64_ptable:
            break;

        case ObjType_VNode_x86_32_pdpt:
            shift += vnode_objbits(ObjType_VNode_x86_32_pdir);
        case ObjType_VNode_x86_32_pdir:
            shift += vnode_objbits(ObjType_VNode_x86_32_ptable);
        case ObjType_VNode_x86_32_ptable:
            break;

        case ObjType_VNode_ARM_l2:
            shift += vnode_objbits(ObjType_VNode_ARM_l1);
        case ObjType_VNode_ARM_l1:
            break;

        default:
            return SYS_ERR_VNODE_TYPE;
    }

    size_t mask = (1ULL<<vnode_objbits(ptable->cap.type))-1;
    vaddr = ((genvaddr_t)(entry & mask)) << shift;

    // add next piece of virtual address until we are at root page table
    struct cte *old = ptable;
    struct cte *next;
    errval_t err;
    while (!is_root_pt(old->cap.type))
    {
        err = find_next_ptable(old, &next);
        if (err_is_fail(err)) {
            return err;
        }
        // calculate offset into next level ptable
        size_t offset = get_offset(old, next);
        // shift new part of vaddr by old shiftwidth + #entries of old ptable
        shift += vnode_entry_bits(old->cap.type);

        mask = (1ULL<<vnode_objbits(next->cap.type))-1;
        vaddr |= ((offset & mask) << shift);
        old = next;
    }

    *retvaddr = vaddr;
    return SYS_ERR_OK;
}

errval_t unmap_capability(struct cte *mem)
{
    if (!mem->mapping_info.pte) {
        // mem is not mapped, so just return
        return SYS_ERR_OK;
    }

    errval_t err;

    // get leaf pt cap
    struct cte *pgtable;
    err = mdb_find_cap_for_address(mem->mapping_info.pte, &pgtable);
    if (err_is_fail(err)) {
        // no page table, should be ok.
        return SYS_ERR_OK;
    }
    lvaddr_t pt = local_phys_to_mem(gen_phys_to_local_phys(get_address(&pgtable->cap)));
    cslot_t slot = (mem->mapping_info.pte - pt) / PTABLE_ENTRY_SIZE;
    genvaddr_t vaddr;
    compile_vaddr(pgtable, slot, &vaddr);

    do_unmap(pt, slot, vaddr, mem->mapping_info.pte_count);

    // XXX: FIXME: Going to reload cr3 to flush the entire TLB.
    // This is inefficient.
    // The current implementation is also not multicore safe.
    // We should only invalidate the affected entry using invlpg
    // and figure out which remote tlbs to flush.
    uint64_t cr3;
    __asm__ __volatile__("mov %%cr3,%0" : "=a" (cr3) : );
    __asm__ __volatile__("mov %0,%%cr3" :  : "a" (cr3));

    return SYS_ERR_OK;
}
