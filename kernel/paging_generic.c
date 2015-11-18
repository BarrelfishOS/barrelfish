/**
 * \file
 * \brief Kernel memory management.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * Copyright (c) 2014, HP Labs.
 * Copyright (c) 2015, Hewlett Packard Enterprise Development LP.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <paging_generic.h>
#include <barrelfish_kpi/paging_arch.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <mdb/mdb_tree.h>
#include <stdio.h>

static inline errval_t find_mapping_for_cap(struct cte *cap, struct cte **mapping)
{
    genpaddr_t faddr = get_address(&cap->cap);
    struct cte *next = cap;
    while ((next = mdb_successor(next)) && get_address(&next->cap) == faddr)
    {
        if (next->cap.type == get_mapping_type(cap->cap.type) &&
            next->cap.u.frame_mapping.frame == &cap->cap)
        {
            *mapping = next;
            return SYS_ERR_OK;
        }
    }
    return SYS_ERR_CAP_NOT_FOUND;
}

// TODO: XXX: multiple mappings?
static inline errval_t find_next_ptable(struct cte *mapping_cte, struct cte **next)
{
    errval_t err;
    assert(mapping_cte);
    struct Frame_Mapping *mapping = &mapping_cte->cap.u.frame_mapping;
    err = mdb_find_cap_for_address(
            local_phys_to_gen_phys(mapping->pte), next);
    if (err_no(err) == CAPS_ERR_CAP_NOT_FOUND) {
        debug(SUBSYS_PAGING, "could not find cap associated "
                "with 0x%"PRIxLPADDR"\n", mapping->pte);
        return SYS_ERR_VNODE_NOT_INSTALLED;
    }
    if (err_is_fail(err)) {
        debug(SUBSYS_PAGING, "error in compile_vaddr:"
                " mdb_find_range: 0x%"PRIxERRV"\n", err);
        return err;
    }
    if (!type_is_vnode((*next)->cap.type)) {
        return SYS_ERR_VNODE_LOOKUP_NEXT;
    }
    return SYS_ERR_OK;
}

static inline size_t get_offset(struct cte *mapping, struct cte *next)
{
    return (mapping->cap.u.frame_mapping.pte - get_address(&next->cap)) / get_pte_size();
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

        case ObjType_VNode_AARCH64_l3:
            shift += vnode_objbits(ObjType_VNode_AARCH64_l2);
        case ObjType_VNode_AARCH64_l2:
            shift += vnode_objbits(ObjType_VNode_AARCH64_l1);
        case ObjType_VNode_AARCH64_l1:
            break;

        default:
            return SYS_ERR_VNODE_TYPE;
    }

    size_t mask = (1ULL<<vnode_objbits(ptable->cap.type))-1;
    vaddr = ((genvaddr_t)(entry & mask)) << shift;

    // add next piece of virtual address until we are at root page table
    struct cte *old = ptable;
    struct cte *next, *mapping = NULL;
    errval_t err;
    while (!is_root_pt(old->cap.type))
    {
        err = find_mapping_for_cap(old, &mapping);
        if (err_is_fail(err)) {
            // no mapping found, cannot reconstruct vaddr
            *retvaddr = 0;
            return SYS_ERR_VNODE_NOT_INSTALLED;
        }
        err = find_next_ptable(mapping, &next);
        if (err == SYS_ERR_VNODE_NOT_INSTALLED) { // no next page table
            *retvaddr = 0;
            return SYS_ERR_VNODE_NOT_INSTALLED;
        }
        if (err_is_fail(err)) {
            return err;
        }
        // calculate offset into next level ptable
        size_t offset = get_offset(mapping, next);
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
    errval_t err;

    genpaddr_t faddr = get_address(&mem->cap);
    struct cte *next = NULL;
    next = mdb_successor(mem);
    if (!next) {
        // corner case frame is last element of tree -> no mappings found
        return SYS_ERR_OK;
    }

    genvaddr_t vaddr = 0;
    bool single_page_flush = false;
    int mapping_count = 0;
    // iterate over all mappings associated with 'mem' and unmap them
    do {
        mapping_count ++;
        if (next->cap.type == get_mapping_type(mem->cap.type) &&
            next->cap.u.frame_mapping.frame == &mem->cap)
        {
            // delete mapping cap
            err = caps_delete(next);
            if (err_is_fail(err)) {
                printk(LOG_NOTE, "%s: caps_delete(mapping): %ld\n", __FUNCTION__, err);
            }

            // do unmap
            struct Frame_Mapping *mapping = &next->cap.u.frame_mapping;
            if (!mapping->pte) {
                // mem is not mapped, so just return
                return SYS_ERR_OK;
            }

            // get leaf pt cap
            struct cte *pgtable;
            err = mdb_find_cap_for_address(mapping->pte, &pgtable);
            if (err_is_fail(err)) {
                // no page table found, should be ok.
                return SYS_ERR_OK;
            }

            lpaddr_t ptable_lp = gen_phys_to_local_phys(get_address(&pgtable->cap));
            lvaddr_t ptable_lv = local_phys_to_mem(ptable_lp);
            cslot_t slot = (mapping->pte - ptable_lp) / PTABLE_ENTRY_SIZE;

            // unmap
            do_unmap(ptable_lv, slot, mapping->pte_count);

            // TLB flush?
            if (mapping_count == 1) {
                err = compile_vaddr(pgtable, slot, &vaddr);
                if (err_is_ok(err) && mapping->pte_count == 1) {
                    single_page_flush = true;
                }
            }
        }
    } while ((next = mdb_successor(next)) && get_address(&next->cap) == faddr);

    // do TLB flush
    if (single_page_flush) {
        do_one_tlb_flush(vaddr);
    } else {
        do_full_tlb_flush();
    }

    return SYS_ERR_OK;
}

// TODO: cleanup arch compatibility mess for page size selection
errval_t paging_tlb_flush_range(struct cte *mapping_cte, size_t offset, size_t pages)
{
    assert(type_is_mapping(mapping_cte->cap.type));

    struct Frame_Mapping *mapping = &mapping_cte->cap.u.frame_mapping;

    // reconstruct first virtual address for TLB flushing
    struct cte *leaf_pt;
    errval_t err;
    err = mdb_find_cap_for_address(mapping->pte, &leaf_pt);
    if (err_is_fail(err)) {
        return err;
    }
    genvaddr_t vaddr;
    size_t entry = (mapping->pte - get_address(&leaf_pt->cap)) /
        PTABLE_ENTRY_SIZE;
    entry += offset;
    err = compile_vaddr(leaf_pt, entry, &vaddr);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_VNODE_NOT_INSTALLED) {
            debug(SUBSYS_PAGING, "couldn't reconstruct virtual address\n");
        }
        else {
            return err;
        }
    }
    debug(SUBSYS_PAGING, "flushing TLB entries for vaddrs 0x%"
            PRIxGENVADDR"--0x%"PRIxGENVADDR"\n",
            vaddr, vaddr+(pages * BASE_PAGE_SIZE));
    // flush TLB entries for all modified pages
    size_t page_size = 0;
    switch(leaf_pt->cap.type) {
#if defined(__x86_64__)
        case ObjType_VNode_x86_64_ptable:
            page_size = X86_64_BASE_PAGE_SIZE;
            break;
        case ObjType_VNode_x86_64_pdir:
            page_size = X86_64_LARGE_PAGE_SIZE;
            break;
        case ObjType_VNode_x86_64_pdpt:
            page_size = X86_64_HUGE_PAGE_SIZE;
            break;
#elif defined(__i386__)
        case ObjType_VNode_x86_32_ptable:
            page_size = X86_32_BASE_PAGE_SIZE;
            break;
        case ObjType_VNode_x86_32_pdir:
            page_size = X86_32_LARGE_PAGE_SIZE;
            break;
#elif defined(__ARM_ARCH_5__)
            // XXX: cannot add code here without breaking CPU driver?!
            // -SG, 2015-05-04.
#elif defined(__ARM_ARCH_7A__)
        case ObjType_VNode_ARM_l1:
            panic("large page support for ARM NYI!\n");
            break;
        case ObjType_VNode_ARM_l2:
            page_size = BASE_PAGE_SIZE;
            break;
#elif defined(__ARM_ARCH_7M__)
            // -M profile chips don't have a TLB
#elif defined(__ARM_ARCH_8A__)
            // TODO: define ARMv8 paging
#else
#error setup page sizes for arch
#endif
        default:
            panic("cannot find page size for cap type: %d\n",
                  leaf_pt->cap.type);
            break;
    }
    assert(page_size);
    // TODO: check what tlb flushing instructions expect for large/huge pages
    for (int i = 0; i < pages; i++) {
        do_one_tlb_flush(vaddr);
        vaddr += page_size;
    }

    return SYS_ERR_OK;
}
