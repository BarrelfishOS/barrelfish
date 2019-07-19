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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <paging_generic.h>
#include <barrelfish_kpi/paging_arch.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <mdb/mdb_tree.h>
#include <trace/trace.h>

static inline errval_t find_mapping_for_cap(struct cte *cap, struct cte **mapping)
{
    genpaddr_t faddr = get_address(&cap->cap);
    struct cte *next = cap;
    while ((next = mdb_successor(next)) && get_address(&next->cap) == faddr)
    {
        if (next->cap.type == get_mapping_type(cap->cap.type) &&
            next->cap.u.frame_mapping.cap == &cap->cap)
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
    assert(mapping_cte);
    struct Frame_Mapping *mapping = &mapping_cte->cap.u.frame_mapping;
    /*
    errval_t err;
    err = mdb_find_cap_for_address(
            local_phys_to_gen_phys(mapping->pte), next);
    if (err_no(err) == CAPS_ERR_CAP_NOT_FOUND ||
        err_no(err) == SYS_ERR_CAP_NOT_FOUND)
    {
        debug(SUBSYS_PAGING, "could not find cap associated "
                "with 0x%"PRIxLPADDR"\n", mapping->pte);
        return SYS_ERR_VNODE_NOT_INSTALLED;
    }
    if (err_is_fail(err)) {
        debug(SUBSYS_PAGING, "error in compile_vaddr:"
                " mdb_find_range: 0x%"PRIxERRV"\n", err);
        return err;
    }
    */
    if (!mapping->ptable || mapping->ptable->cap.type == ObjType_Null)
    {
        return SYS_ERR_VNODE_NOT_INSTALLED;
    }
    *next = mapping->ptable;

    if (!type_is_vnode((*next)->cap.type)) {
        struct cte *tmp = mdb_predecessor(*next);
        // check if there's a copy of *next that is a vnode, and return that
        // copy, if found.
        while(is_copy(&tmp->cap, &(*next)->cap)) {
            if (type_is_vnode(tmp->cap.type)) {
                *next = tmp;
                return SYS_ERR_OK;
            }
            tmp = mdb_predecessor(tmp);
        }
        tmp = mdb_successor(*next);
        while(is_copy(&tmp->cap, &(*next)->cap)) {
            if (type_is_vnode(tmp->cap.type)) {
                *next = tmp;
                return SYS_ERR_OK;
            }
            tmp = mdb_successor(tmp);
        }

        debug(SUBSYS_CAPS, "found cap not a VNode\n");
        // no copy was vnode
        return SYS_ERR_VNODE_LOOKUP_NEXT;
    }
    return SYS_ERR_OK;
}

/*
 * 'set_cap()' for mapping caps
 */
void create_mapping_cap(struct cte *mapping_cte, struct capability *cap,
                        struct cte *ptable, cslot_t entry, size_t pte_count)
{
    assert(mapping_cte->cap.type == ObjType_Null);
    assert(type_is_vnode(ptable->cap.type));
    // we can currently only handle page tables with less than 2^16 entries.
    assert(entry < UINT16_MAX);
    assert(pte_count < UINT16_MAX);

    mapping_cte->cap.type = get_mapping_type(cap->type);
    mapping_cte->cap.u.frame_mapping.cap = cap;
    mapping_cte->cap.u.frame_mapping.ptable = ptable;
    mapping_cte->cap.u.frame_mapping.entry = entry;
    mapping_cte->cap.u.frame_mapping.pte_count = pte_count;
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

        case ObjType_VNode_AARCH64_l0:
            shift += vnode_objbits(ObjType_VNode_AARCH64_l1);
        case ObjType_VNode_AARCH64_l1:
            shift += vnode_objbits(ObjType_VNode_AARCH64_l2);
        case ObjType_VNode_AARCH64_l2:
            shift += vnode_objbits(ObjType_VNode_AARCH64_l3);
        case ObjType_VNode_AARCH64_l3:
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
        // no next page table
        if (err == SYS_ERR_VNODE_NOT_INSTALLED ||
            err == SYS_ERR_VNODE_LOOKUP_NEXT)
        {
            *retvaddr = 0;
            return SYS_ERR_VNODE_NOT_INSTALLED;
        }
        if (err_is_fail(err)) {
            return err;
        }
        // calculate offset into next level ptable
        size_t offset = mapping->cap.u.frame_mapping.entry * get_pte_size();
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
    TRACE_CTE(KERNEL_CAPOPS, UNMAP_CAPABILITY, mem);

    TRACE_CAP_MSG("unmapping", mem);

    genvaddr_t vaddr = 0;
    bool single_page_flush = false;
    int mapping_count = 0, unmap_count = 0;
    genpaddr_t faddr = get_address(&mem->cap);

    // iterate over all mappings associated with 'mem' and unmap them
    struct cte *next = mem;
    struct cte *to_delete = NULL;

    while ((next = mdb_successor(next)) && get_address(&next->cap) == faddr) {
        TRACE_CAP_MSG("looking at", next);
        if (next->cap.type == get_mapping_type(mem->cap.type) &&
            next->cap.u.frame_mapping.cap == &mem->cap)
        {
            TRACE_CAP_MSG("cleaning up mapping", next);
            mapping_count ++;

            // do unmap
            struct Frame_Mapping *mapping = &next->cap.u.frame_mapping;
            struct cte *pgtable = mapping->ptable;
            if (!pgtable) {
                debug(SUBSYS_PAGING, "mapping->ptable == 0: just deleting mapping\n");
                // mem is not mapped, so just return
                goto delete_mapping;
            }
            if (!type_is_vnode(pgtable->cap.type)) {
                debug(SUBSYS_PAGING,
                        "mapping->ptable.type not vnode (%d): just deleting mapping\n",
                        mapping->ptable->cap.type);
                // mem is not mapped, so just return
                goto delete_mapping;
            }

            lpaddr_t ptable_lp = gen_phys_to_local_phys(get_address(&pgtable->cap));
            lvaddr_t ptable_lv = local_phys_to_mem(ptable_lp);
            cslot_t slot = mapping->entry;

            // unmap
            do_unmap(ptable_lv, slot, mapping->pte_count);

            unmap_count ++;

            // TLB flush?
            if (unmap_count == 1) {
                err = compile_vaddr(pgtable, slot, &vaddr);
                if (err_is_ok(err) && mapping->pte_count == 1) {
                    single_page_flush = true;
                }
            }

delete_mapping:
            assert(!next->delete_node.next);
            // mark mapping cap for delete: cannot do delete here as it messes
            // up mdb_successor()
            next->delete_node.next = to_delete;
            to_delete = next;
        }
    }

    // delete mapping caps
    while (to_delete) {
        next = to_delete->delete_node.next;
        err = caps_delete(to_delete);
        if (err_is_fail(err)) {
            printk(LOG_NOTE, "caps_delete: %"PRIuERRV"\n", err);
        }
        to_delete = next;
    }

    TRACE_CAP_MSGF(mem, "unmapped %d/%d instances", unmap_count, mapping_count);

    // do TLB flush
    if (single_page_flush) {
        do_one_tlb_flush(vaddr);
    } else {
        do_full_tlb_flush();
    }

    return SYS_ERR_OK;
}

errval_t page_mappings_unmap(struct capability *pgtable, struct cte *mapping)
{
    assert(type_is_vnode(pgtable->type));
    assert(type_is_mapping(mapping->cap.type));
    struct Frame_Mapping *info = &mapping->cap.u.frame_mapping;
    errval_t err;
    debug(SUBSYS_PAGING, "page_mappings_unmap(%hu pages)\n", info->pte_count);


    if (!(pgtable->rights & CAPRIGHTS_WRITE)) {
        return SYS_ERR_DEST_CAP_RIGHTS;
    }

    // calculate page table address
    lvaddr_t pt = local_phys_to_mem(gen_phys_to_local_phys(get_address(pgtable)));

    cslot_t slot = info->entry;
    // get virtual address of first page
    genvaddr_t vaddr;
    bool tlb_flush_necessary = true;
    struct cte *leaf_pt = cte_for_cap(pgtable);
    err = compile_vaddr(leaf_pt, slot, &vaddr);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_VNODE_NOT_INSTALLED && vaddr == 0) {
            debug(SUBSYS_PAGING, "unmapping in floating page table; not flushing TLB\n");
            tlb_flush_necessary = false;
        } else if (err_no(err) == SYS_ERR_VNODE_SLOT_INVALID) {
            debug(SUBSYS_PAGING, "couldn't reconstruct virtual address\n");
        } else {
            printk(LOG_NOTE, "%s: compile_vaddr returned %lu\n", __FUNCTION__, err);
            char buf[256];
            sprint_cap(buf, 256, pgtable);
            printk(LOG_NOTE, "%s: ptable = %p[%s]\n", __FUNCTION__, pgtable, buf);
            sprint_cap(buf, 256, &mapping->cap);
            printk(LOG_NOTE, "%s: mapping = %p[%s]\n", __FUNCTION__, mapping, buf);
            return err;
        }
    }

    do_unmap(pt, slot, info->pte_count);

    // flush TLB for unmapped pages if we got a valid virtual address
    // TODO: heuristic that decides if selective or full flush is more
    //       efficient?
    if (tlb_flush_necessary) {
        if (info->pte_count > 1 || err_is_fail(err)) {
            do_full_tlb_flush();
        } else {
            do_one_tlb_flush(vaddr);
        }
    }

    return SYS_ERR_OK;
}

// TODO: cleanup arch compatibility mess for page size selection
errval_t paging_tlb_flush_range(struct cte *mapping_cte, size_t offset, size_t pages)
{
    assert(type_is_mapping(mapping_cte->cap.type));

    struct Frame_Mapping *mapping = &mapping_cte->cap.u.frame_mapping;

    // reconstruct first virtual address for TLB flushing
    struct cte *leaf_pt = mapping->ptable;
    if (!type_is_vnode(leaf_pt->cap.type)) {
        return SYS_ERR_VNODE_TYPE;
    }
    assert(type_is_vnode(leaf_pt->cap.type));
    errval_t err;
    genvaddr_t vaddr;
    size_t entry = mapping->entry;
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
#elif defined(__ARM_ARCH_7A__)
        case ObjType_VNode_ARM_l1:
            panic("large page support for ARM NYI!\n");
            break;
        case ObjType_VNode_ARM_l2:
            page_size = BASE_PAGE_SIZE;
            break;
#elif defined(__ARM_ARCH_8A__)
            // TODO: define ARMv8 paging
        case ObjType_VNode_AARCH64_l3:
            page_size = VMSAv8_64_BASE_PAGE_SIZE;
            break;
        case ObjType_VNode_AARCH64_l2:
            page_size = VMSAv8_64_L2_BLOCK_SIZE;
            break;
        case ObjType_VNode_AARCH64_l1:
            page_size = VMSAv8_64_L1_BLOCK_SIZE;
            break;            
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

