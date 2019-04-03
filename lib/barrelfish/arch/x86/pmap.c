/**
 * \file
 * \brief Pmap code common for x86 archs
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * Copyright (c) 2014, HP Labs.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/pmap.h>
#include "target/x86/pmap_x86.h"
#include <pmap_priv.h> // for set_mapping_cap
#include <pmap_ds.h> // for selected pmap datastructure

// For tracing
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

/**
 * has_vnode() checks whether there exists any vnodes in `len` slots
 * starting from `entry` in vnode `root`, if `only_pages` is set, page table
 * vnodes are ignored 
 */
bool has_vnode(struct vnode *root, uint32_t entry, size_t len,
               bool only_pages)
{
    assert(root != NULL);
    assert(root->v.is_vnode);
    struct vnode *n;

    uint32_t end_entry = entry + len;

    // region we check [entry .. end_entry)
    pmap_foreach_child(root, n) {
        assert(n);
        // n is page table, we need to check if it's anywhere inside the
        // region to check [entry .. end_entry)
        // this amounts to n->entry == entry for len = 1
        if (n->v.is_vnode && n->v.entry >= entry && n->v.entry < end_entry) {
            if (only_pages) {
                return has_vnode(n, 0, PTABLE_ENTRIES, true);
            }
#ifdef LIBBARRELFISH_DEBUG_PMAP
            debug_printf("1: found page table inside our region\n");
#endif
            return true;
        } else if (n->v.is_vnode) {
            // all other vnodes do not overlap with us, so go to next
            assert(n->v.entry < entry || n->v.entry >= end_entry);
            continue;
        }
        // this remains the same regardless of `only_pages`.
        // n is frame [n->v.entry .. end)
        // 3 cases:
        // 1) entry < n->v.entry && end_entry >= end --> n is a strict subset of
        // our region
        // 2) entry inside n (entry >= n->v.entry && entry < end)
        // 3) end_entry inside n (end_entry >= n->v.entry && end_entry < end)
        uint32_t end = n->v.entry + n->v.u.frame.pte_count;
        if (entry < n->v.entry && end_entry >= end) {
#ifdef LIBBARRELFISH_DEBUG_PMAP
            debug_printf("2: found a strict subset of our region: (%"
                    PRIu32"--%"PRIu32") < (%"PRIu32"--%"PRIu32")\n",
                    n->v.entry, end, entry, end_entry);
#endif
            return true;
        }
        if (entry >= n->v.entry && entry < end) {
#ifdef LIBBARRELFISH_DEBUG_PMAP
            debug_printf("3: found a region starting inside our region: (%"
                    PRIu32"--%"PRIu32") <> (%"PRIu32"--%"PRIu32")\n",
                    n->v.entry, end, entry, end_entry);
#endif
            return true;
        }
        if (end_entry > n->v.entry && end_entry < end) {
#ifdef LIBBARRELFISH_DEBUG_PMAP
            debug_printf("4: found a region ending inside our region: (%"
                    PRIu32"--%"PRIu32") <> (%"PRIu32"--%"PRIu32")\n",
                    n->v.entry, end, entry, end_entry);
#endif
            return true;
        }
    }

    return false;
}


/**
 * \brief Allocates a new VNode, adding it to the page table and our metadata
 */
errval_t alloc_vnode(struct pmap_x86 *pmap, struct vnode *root,
                     enum objtype type, uint32_t entry,
                     struct vnode **retvnode, genvaddr_t base)
{
    errval_t err;

    struct vnode *newvnode = slab_alloc(&pmap->p.m.slab);
    if (newvnode == NULL) {
        return LIB_ERR_SLAB_ALLOC_FAIL;
    }

    // The VNode capability
    err = pmap->p.slot_alloc->alloc(pmap->p.slot_alloc, &newvnode->v.cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }
    pmap->used_cap_slots ++;

    err = vnode_create(newvnode->v.cap, type);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VNODE_CREATE);
    }

    // XXX: need to make sure that vnode cap that we will invoke is in our cspace!
    if (get_croot_addr(newvnode->v.cap) != CPTR_ROOTCN) {
        // debug_printf("%s: creating vnode for another domain in that domain's cspace; need to copy vnode cap to our cspace to make it invokable\n", __FUNCTION__);
        err = slot_alloc(&newvnode->v.u.vnode.invokable);
        assert(err_is_ok(err));
        pmap->used_cap_slots ++;
        err = cap_copy(newvnode->v.u.vnode.invokable, newvnode->v.cap);
        assert(err_is_ok(err));
    } else {
        // debug_printf("vnode in our cspace: copying capref to invokable\n");
        newvnode->v.u.vnode.invokable = newvnode->v.cap;
    }
    assert(!capref_is_null(newvnode->v.cap));
    assert(!capref_is_null(newvnode->v.u.vnode.invokable));

    set_mapping_cap(&pmap->p, newvnode, root, entry);
    pmap->used_cap_slots ++;
    assert(!capref_is_null(newvnode->v.mapping));

    // Map it
    err = vnode_map(root->v.u.vnode.invokable, newvnode->v.cap, entry,
                    PTABLE_ACCESS_DEFAULT, 0, 1, newvnode->v.mapping);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pmap_x86: vnode_map\n");
        return err_push(err, LIB_ERR_VNODE_MAP);
    }

    // The VNode meta data
    newvnode->v.is_vnode  = true;
    newvnode->is_cloned = false;
    newvnode->is_pinned = false;
    newvnode->v.entry     = entry;
    newvnode->v.type      = type;
    pmap_vnode_init(&pmap->p, newvnode);
    pmap_vnode_insert_child(root, newvnode);
    newvnode->u.vnode.virt_base = 0;
    newvnode->u.vnode.page_table_frame  = NULL_CAP;
    newvnode->u.vnode.base = base;

#if GLOBAL_MCN
    /* allocate mapping cnodes */
    for (int i = 0; i < MCN_COUNT; i++) {
        err = cnode_create_l2(&newvnode->u.vnode.mcn[i], &newvnode->u.vnode.mcnode[i]);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_PMAP_ALLOC_CNODE);
        }
    }
#endif

    *retvnode = newvnode;
    return SYS_ERR_OK;
}

void remove_empty_vnodes(struct pmap_x86 *pmap, struct vnode *root,
                         uint32_t entry, size_t len)
{
    errval_t err;
    uint32_t end_entry = entry + len;
    struct vnode *n;
    pmap_foreach_child(root, n) {
        assert(n);
        if (n->v.entry >= entry && n->v.entry < end_entry) {
            // sanity check and skip leaf entries
            if (!n->v.is_vnode || n->is_pinned) {
                continue;
            }
            // here we know that all vnodes we're interested in are
            // page tables
            assert(n->v.is_vnode);
            if (n->v.u.vnode.children) {
                remove_empty_vnodes(pmap, n, 0, PTABLE_ENTRIES);
            }

            // unmap
            err = vnode_unmap(root->v.cap, n->v.mapping);
            if (err_is_fail(err)) {
                debug_printf("remove_empty_vnodes: vnode_unmap: %s\n",
                        err_getstring(err));
            }

            // delete mapping cap first: underlying cap needs to exist for
            // this to work properly!
            err = cap_delete(n->v.mapping);
            if (err_is_fail(err)) {
                debug_printf("remove_empty_vnodes: cap_delete (mapping): %s\n",
                        err_getstring(err));
            }
#ifndef GLOBAL_MCN
            err = pmap->p.slot_alloc->free(pmap->p.slot_alloc, n->v.mapping);
            if (err_is_fail(err)) {
                debug_printf("remove_empty_vnodes: slot_free (mapping): %s\n",
                        err_getstring(err));
            }
#endif
            assert(pmap->used_cap_slots > 0);
            pmap->used_cap_slots --;
            // delete capability
            err = cap_delete(n->v.cap);
            if (err_is_fail(err)) {
                debug_printf("remove_empty_vnodes: cap_delete (vnode): %s\n",
                        err_getstring(err));
            }
            if (!capcmp(n->v.cap, n->v.u.vnode.invokable)) {
                // invokable is always allocated in our cspace
                err = cap_destroy(n->v.u.vnode.invokable);
                if (err_is_fail(err)) {
                    debug_printf("remove_empty_vnodes: cap_delete (vnode.invokable): %s\n",
                        err_getstring(err));

                }
            }
            err = pmap->p.slot_alloc->free(pmap->p.slot_alloc, n->v.cap);
            if (err_is_fail(err)) {
                debug_printf("remove_empty_vnodes: slot_free (vnode): %s\n",
                        err_getstring(err));
            }
            assert(pmap->used_cap_slots > 0);
            pmap->used_cap_slots --;

            // remove vnode from list
            pmap_remove_vnode(root, n);

#if GLOBAL_MCN
            /* delete mapping cap cnodes */
            for (int x = 0; x < MCN_COUNT; x++) {
                err = cap_destroy(n->u.vnode.mcn[x]);
                if (err_is_fail(err)) {
                    debug_printf("%s: cap_destroy(mapping cn %d): %s\n",
                            __FUNCTION__, x, err_getcode(err));
                }
            }
#endif
            pmap_vnode_free(&pmap->p, n);
        }
    }
}



/**
 * \brief Determine a suitable address for a given memory object
 *
 * \param pmap    The pmap object
 * \param memobj  The memory object to determine the address for
 * \param alignment Minimum alignment
 * \param retvaddr Pointer to return the determined address
 *
 * Relies on vspace.c code maintaining an ordered list of vregions
 */
errval_t pmap_x86_determine_addr(struct pmap *pmap, struct memobj *memobj,
                                 size_t alignment, genvaddr_t *retvaddr)
{
    trace_event(TRACE_SUBSYS_MEMORY, TRACE_EVENT_MEMORY_DETADDR, 0);
    struct pmap_x86 *pmapx = (struct pmap_x86 *)pmap;
    genvaddr_t vaddr;

    struct vregion *walk = pmap->vspace->head;
    assert(walk != NULL); // assume there's always at least one existing entry

    if (alignment == 0) {
        alignment = BASE_PAGE_SIZE;
    } else {
        alignment = ROUND_UP(alignment, BASE_PAGE_SIZE);
    }
    size_t size = ROUND_UP(memobj->size, alignment);

    // if there's space before the first object, map it there
    genvaddr_t minva = ROUND_UP(pmapx->min_mappable_va, alignment);
    if (minva + size <= vregion_get_base_addr(walk)) {
        vaddr = minva;
        goto out;
    }

    while (walk->next) { // Try to insert between existing mappings
        genvaddr_t walk_base = vregion_get_base_addr(walk);
        genvaddr_t walk_size = ROUND_UP(vregion_get_size(walk), BASE_PAGE_SIZE);
        genvaddr_t walk_end = ROUND_UP(walk_base + walk_size, alignment);
        genvaddr_t next_base = vregion_get_base_addr(walk->next);

        // sanity-check for page alignment
        assert(walk_base % BASE_PAGE_SIZE == 0);
        assert(next_base % BASE_PAGE_SIZE == 0);

        if (next_base > walk_end + size) {
            vaddr = walk_end;
            goto out;
        }

        walk = walk->next;
    }

    // Place beyond last mapping, with alignment
    vaddr = ROUND_UP((vregion_get_base_addr(walk)
                      + ROUND_UP(vregion_get_size(walk), BASE_PAGE_SIZE)),
                     alignment);
 
 out:
    // Ensure that we haven't run out of address space
    if (vaddr + memobj->size > pmapx->max_mappable_va) {
        trace_event(TRACE_SUBSYS_MEMORY, TRACE_EVENT_MEMORY_DETADDR, 1);
        return LIB_ERR_OUT_OF_VIRTUAL_ADDR;
    }

    assert(retvaddr != NULL);
    *retvaddr = vaddr;

    trace_event(TRACE_SUBSYS_MEMORY, TRACE_EVENT_MEMORY_DETADDR, 1);
    return SYS_ERR_OK;
}

errval_t pmap_x86_measure_res(struct pmap *pmap, struct pmap_res_info *buf)
{
    assert(buf);
    struct pmap_x86 *x86 = (struct pmap_x86 *)pmap;

    size_t free_slabs = 0;
    size_t used_slabs = 0;
    // Count allocated and free slabs in bytes; this accounts for all vnodes
    for (struct slab_head *sh = pmap->m.slab.slabs; sh != NULL; sh = sh->next) {
        free_slabs += sh->free;
        used_slabs += sh->total - sh->free;
    }
    buf->vnode_used = used_slabs * pmap->m.slab.blocksize;
    buf->vnode_free = free_slabs * pmap->m.slab.blocksize;

    // Report capability slots in use by pmap
    buf->slots_used = x86->used_cap_slots;

    return SYS_ERR_OK;
}

struct vnode_public *pmap_get_vroot(struct pmap *pmap) {
    struct pmap_x86 *pmapx = (struct pmap_x86 *)pmap;
    return &pmapx->root.v;
}

void pmap_set_min_mappable_va(struct pmap *pmap, lvaddr_t minva)
{
    struct pmap_x86 *pmapx = (struct pmap_x86 *)pmap;
    pmapx->min_mappable_va = minva;
}
