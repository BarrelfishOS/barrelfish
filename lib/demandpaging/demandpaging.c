/*
 * \brief demandpaging.c
 *
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/except.h>
#include <barrelfish/memobj.h>
#include "../barrelfish/vspace/vspace_internal.h"
#include <vfs/vfs.h>

#include <dp_internal.h>


struct demand_paging_region *demand_paging_regions = NULL;


/*
 * ===========================================================================
 * helper functions
 * ===========================================================================
 */

static bool is_dirty(struct dp_page *dpp)
{
    union x86_64_ptable_entry *entry = dpp->vnode_entry;
    switch(dpp->dpr->pagesize) {
        case BASE_PAGE_SIZE:
            assert(entry->base.present == 1);
            return entry->base.dirty;
            break;
        case LARGE_PAGE_SIZE:
            assert(entry->large.present == 1);
            return entry->large.dirty;
            break;
        case HUGE_PAGE_SIZE:
            assert(entry->huge.present == 1);
            return entry->huge.dirty;
            break;
        default:
            return -1;
            break;

    }
    return 1;
}

/*
 * ===========================================================================
 * swapping of pages
 * ===========================================================================
 */

static errval_t swap_in(struct dp_page *dpp)
{
    errval_t err;
    struct demand_paging_region *dpr = dpp->dpr;

    lvaddr_t offset = dpp->vaddr - vregion_get_base_addr(&dpr->vreg);


    DP_DEBUG_SWAP("[in] page=%" PRIx64 "\n", dpp->vaddr );

    size_t read;
    size_t totalread = 0;

    err = vfs_seek(dpr->swapfile, VFS_SEEK_SET, offset);
    if (err_is_fail(err)) {
        return err;
    }

    while(totalread < dpr->pagesize) {
        err = vfs_read(dpr->swapfile, (void *)dpp->vaddr + totalread,
                       dpr->pagesize - totalread, &read);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "writing to fail");
        }

        totalread += read;
    }

    dpp->state = DEMAND_PAGING_PST_MEMORY;

    return SYS_ERR_OK;
}

static errval_t swap_out(struct dp_page *dpp)
{
    errval_t err;

    struct demand_paging_region *dpr = dpp->dpr;
    lvaddr_t offset = dpp->vaddr - vregion_get_base_addr(&dpr->vreg);

    DP_DEBUG_SWAP("[out] page=%" PRIx64 "\n", dpp->vaddr );

    size_t written;
    size_t totalwritten = 0;

    err = vfs_seek(dpr->swapfile, VFS_SEEK_SET, offset);
    if (err_is_fail(err)) {
        return err;
    }

    while(totalwritten < dpr->pagesize) {
        err = vfs_write(dpr->swapfile, (void *)dpp->vaddr + totalwritten,
                        dpr->pagesize - totalwritten, &written);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "writing to fail");
        }
        totalwritten += written;
    }

    dpp->state = DEMAND_PAGING_PST_FILE;

    return SYS_ERR_OK;
}

/*
 * ===========================================================================
 * mapping of pages
 * ===========================================================================
 */

static inline errval_t frame_map(struct demand_paging_region *dpr,
                                 struct dp_page *dpp, struct dp_frame *dpf)
{
    DP_DEBUG_MAP("[map] vaddr= 0x%" PRIx64 "\n", dpp->vaddr);
    struct pmap *pmap = vregion_get_vspace(&dpr->vreg)->pmap;
    dpf->page = dpp;
    dpf->vnode_entry = dpp->vnode_entry;
    return pmap->f.map(pmap, dpp->vaddr, dpf->frame, 0, dpr->pagesize,
                       vregion_get_flags(&dpr->vreg), NULL, NULL);
}

static inline errval_t frame_unmap(struct demand_paging_region *dpr, struct dp_page *dpp)
{
    DP_DEBUG_MAP("[unmap] vaddr= 0x%" PRIx64 "\n", dpp->vaddr);
    struct pmap *pmap = vregion_get_vspace(&dpr->vreg)->pmap;
    return pmap->f.unmap(pmap, dpp->vaddr, dpr->pagesize, NULL);
}


/*
 * ===========================================================================
 * frame evict policy
 * ===========================================================================
 */

static inline errval_t frame_evict(struct demand_paging_region *dpr,
                                   struct dp_frame *dpf)
{
    errval_t err;

    if (is_dirty(dpf->page)) {
        /* is dirty */
        swap_out(dpf->page);
    }

    err = frame_unmap(dpr, dpf->page);
    if (err_is_fail(err)) {
        return err;
    }
    dpf->page = NULL;
    dpf->vnode_entry = NULL;

    return SYS_ERR_OK;
}


static errval_t frame_evict_any(struct demand_paging_region *dpr,
                                struct dp_frame **ret_dpf)
{
    errval_t err;

    DP_DEBUG_SWAP("[evict] victim=%" PRIu64 "\n",  dpr->frames_victim);

    struct dp_frame *dpf = dpr->frames[dpr->frames_victim];

    err = frame_evict(dpr, dpf);
    if (err_is_fail(err)) {
        return err;
    }

    /* set the next victim */
    dpr->frames_victim = (dpr->frames_victim + 1) % dpr->frames_count;

    *ret_dpf = dpf;

    return SYS_ERR_OK;
}

/*
 * ===========================================================================
 * Page-fault handler
 * ===========================================================================
 */

static errval_t handle_pagefault(lvaddr_t vaddr)
{
    /* find demand paging regions */
    errval_t err;

    DP_DEBUG_HANDLER("pagefault at vaddr = %" PRIx64 "\n", vaddr);

    struct demand_paging_region *dpr = demand_paging_regions;

    lvaddr_t base;

    while (dpr) {
        base = vregion_get_base_addr(&dpr->vreg);
        if (base <= vaddr && vaddr < (base + vregion_get_size(&dpr->vreg))) {
            /* found */
            break;
        }
        dpr = dpr->next;
    }

    if (dpr == NULL) {
        DP_DEBUG_HANDLER("dpr not found\n");
        return -1;
    }

    if (dpr->frames_count == 0) {
        USER_PANIC("there are o frames in the region\n");
    }

    struct dp_frame *dpf;
    if (dpr->frames_free) {
        dpf = dpr->frames_free;
        dpr->frames_free = dpf->next;
    } else {
        err = frame_evict_any(dpr, &dpf);
        if (err_is_fail(err)) {
            return err;
        }
    }

    /* find page */
    vaddr = vaddr & ~(dpr->pagesize - 1);

    DP_DEBUG_HANDLER("handling fault on page 0x%" PRIx64 " in dpr='%s' with "
                     "frame 0x%" PRIx64"\n", vaddr, dpr->swapname, dpf->paddr);

    size_t slot = (vaddr - base) / dpr->pagesize;
    struct dp_page *dpp = &dpr->pages[slot];
    assert(dpp->vaddr == vaddr);

    /* install the mapping */
    err = frame_map(dpr, dpp, dpf);
    if (err_is_fail(err)) {
        return err;
    }

    /* check if we need to swap in the page, otherwise clean */
    if (dpp->state == DEMAND_PAGING_PST_FILE) {
        swap_in(dpp);
    } else {
        memset((void *)vaddr, 0, dpr->pagesize);
    }

    return SYS_ERR_OK;
}

static void exn_handler(enum exception_type type, int subtype,
                        void *addr, arch_registers_state_t *regs,
                        arch_registers_fpu_state_t *fpuregs)
{
    errval_t err;
    if (type == EXCEPT_PAGEFAULT) {
        err = handle_pagefault((lvaddr_t)addr);
        if (err_is_fail(err)) {
            // could not handle page fault, exiting for now
            // TODO: do something sensible here
            exit(1);
        }
    } else {
        DP_DEBUG_HANDLER("unknown exception\n");
    }
    return;
}

static errval_t vspace_reserve_region(struct vregion *vregion,
                                      size_t bytes, size_t pagesize,
                                      vregion_flags_t flags)
{
    errval_t err;

    struct vspace *vspace = get_current_vspace();
    struct pmap *pmap = vspace_get_pmap(vspace);

    struct memobj memobj;
    memobj.size = bytes;

    genvaddr_t address;
    err = pmap->f.determine_addr(pmap, &memobj, pagesize, &address);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_DETERMINE_ADDR);
    }

    vregion->vspace = vspace;
    vregion->memobj = NULL;
    vregion->base   = address;
    vregion->offset = 0;
    vregion->size   = bytes;
    vregion->flags  = flags;

    err = vspace_add_vregion(vspace, vregion);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_ADD_REGION);
    }

    err = pmap->f.create_pts_pinned(pmap, address, bytes, flags);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_MAP);
    }

    return err;
}

static errval_t vspace_get_vnode(struct vregion *vregion, lvaddr_t vaddr,
                                 lvaddr_t *ret_vaddr)
{
    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap = vspace_get_pmap(vspace);
    assert(pmap->f.get_leaf_pt);
    return pmap->f.get_leaf_pt(pmap, vaddr, ret_vaddr);
}


static errval_t create_swap_file(char *path, size_t bytes, vfs_handle_t *ret_handle)
{
    errval_t err;

    /* open the paging file */
    err = vfs_create(path, ret_handle);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not create the vfs handle");
        return err;
    }

    err = vfs_truncate(*ret_handle, bytes);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not truncate swapfile");
        return err;
    }

    return SYS_ERR_OK;
}

/*
 * ===========================================================================
 * Public interface
 * ===========================================================================
 */


errval_t demand_paging_init(void *ex_stack, size_t stack_size)
{
    errval_t err;

    DP_DEBUG_MGMT("[init] preparing exception handler");

    if (ex_stack != NULL && stack_size < EXCEPTION_STACK_MIN_SIZE) {
        return -1;
    }

    if (ex_stack == NULL) {
        if (stack_size < EXCEPTION_STACK_MIN_SIZE) {
            stack_size = EXCEPTION_STACK_SIZE;
        }
        ex_stack = calloc(stack_size, sizeof(char));
    }

    void *ex_stack_top = ex_stack + stack_size;

    DP_DEBUG_MGMT("[init] stack top=%p, stackbase=%p\n", exn_handler,
                 ex_stack_top, ex_stack);

    err = thread_set_exception_handler(exn_handler, NULL, ex_stack, ex_stack_top,
                                       NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to set the exceptin handler");
        return err;
    }

    vfs_init();

    err = vfs_mkdir(DEMAND_PAGING_SWAP_FILE);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "err");
        /* can actually fail */
    }

    return SYS_ERR_OK;
}

errval_t demand_paging_region_create(size_t bytes, size_t pagesize, size_t numframes,
                                     struct demand_paging_region **ret_dpr)
{
    errval_t err;

    DP_DEBUG_MGMT("[create] dpr of size %" PRIu64 "\n", bytes);

    /* determine basic information about the page sizes */
    vregion_flags_t flags = VREGION_FLAGS_READ_WRITE;
    uint8_t pagebits;
    switch (pagesize) {
        case BASE_PAGE_SIZE:
            pagebits = BASE_PAGE_BITS;
            break;
        case LARGE_PAGE_SIZE:
            flags |= VREGION_FLAGS_LARGE;
            pagebits = LARGE_PAGE_BITS;
            break;
        case HUGE_PAGE_SIZE:
            pagebits = HUGE_PAGE_BITS;
            flags |= VREGION_FLAGS_HUGE;
            break;
        default:
            return -1;
            break;
    }

    /* round up bytes and calcualte number of slots */
    bytes = ROUND_UP(bytes, pagesize);
    size_t slots = bytes / pagesize;
    size_t vnode_leaves_count = ((bytes / pagesize) + 511) / 512;

    /* allocate the data structure */
    struct demand_paging_region *dpr = calloc(1, sizeof(*dpr) +
                                              slots * sizeof(struct dp_page) +
                                              (vnode_leaves_count) * sizeof(void *));
    if (dpr == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    /* initialize fields */
    dpr->pagesize = pagesize;
    dpr->pages = (struct dp_page *)(dpr + 1);

    err = vspace_reserve_region(&dpr->vreg, bytes, pagesize, flags);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "reserve region in vspace for demand paging\n");
        return err;
    }

    snprintf(dpr->swapname, DEMAND_PAGING_SWAP_FILE_PATHLEN, "%s/0x%016lx",
             DEMAND_PAGING_SWAP_FILE,
             vspace_genvaddr_to_lvaddr(vregion_get_base_addr(&dpr->vreg)));

    err = create_swap_file(dpr->swapname, bytes, &dpr->swapfile);
    if (err_is_fail(err)) {
        return err;
    }

    /* initialize pages */
    genvaddr_t addr = vspace_genvaddr_to_lvaddr(vregion_get_base_addr(&dpr->vreg));
    lvaddr_t vnode_addr;


    err = vspace_get_vnode(&dpr->vreg, addr, &vnode_addr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "foobar");
    }

    dpr->vnodes = (void **)(dpr->pages + slots);
    dpr->vnodes[0] = (void *)vnode_addr;
    int j = 0;
    for (size_t i = 0; i < slots; ++i) {
        err = vspace_get_vnode(&dpr->vreg, addr, &vnode_addr);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "foobar");
        }

        if (dpr->vnodes[j] != (void *)vnode_addr) {
            dpr->vnodes[++j] = (void *)vnode_addr;
        }
        dpr->pages[i].pagenr = i;
        dpr->pages[i].dpr = dpr;
        dpr->pages[i].vaddr = addr;
        dpr->pages[i].vnode = (void *)vnode_addr;
        dpr->pages[i].vnode_entry = (void *)vnode_addr;
        if (pagesize == HUGE_PAGE_SIZE) {
            dpr->pages[i].vnode_entry += X86_64_PDPT_BASE(addr);
        } else if (pagesize == LARGE_PAGE_SIZE) {
            dpr->pages[i].vnode_entry += X86_64_PDIR_BASE(addr);
        } else {
            dpr->pages[i].vnode_entry += X86_64_PTABLE_BASE(addr);
        }
        addr += pagesize;

    }

    /* allocate the frames */
    struct capref frame;
    size_t allocated_size;
    err = frame_alloc(&frame, numframes * pagesize, &allocated_size);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "frame alloc\n");
    }

    struct frame_identity id;
    err = frame_identify(frame, &id);
    assert(err_is_ok(err));

    struct capref cnode_cap;
    struct capref frames;
    err = cnode_create(&cnode_cap, &frames.cnode, allocated_size / pagesize, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cnode create\n");
    }

    debug_printf("FRAME BASE: %lx\n", id.base);

    err = cap_retype(frames, frame, ObjType_Frame, pagebits);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "cap retype\n");
    }

    dpr->frames = calloc(numframes, sizeof(void *));
    if (dpr->frames == NULL) {
        USER_PANIC("alloc frame counter\n");
    }

    /* initialize the frames */
    struct dp_frame *dpf = calloc(numframes, sizeof(*dpf));
    if (dpf == NULL) {
        USER_PANIC("alloc frame counter\n");
    }
    dpf->first = 1;
    for (size_t i = 0; i < numframes; ++i) {
        dpf->frame = frames;
        dpf->page = NULL;
        if (i == (numframes - 1)) {
            dpf->next = NULL;
        } else {
            dpf->next = (dpf+1);
        }

        dpr->frames[i] = dpf;

        dpf++;
        frames.slot++;
    }

    dpr->frames_free = dpr->frames[0];
    dpr->frames_count = numframes;

    dpr->next = demand_paging_regions;
    demand_paging_regions = dpr;

    if (ret_dpr) {
        *ret_dpr = dpr;
    }

    debug_printf("region created\n");


    return SYS_ERR_OK;
}

errval_t demand_paging_region_add_frames(struct capref *frames, size_t count,
                                         struct demand_paging_region *dpr)
{
    if (count == 0) {
        return SYS_ERR_OK;
    }

    assert(dpr);

    /* initialize the frames */
    struct dp_frame *dpf = calloc(count, sizeof(*dpf));
    if (dpf == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    struct dp_frame **dp_frames = realloc(dpr->frames,
                                          (dpr->frames_count + count) * sizeof(void *));
    if (dp_frames == NULL) {
        free(dpf);
        return LIB_ERR_MALLOC_FAIL;
    }

    dpf->first = 1;
    for (size_t i = 0; i < count; ++i) {
        dpf->frame = frames[i];
        dpf->page = NULL;
        if (i == (count - 1)) {
            dpf->next = NULL;
        } else {
            dpf->next = (dpf+1);
        }

        dpr->frames[dpr->frames_count + i] = dpf;

        dpf++;
    }

    /* add it to the free list */
    if (dpr->frames_free) {
        dpr->frames[dpr->frames_count + count - 1]->next = dpr->frames_free;
    }
    dpr->frames_free = dpr->frames[dpr->frames_count];

    /* update count */
    dpr->frames_count += count;
    dpr->frames = dp_frames;

    return SYS_ERR_OK;
}

errval_t demand_paging_region_remove_frames(size_t count, struct demand_paging_region *dpr,
                                             struct capref *ret_frames, size_t *ret_count)
{
    errval_t err;

    if (count == 0) {
        goto out;
    }

    if (count > (dpr->frames_count - 1)) {
        count = (dpr->frames_count - 1);
    }

    struct dp_frame *dpf = dpr->frames[dpr->frames_count - 1];
    for (size_t i = 0; i < count; ++i) {
        err = frame_evict(dpr, dpf);
        if (err_is_fail(err)) {
            count = i;
            break;
        }
        ret_frames[i] = dpf->frame;
        memset(dpf, 0, sizeof(*dpf));
        if (dpf->first) {
            free(dpf);
        }
        dpf--;
    }

    dpr->frames_count -= count;
    dpr->frames = realloc(dpr->frames, dpr->frames_count * sizeof(void *));
    assert(dpr->frames);

    out:
    if (ret_count) {
        *ret_count = count;
    }
    return SYS_ERR_OK;
}

errval_t demand_paging_region_destory(struct demand_paging_region *dpr)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}

void *demand_paging_get_base_address(struct demand_paging_region *dpr)
{
    return (void *)vspace_genvaddr_to_lvaddr(vregion_get_base_addr(&dpr->vreg));
}
