/**
 * \file
 * \brief General Numa functions
 *
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>

#include <numa.h>
#include <bitmap.h>
#include "numa_internal.h"

///< numa interleave mask for allocations
struct bitmap *numa_alloc_interleave_mask;

///< numa bind mask for allocations
struct bitmap *numa_alloc_bind_mask;

static void validate_page_size(size_t *pagesize, vregion_flags_t *flags)
{
#if !defined(__x86_64__)
    if (*pagesize < LARGE_PAGE_SIZE) {
        *pagesize = BASE_PAGE_SIZE;
        *flags = VREGION_FLAGS_READ_WRITE;
    } else if (*pagesize < HUGE_PAGE_SIZE) {
        *pagesize = LARGE_PAGE_SIZE;
        *flags = VREGION_FLAGS_READ_WRITE | VREGION_FLAGS_LARGE;
    } else {
        *pagesize = HUGE_PAGE_SIZE;
        *flags = VREGION_FLAGS_READ_WRITE | VREGION_FLAGS_HUGE;
    }
#else
    *pagesize = BASE_PAGE_SIZE;
    *flags = VREGION_FLAGS_READ_WRITE;
#endif


}


/** \brief   returns the current interleave mask
 *
 * \returns bitmask representing the current interleave state
 *
 * returns the current interleave mask if the task's memory allocation policy is
 * page interleaved. Otherwise, this function returns an empty mask.
 */
struct bitmap *numa_get_interleave_mask(void)
{
    assert(numa_alloc_interleave_mask);
    struct bitmap *im = numa_allocate_nodemask();
    if (im == NULL) {
        return NULL;
    }
    bitmap_copy(im, numa_alloc_interleave_mask);
    return im;
}


/**
 * \brief sets the memory interleave mask for the current task to nodemask
 *
 * \param nodemask bitmask representing the nodes
 *
 * All new memory allocations are page interleaved over all nodes in the interleave
 * mask. Interleaving can be turned off again by passing an empty mask.
 *
 * This bitmask is considered to be a hint. Fallback to other nodes may be possible
 */
void numa_set_interleave_mask(struct bitmap *nodemask)
{
    assert(numa_alloc_interleave_mask);

    if (!nodemask) {
        bitmap_clear_all(numa_alloc_interleave_mask);
        return;
    }

    if (bitmap_get_nbits(nodemask) < NUMA_MAX_NUMNODES) {
        NUMA_WARNING("supplied interleave mask (%p) has to less bits!", nodemask);
        return;
    }
    bitmap_copy(numa_alloc_interleave_mask, nodemask);

    /* clear out the invalid nodes */
    bitmap_clear_range(numa_alloc_interleave_mask, numa_num_configured_nodes(),
                       bitmap_get_nbits(numa_alloc_interleave_mask));

    /* clear the bind mask as we are using interleaving mode now */
    bitmap_clear_all(numa_alloc_bind_mask);
}


/**
 * \brief binds the current task and its children to the nodes specified in nodemask.
 *
 * \param nodemask  bitmap representing the nodes
 */
void numa_bind(struct bitmap *nodemask)
{
    USER_PANIC("Not yet implemented");
}


/**
 * \brief sets the memory allocation policy for the calling task to local allocation.
 */
void numa_set_localalloc(void)
{
    assert(numa_alloc_bind_mask);
    assert(numa_alloc_interleave_mask);

    /* clear interleave mode */
    bitmap_clear_all(numa_alloc_interleave_mask);

    bitmap_clear_all(numa_alloc_bind_mask);
    bitmap_set_bit(numa_alloc_bind_mask, numa_current_node());
}

/**
 * \brief sets the memory allocation mask.
 *
 * \param nodemask  bitmap representing the nodes
 *
 * The task will only allocate memory from the nodes set in nodemask.
 *
 * an empty mask or not allowed nodes in the mask will result in an error
 */
errval_t numa_set_membind(struct bitmap *nodemask)
{
    assert(numa_alloc_bind_mask);
    assert(numa_alloc_interleave_mask);

    if (!nodemask) {
        return NUMA_ERR_BITMAP_PARSE;
    }

    if (bitmap_get_nbits(nodemask) < NUMA_MAX_NUMNODES) {
        NUMA_WARNING("supplied interleave mask (%p) has to less bits!", nodemask);
        return NUMA_ERR_BITMAP_RANGE;
    }

    /* copy new membind mask and clear out invalid bits */
    bitmap_copy(numa_alloc_bind_mask, nodemask);
    bitmap_clear_range(numa_alloc_bind_mask, numa_num_configured_nodes(),
                       bitmap_get_nbits(numa_alloc_bind_mask));

    if (bitmap_get_weight(numa_alloc_bind_mask) == 0) {
        /* cannot bind to no node, restore with all nodes pointer*/
        bitmap_copy(numa_alloc_bind_mask, numa_all_nodes_ptr);
        return NUMA_ERR_NUMA_MEMBIND;
    }

    /* disable interleaving mode */
    bitmap_clear_all(numa_alloc_interleave_mask);

    return SYS_ERR_OK;
}


/**
 * \brief returns the mask of nodes from which memory can currently be allocated.
 *
 * \return bitmap of nodes from which can be allocated
 */
struct bitmap *numa_get_membind(void)
{
    assert(numa_alloc_bind_mask);
    struct bitmap *im = numa_allocate_nodemask();
    if (im == NULL) {
        return NULL;
    }
    bitmap_copy(im, numa_alloc_bind_mask);
    return im;
}


/**
 * \brief allocates memory on a specific node.
 *
 * \param size      size of the region in bytes
 * \param node      ID of the node to allocate from
 * \param pagesize  page size to be used for the mapping
 *
 * \returns pointer to memory region
 *
 * The size argument will be rounded up to a multiple of the system page size.
 * if the specified node is externally denied to this process, this call will fail.
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_onnode(size_t size, nodeid_t node, size_t pagesize)
{
    errval_t err;

    /*
     * TODO: keep track of the allocated numa frames
     */

    NUMA_DEBUG_ALLOC("allocate on node %" PRIuNODEID "\n", node);

    /* validate page size and round up size */
    vregion_flags_t flags;
    validate_page_size(&pagesize, &flags);
    size = (size + pagesize - 1) & ~(pagesize - 1);

    /* allocate frame */
    struct capref frame;
    size_t ret_size;
    err = numa_frame_alloc_on_node(&frame, size, node, &ret_size);
    if (err_is_fail(err)) {
        return NULL;
    }

    NUMA_DEBUG_ALLOC("mapping allocated frame\n");

    void *addr;
    err = vspace_map_one_frame_attr_aligned(&addr, size, frame, flags,
                                            pagesize, NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace_map_one_frame_attr_aligned");
        err = numa_frame_free(frame);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "nested error while freeing frame");
        }
        return NULL;
    }

    NUMA_DEBUG_ALLOC("frame mapped @ %p\n", addr);

    return addr;
}


/**
 * \brief allocates size bytes of memory on the local node
 *
 * \param size  size of the memory region in bytes
 * \param pagesize  page size to be used for the mapping
 *
 * \returns pointer to memory region
 *
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_local(size_t size, size_t pagesize)
{
    nodeid_t node = numa_current_node();

    NUMA_DEBUG_ALLOC("allocate on local node %" PRIuNODEID "\n", node);

    return numa_alloc_onnode(size, node, pagesize);
}


/**
 * \brief allocates size bytes of memory page interleaved on all nodes.
 *
 * \param size      size of the memory region in bytes
 * \param pagesize  preferred page size to be used
 *
 * \returns pointer to the mapped memory region
 *
 * should only be used for large areas consisting of multiple pages.
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_interleaved(size_t size, size_t pagesize)
{
    return numa_alloc_interleaved_subset(size, pagesize, numa_all_nodes_ptr);
}


/**
 * \brief allocates size bytes of memory page interleaved the nodes specified in
 *        the nodemask.
 *
 * \param size     size of the memory region in bytes
 * \param nodemask subset of nodes to consider for allocation
 * \param pagesize  preferred page size to be used
 *
 * \returns pointer to the mapped memory region
 *
 * should only be used for large areas consisting of multiple pages.
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_interleaved_subset(size_t size, size_t pagesize,
                                    struct bitmap *nodemask)
{
    errval_t err;

    /* clear out invalid bits */
    bitmap_clear_range(nodemask, numa_num_configured_nodes(),
                       bitmap_get_nbits(nodemask));

    /* get the number of nodes */
    nodeid_t nodes = bitmap_get_weight(nodemask);
    if (nodes == 0) {
        return NULL;
    }

    NUMA_DEBUG_ALLOC("allocating interleaved using %" PRIuNODEID " nodes\n", nodes);

    assert(nodes <= numa_num_configured_nodes());

    vregion_flags_t flags;
    validate_page_size(&pagesize, &flags);
    size_t stride = pagesize;

    size_t node_size = size / nodes;
    node_size = (node_size + pagesize - 1) & ~(pagesize - 1);

    /* update total size as this may change due to rounding of node sizes*/
    size = nodes * node_size;

    /*
     * XXX: we may want to keep track of numa alloced frames
     */

    struct memobj_numa *memobj = calloc(1, sizeof(struct memobj_numa));
    err = memobj_create_numa(memobj, size, 0, numa_num_configured_nodes(), stride);
    if (err_is_fail(err)) {
        return NULL;
    }

    bitmap_bit_t node = bitmap_get_first(nodemask);
    nodeid_t node_idx=0;
    while(node != BITMAP_BIT_NONE) {
        struct capref frame;
        err = numa_frame_alloc_on_node(&frame, node_size, (nodeid_t)node, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "numa_frame_alloc_on_node");
            goto out_err;
        }
        memobj->m.f.fill(&memobj->m, node_idx, frame, 0);
        ++node_idx;
        node = bitmap_get_next(nodemask, node);
    }

    struct vregion *vreg = calloc(1, sizeof(struct vregion));
    if (vreg == NULL) {
        goto out_err;
    }
    err = vregion_map_aligned(vreg, get_current_vspace(), &memobj->m, 0, size,
                        flags, pagesize);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vregion_map_aligned");
        goto out_err;
    }

    err = memobj->m.f.pagefault(&memobj->m, vreg, 0, 0);
    if (err_is_fail(err)) {
        vregion_destroy(vreg);
        free(vreg);
        DEBUG_ERR(err, "memobj.m.f.pagefault");
        goto out_err;
    }

    return (void *)vregion_get_base_addr(vreg);

    out_err:
    for (int i = 0; i < node_idx; ++i) {
        struct capref frame;
        memobj->m.f.unfill(&memobj->m, node_idx, &frame, NULL);
        cap_delete(frame);
    }
    return NULL;

}


/**
 * \brief allocates size bytes of memory with the current NUMA policy.
 *
 * \param size      size of the memory region in bytes
 * \param pagesize  preferred page size to be used
 * \returns pointer to the mapped memory region
 *
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc(size_t size, size_t pagesize)
{
    NUMA_DEBUG_ALLOC("allocate according to policy\n");

    /* check if we use interleaved mode */
    if (bitmap_get_weight(numa_alloc_interleave_mask)) {
        return numa_alloc_interleaved_subset(size, pagesize,
                                             numa_alloc_interleave_mask);
    }

    /* check membind */
    if (bitmap_get_weight(numa_alloc_bind_mask) == 1) {
        nodeid_t node = (nodeid_t) bitmap_get_first(numa_alloc_bind_mask);
        return numa_alloc_onnode(size, node, pagesize);
    }

    /* TODO:
     * - handle the case where multiple nodes are set in membind
     */

    /* just return some memory */
    return malloc(size);

}


/**
 * \brief changes the size of the memory area.
 *
 * \param old_addr  pointer ot the old memory region
 * \param old_size  size of the old memory region
 * \param new_size  new size to allocate
 */
void *numa_realloc(void *old_addr, size_t old_size, size_t new_size)
{
    assert(!"NYI");
    return 0;
}


/**
 * \brief frees size bytes of memory starting at start
 *
 * \param start start of the memory region
 * \param size  number of bytes to free
 *
 * the memory must be previously allocated by one of the numa_alloc* functions
 */
void numa_free(void *start, size_t size)
{
    assert(!"NYI");
}



/**
 * \brief allocates a frame on a specific node
 *
 * \param dest      capref to store the frame
 * \param size      size of the frame to allocated
 * \param node      node on which the frame should be allocated
 * \param ret_size  returned size of the frame capability
 *
 * \returns SYS_ERR_OK on SUCCESS
 *          errval on FAILURE
 */
errval_t numa_frame_alloc_on_node(struct capref *dest,
                                  size_t size,
                                  nodeid_t node,
                                  size_t *ret_size)
{
    errval_t err;

    NUMA_DEBUG_ALLOC("allocating frame on node %" PRIuNODEID "\n", node);

    uint64_t min_base, max_limit;
    ram_get_affinity(&min_base, &max_limit);

    if (node >= numa_topology.num_nodes) {
        return NUMA_ERR_NODEID_INVALID;
    }

    uint64_t node_base = numa_node_base(node);
    uint64_t node_limit = node_base + numa_node_size(node, NULL);

    NUMA_DEBUG_ALLOC("setting affinity to 0x%" PRIx64 "..0x%" PRIx64 "\n",
                     node_base, node_limit);

    ram_set_affinity(node_base, node_limit);

    err = frame_alloc(dest, size, ret_size);

    ram_set_affinity(min_base, max_limit);

    NUMA_DEBUG_ALLOC("restore affinity to 0x%" PRIx64 "..0x%" PRIx64 "\n",
                     min_base, max_limit);

    return err;
}


/**
 * \brief frees a previously allocated frame
 *
 * \param frame capability to free
 */
errval_t numa_frame_free(struct capref frame)
{
    assert(!"NYI");
    return 0;
}


/**
 * \brief  moves a list of pages in the address space of the current domain
 *
 * \param did    the domain ID
 * \param count  number of pages to move
 * \param pages  list of pages
 * \param nodes  list of nodes to which the pages can be moved
 * \param status returns the outcome for each page
 * \param flags  flags for moving the pages
 *
 * \returns SYS_ERR_OK on SUCCESS
 */
errval_t numa_move_pages(domainid_t did,
                         size_t count,
                         void **pages,
                         const nodeid_t *nodes,
                         errval_t *status,
                         int flags)
{
    assert(!"NYI");
    return 0;
}


/**
 * \brief migrate a domain from one set of nodes to another
 *
 * \param did        the domain ID
 * \param fromnodes  bitmap representing the current nodes
 * \param tonodes    bitmap representing the
 *
 * \returns SYS_ERR_OK on SUCCESS
 */
errval_t numa_migrate_pages(domainid_t did,
                            struct bitmap *fromnodes,
                            struct bitmap *tonodes)
{
    assert(!"NYI");
    return 0;
}
