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
#include "numa_internal.h"


/** \brief   returns the current interleave mask
 *
 * \returns bitmask representing the current interleave state
 *
 * returns the current interleave mask if the task's memory allocation policy is
 * page interleaved. Otherwise, this function returns an empty mask.
 */
struct numa_bm *numa_get_interleave_mask(void)
{
    assert(!"NYI");
    return 0;
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
void numa_set_interleave_mask(struct numa_bm *nodemask)
{
    assert(!"NYI");
}


/**
 * \brief binds the current task and its children to the nodes specified in nodemask.
 *
 * \param nodemask  bitmap representing the nodes
 */
void numa_bind(struct numa_bm *nodemask)
{
    assert(!"NYI");
}


/**
 * \brief sets the memory allocation policy for the calling task to local allocation.
 */
void numa_set_localalloc(void)
{
    assert(!"NYI");
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
errval_t numa_set_membind(struct numa_bm *nodemask)
{
    assert(!"NYI");
    return 0;
}


/**
 * \brief returns the mask of nodes from which memory can currently be allocated.
 *
 * \return bitmap of nodes from which can be allocated
 */
struct numa_bm *numa_get_membind(void){
    assert(!"NYI");
    return 0;
}


/**
 * \brief allocates memory on a specific node.
 *
 * \param size  size of the region in bytes
 * \param node  ID of the node to allocate from
 *
 * \returns pointer to memory region
 *
 * The size argument will be rounded up to a multiple of the system page size.
 * if the specified node is externally denied to this process, this call will fail.
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_onnode(size_t size, nodeid_t node){

    //numa_check_node_id(node);

    assert(!"NYI");
    return 0;
}


/**
 * \brief allocates size bytes of memory on the local node
 *
 * \param size  size of the memory region in bytes
 *
 * \returns pointer to memory region
 *
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_local(size_t size){
    assert(!"NYI");
    return 0;
}


/**
 * \brief allocates size bytes of memory page interleaved on all nodes.
 *
 * \param size   size of the memory region in bytes
 *
 * \returns pointer to the mapped memory region
 *
 * should only be used for large areas consisting of multiple pages.
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_interleaved(size_t size)
{
    assert(!"NYI");
    return 0;
}


/**
 * \brief allocates size bytes of memory page interleaved on all nodes.
 *
 * \param size     size of the memory region in bytes
 * \param nodemask subset of nodes to consider for allocation
 * \returns pointer to the mapped memory region
 *
 * should only be used for large areas consisting of multiple pages.
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc_interleaved_subset(size_t size, struct numa_bm *nodemask)
{
    assert(!"NYI");
    return 0;
}


/**
 * \brief allocates size bytes of memory with the current NUMA policy.
 *
 * \param size  size of the memory region in bytes
 *
 * \returns pointer to the mapped memory region
 *
 * The memory must be freed with numa_free(). On errors NULL is returned.
 */
void *numa_alloc(size_t size)
{
    assert(!"NYI");
    return 0;
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
    assert(!"NYI");
    return 0;
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
                            struct numa_bm *fromnodes,
                            struct numa_bm *tonodes)
{
    assert(!"NYI");
    return 0;
}
