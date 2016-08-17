/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <devif/queue_interface.h>
#include "region.h"
#include "dqi_debug.h"

/*
 * A region keeps track of the buffers that are currently allocated 
 * i.e. If two buffer is marked as allocated and it is reused before it
 * is freed, the region will return an allocation error
 *
 * The datastructure to keep track of the allocation has a bucket for each
 * 4K page. If a buffer starts in a certain page, the buffer will be added to 
 * the bucket of this page. Within a bucket there is a linked list. To allocated
 * the buffer structures a slab allocator is used.
 */


/**
 * @brief initialized a region from which only fixed size buffers are used
 *
 * @param region                Return pointer to the region
 * @param region_id             The ID of the region,
 * @param cap                   Capability of the memory region
 *
 * @returns error on failure or SYS_ERR_OK on success
 */

errval_t region_init(struct region** region,
                     regionid_t region_id,
                     struct capref* cap)
{
    errval_t err;
    struct frame_identity id;

    struct region* tmp = malloc(sizeof(struct region));
    if (tmp == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    tmp->id = region_id;
    tmp->cap = cap;

    err = invoke_frame_identify(*cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    tmp->base_addr = id.base;
    tmp->len = id.bytes;
    tmp->max_page_id = tmp->len/BASE_PAGE_SIZE;

    // Datastructures for keeping track of buffers
    slab_init(&tmp->alloc, sizeof(struct buffer), slab_default_refill);
    slab_grow(&tmp->alloc, tmp->bufs, sizeof(tmp->bufs));
    tmp->used_bufs = malloc(sizeof(struct buffer*)*tmp->len/BASE_PAGE_SIZE);

    *region = tmp;   
    
    DQI_DEBUG_REGION("Initialize Region size=%ld addr=%16lx\n",
              tmp->len, tmp->base_addr);

    return SYS_ERR_OK;
}


/**
 * @brief free up a region
 *
 * @param region                The region to free up
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_destroy(struct region* region)
{
    for (int i = 0; i < region->max_page_id; i++) {
        if (region->used_bufs[i] != NULL) {
            return DEVQ_ERR_REGION_DESTROY;
        }
    }

    free(region->used_bufs);
    free(region);
    return SYS_ERR_OK;
}
/**
 * @brief Get a buffer id from a region
 *
 * @param region                The region to get the buffer from
 * @param addr                  The physical address of the buffer
 * @param len                   lenght of the buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_get_buffer_id(struct region* region,
                              lpaddr_t addr,
                              bufferid_t* buffer_id)
{
    uint32_t page_id;
    *buffer_id = (addr - region->base_addr);
    page_id = (*buffer_id)/BASE_PAGE_SIZE;

    // Test if buffer can not be in region
    if (page_id > region->max_page_id) {
        return DEVQ_ERR_BUFFER_NOT_IN_REGION;
    }

    // Test if buffer is already used
    if (region_buffer_id_in_use(region, *buffer_id)) {
        return DEVQ_ERR_BUFFER_ALREADY_IN_USE;
    }

    struct buffer* tmp = slab_alloc(&region->alloc);
    tmp->id = *buffer_id;
    tmp->next = NULL;
    struct buffer* ele = region->used_bufs[page_id];
    
    // Empty list
    if (ele == NULL) {
        region->used_bufs[page_id] = tmp;

        DQI_DEBUG_REGION("buffer region=%d bucket=%d, id=%d, addr=%16lx\n", 
                  region->id, page_id, *buffer_id, addr);
        return SYS_ERR_OK;
    }    

    // Iterate through list
    while (ele->next != NULL) {
        ele = ele->next;
    }

    ele->next = tmp;
    
    DQI_DEBUG_REGION("buffer region=%d bucket=%d, id=%d, addr=%16lx\n", 
               region->id, page_id, *buffer_id, addr);
    return SYS_ERR_OK;
}


/**
 * @brief Return a buffer to the region
 *
 * @param region                The region to return the buffer to
 * @param buffer_id             The id of the buffer to return to the region
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_free_buffer_id(struct region* region,
                               bufferid_t buffer_id)
{
    uint32_t page_id;
    page_id = buffer_id/BASE_PAGE_SIZE;
    // Test if buffer can not be in region
    if (page_id > region->max_page_id) {
        return DEVQ_ERR_BUFFER_NOT_IN_REGION;
    }

    // Test if buffer is used
    if (!region_buffer_id_in_use(region, buffer_id)) {
        return DEVQ_ERR_BUFFER_NOT_IN_USE;
    }
    
    struct buffer* ele = region->used_bufs[page_id];
    // First entry is special case
    if (ele->id == buffer_id) {
        region->used_bufs[page_id] = ele->next;
        slab_free(&region->alloc, ele);
        DQI_DEBUG_REGION("Returned buffer id=%d (first entry)\n", buffer_id);
        return SYS_ERR_OK;
    }
    
    while (ele->next != NULL) {
        if (ele->next->id == buffer_id) {
            ele->next = ele->next->next;
            slab_free(&region->alloc, ele);
            DQI_DEBUG_REGION("Returned buffer id=%d (second or higher entry) \n", 
                      buffer_id);
            return SYS_ERR_OK;
        }
        ele = ele->next;
    }
    
    return SYS_ERR_OK;
}


/**
 * @brief Return a if a buffer to the region is in use
 *
 * @param region                The region to return the buffer to
 * @param buffer_id             The id of the buffer
 *
 * @returns true if the buffer is in use otherwise false
 */
bool region_buffer_id_in_use(struct region* region,
                             bufferid_t buffer_id)
{
    uint32_t page_id;
    page_id = buffer_id/BASE_PAGE_SIZE;

    // Empty bucket -> can not be used
    if (region->used_bufs[page_id] == NULL) {
        return false;
    }

    // check list
    struct buffer* ele = region->used_bufs[page_id];
    while (ele != NULL) {
        if (ele->id == buffer_id) {
            return true;
        } 
        ele = ele->next;
    }

    DQI_DEBUG_REGION("Returned buffer id=%d \n", buffer_id);
    return false;
}

