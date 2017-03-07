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
/*
static void print_list(struct buffer* b, uint32_t page_id, bool insert) {

    printf("########################### \n");
    struct buffer* buf = b;
    for (int i =0; i < 9; i++) {
        if (buf) {
            if (insert) {
                printf("ins page_id %d, bid %d, baddr %p, baddr->next %p \n", page_id, 
                       buf->id, buf, buf->next);
            } else {
                printf("del page_id %d, bid %d, baddr %p, baddr->next %p \n", page_id, 
                       buf->id, buf, buf->next);
            }
            buf = buf->next;
        } else {
            break;
        }
    }
    printf("############################## \n");
}
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

    struct region* tmp = calloc(1, sizeof(struct region));
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
    free(region);
    return SYS_ERR_OK;
}

