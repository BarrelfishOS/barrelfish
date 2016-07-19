/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "region.h"
#include "dqi_debug.h"

/**
 * @brief initialized a region
 *
 * @param region                Return pointer to the region
 * @param region_id             The ID of the region,
 * @param cap                   Capability of the memory region
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
/*
errval_t region_init_variable_sized(struct region** region,
                     uint32_t region_id,
                     struct capref* cap)
{
    USER_PANIC("NIY for variable sized buffers\n");
    return SYS_ERR_OK;
}
*/
/**
 * @brief initialized a region from which only fixed size buffers are used
 *
 * @param region                Return pointer to the region
 * @param region_id             The ID of the region,
 * @param cap                   Capability of the memory region
 * @param len                   Length of the fixed sized buffers
 *
 * @returns error on failure or SYS_ERR_OK on success
 */

errval_t region_init(struct region** region,
                     uint32_t region_id,
                     struct capref* cap,
                     size_t len)
{
    errval_t err;
    struct frame_identity id;

    struct region* tmp = malloc(sizeof(struct region));
    if (tmp == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    tmp->region_id = region_id;
    tmp->cap = cap;

    err = invoke_frame_identify(*cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    tmp->base_addr = id.base;
    tmp->len = id.bytes;

    // Init state for fixed buffer region
    tmp->fixed_size = true;
    tmp->next_buf = 0;  
    tmp->num_buf = id.bytes/len;
    tmp->in_use = calloc(sizeof(bool)*tmp->num_buf, 1);
    if (region == NULL) {
        free(tmp);
        return LIB_ERR_MALLOC_FAIL;
    }

    *region = tmp;   
    
    DQI_DEBUG("Initialize Region size=%ld addr=%16lx num_bufs=%ld \n",
              tmp->len, tmp->base_addr, tmp->num_buf);

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
    for (int i = 0; i < region->num_buf; i++) {
        if (region->in_use[i]) {
            DQI_DEBUG("Could not destroy region, some buffers stil in use \n");
            // TODO reasonable error;
            return -1;      
        }
    }

    free(region->in_use);
    free(region);
    return SYS_ERR_OK;
}
/**
 * @brief Get a buffer from a region
 *
 * @param region                The region to get the buffer from
 * @param len                   lenght of the buffer
 * @param base_addr             Return pointer to the physical address 
 *                              of the buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_get_buffer(struct region* region,
                           uint32_t* buffer_id,
                           lpaddr_t* addr)
{
    if (region->fixed_size) {
        uint32_t count = 0;
        // Try to find empty slot, If we did not find 
        // a slot after making a whole round, return error
        while (count < region->num_buf+1) {
            if (!region->in_use[region->next_buf]) {
                *buffer_id = region->next_buf;
                *addr = (region->base_addr + (region->next_buf* 
                         region->buf_len));
                region->in_use[region->next_buf] = true;
                region->next_buf = (region->next_buf + 1) % region->num_buf;

                DQI_DEBUG("Got buffer id=%d addr=%16lx \n", *buffer_id, *addr);
                return SYS_ERR_OK;
            } else {
                region->next_buf = (region->next_buf + 1) % region->num_buf;
                count++;
            }
        }   

    } else {
        USER_PANIC("NIY for variable sized buffers\n");
    }

    DQI_DEBUG("Failed to get buffer \n");
    // TODO reasonable error
    return -1;
}


/**
 * @brief Return a buffer to the region
 *
 * @param region                The region to return the buffer to
 * @param len                   Lenght of the buffer returned 
 * @param buffer_id             The id of the buffer to return to the region
 * @param addr                  The physical address of the freed buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_free_buffer(struct region* region,
                            uint32_t buffer_id)
{
    if (region->fixed_size) {
        // Can not free buffer that is not used
        if (region->in_use[buffer_id] != true) {
            // TODO reasonable error
            return -1;
        }
        region->in_use[buffer_id] = false;
    } else {
        USER_PANIC("NIY for variable sized buffers\n");
    }

    DQI_DEBUG("Returned buffer id=%d \n", buffer_id);
    return SYS_ERR_OK;
}

