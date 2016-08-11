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

    tmp->region_id = region_id;
    tmp->cap = cap;

    err = invoke_frame_identify(*cap, &id);
    if (err_is_fail(err)) {
        return err;
    }

    tmp->base_addr = id.base;
    tmp->len = id.bytes;

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
    free(region);
    return SYS_ERR_OK;
}
/**
 * @brief Get a buffer from a region
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
    *buffer_id = 0;
    DQI_DEBUG("Got buffer id=%d addr=%16lx \n", *buffer_id, *addr);
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
    DQI_DEBUG("Returned buffer id=%d \n", buffer_id);
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
    DQI_DEBUG("Returned buffer id=%d \n", buffer_id);
    return true;
}

