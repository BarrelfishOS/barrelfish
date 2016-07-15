/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef REGION_H_
#define REGION_H_ 1


#include <barrelfish/barrelfish.h>

struct region {
    // ID of the region
    uint32_t region_id;
    // Base address of the region
    lpaddr_t base_addr;
    // Capability of the region
    struct capref* cap;
    // Lenght of the memory region
    size_t len;

    // Are the buffers fixed size?
    bool fixed_size;
    // State for fixed sized buffers
    size_t num_buf;
    size_t buf_len;
    size_t next_buf;  
    bool* in_use;
};

/**
 * @brief initialized a region
 *
 * @param region                Return pointer to the region
 * @param region_id             The ID of the region,
 * @param cap                   Capability of the memory region
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_init(struct region** region,
                     uint32_t region_id,
                     struct capref* cap,
                     size_t len);
/**
 * @brief free up a region
 *
 * @param region                The region to free up
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_destroy(struct region* region);

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
/*
errval_t region_init_variable_size(struct region** region,
                                   uint32_t region_id,
                                   struct capref* cap,
                                   size_t len);
*/
/**
 * @brief Get a buffer from a region
 *
 * @param region                The region to get the buffer from
 * @param buffer_id             Return pointer to the buffer id of the buffer
 * @param addr                  Return pointer to the physical address 
 *                              of the buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_get_buffer(struct region* region,
                           uint32_t* buffer_id,
                           lpaddr_t* addr);

/**
 * @brief Return a buffer to the region
 *
 * @param region                The region to return the buffer to
 * @param buffer_id             The id of the buffer to return to the region
 * @param addr                  The physical address of the freed buffer
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t region_free_buffer(struct region* region,
                            uint32_t buffer_id);


/**
 * @brief Return a if a buffer to the region is in use
 *
 * @param region                The region to return the buffer to
 * @param buffer_id             The id of the buffer
 *
 * @returns true if the buffer is in use otherwise false
 */
inline bool region_buffer_in_use(struct region* region,
                                 uint32_t buffer_id)
{
    return region->in_use[buffer_id];
}

#endif /* REGION_H_ */
