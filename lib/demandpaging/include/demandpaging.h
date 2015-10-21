/*
 * \brief demandpaging.h
 *
 * Copyright (c) 2015 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef LIB_DEMANDPAGING_INCLUDE_DEMANDPAGING_H_
#define LIB_DEMANDPAGING_INCLUDE_DEMANDPAGING_H_

struct demand_paging_region;

typedef size_t (demand_paging_evict_fn_t)(void);

/**
 * @brief initializes the demand paging library
 *
 * @return
 */
errval_t demand_paging_init(void *ex_stack, size_t stack_size);


errval_t demand_paging_region_create(size_t bytes, size_t pagesize, size_t numframes,
                                     struct demand_paging_region **ret_dpr);

errval_t demand_paging_region_add_frames(struct capref *frames, size_t count,
                                         struct demand_paging_region *dpr);

errval_t demand_paging_region_remove_frames(struct capref **frames, size_t count,
                                            size_t *ret_count,
                                            struct demand_paging_region *dpr);

errval_t demand_paging_region_destory(struct demand_paging_region *dpr);


void *demand_paging_get_base_address(struct demand_paging_region *dpr);

#endif /* LIB_DEMANDPAGING_INCLUDE_DEMANDPAGING_H_ */
