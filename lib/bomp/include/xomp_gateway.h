/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __XOMP_GATEWAY_H
#define	__XOMP_GATEWAY_H

/**
 * \brief initializes the xomp gatway service
 *
 * \return SYS_ERR_OK on success
 *         errval on failure
 */
errval_t xomp_gateway_init(void);

/**
 * \brief registers a new memory region that it can be requested lateron
 *
 * \param addr  the virtual address of the region
 * \param frame frame capability backing the address
 *
 * \return SYS_ERR_OK on success
 *         LIB_ERR_MALLOC_FAIL on failure
 */
errval_t xomp_gateway_mem_insert(struct capref frame,
                                 lpaddr_t addr);

/**
 * \brief generates a memory token based on the domain
 *
 * \return 64bit token
 */
uint64_t xomp_gateway_make_token(void);


#endif	/* __XOMP_GATEWAY_H */
