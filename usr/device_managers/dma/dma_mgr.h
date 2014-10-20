/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DMA_MGR_H_
#define DMA_MGR_H_

struct dma_mgr_driver_info;

errval_t driver_store_init(void);

errval_t driver_store_insert(lpaddr_t mem_low,
                             lpaddr_t mem_high,
                             uint8_t numa_node,
                             uint8_t type,
                             iref_t iref);


errval_t driver_store_lookup(lpaddr_t mem_low,
                             size_t size,
                             uint8_t numa_node,
                             struct dma_mgr_driver_info **info);

errval_t driver_store_lookup_by_iref(iref_t iref,
                                     struct dma_mgr_driver_info **info);

/**
 * \brief initializes the DMA manager service service
 *
 * \param svc_name  The name of the service for nameservice registration
 * \param cb        Callback function pointers
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_mgr_svc_start(void);

#endif  /* DMA_MGR_H_ */
