/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef XEON_PHI_DMA_H
#define XEON_PHI_DMA_H

//#include <dev/xeon_phi/xeon_phi_dma_dev.h>


struct dma_info {

};

struct dma_channel {


};

errval_t dma_request_alloc(void);

errval_t dma_request_exec(void);

errval_t dma_request_free(void);


/**
 * \brief Initializes the DMA structure for the Xeon Phi
 *
 * \param phi the xeon phi DMA structure
 *
 * \return SYS_ERR_OK on success,
 */
errval_t dma_init(struct xeon_phi *phi);

#endif /* XEON_PHI_DMA_H */
