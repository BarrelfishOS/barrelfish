/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_MGR_SERVICE_H_
#define IOAT_MGR_SERVICE_H_

#include <if/ioat_dma_mgr_defs.h>

struct ioat_dev_handle;

errval_t ioat_mgr_svc_init(void);

errval_t ioat_mgr_svc_add_device(struct capref *frame);

errval_t ioat_mgr_svc_acquire(struct ioat_dma_mgr_binding *binding,
                              struct ioat_dev_handle **handle);

errval_t ioat_mgr_svc_release(struct ioat_dev_handle *handle);

#endif /* IOAT_MGR_SERVICE_H_ */
