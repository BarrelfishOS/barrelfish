/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DMA_REQUEST_H
#define IOAT_DMA_REQUEST_H

#include <dev/ioat_dma_dev.h>

struct ioat_dma_request;

/// callback to be called when the request is finished
typedef void (*ioat_dma_cb_t)(void);

typedef uint64_t ioat_dma_req_id_t;

enum ioat_dma_req_type {
    IOAT_DMA_REQ_TYPE_INVALID,
    IOAT_DMA_REQ_TYPE_MEMCPY
};

enum ioat_dma_req_state {
    IOAT_DMA_REQ_ST_INVALID,
    IOAT_DMA_REQ_ST_PREPARED,
    IOAT_DMA_REQ_ST_SUBMITTED,
    IOAT_DMA_REQ_ST_DONE,
    IOAT_DMA_REQ_ST_ERR
};



struct ioat_dma_req_setup
{
    enum ioat_dma_req_type type;
    lpaddr_t src;
    lpaddr_t dst;
    size_t   bytes;
    ioat_dma_cb_t done_cb;
    void *arg;
};

struct ioat_dma_request
{
    ioat_dma_req_id_t id;
    struct ioat_dma_descriptor *desc_head;
    struct ioat_dma_descriptor *desc_tail;
    struct ioat_dma_request *next;
    ioat_dma_cb_t done_cb;
    void *arg;
};

errval_t ioat_dma_request_cleanup(struct ioat_dma_request *req);

bool ioat_dma_request_done(struct ioat_dma_request *req);


errval_t ioat_dma_request_memcpy(struct ioat_dma_device *dev,
                                 struct ioat_dma_req_setup *setup);


#endif /* IOAT_DMA_REQUEST_H */
