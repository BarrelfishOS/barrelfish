/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>

#include <devif/queue_interface.h>

#include <lwip/pbuf.h>

#include "networking_internal.h"
#define NETDEBUG_SUBSYSTEM "net_buf"


///< the default flags to map the buffers
#define NETWORKING_DEFAULT_BUFFER_FLAGS VREGION_FLAGS_READ_WRITE

///< buffer alignment
#define NETWORKING_BUFFER_ALIGN 2048



/**
 * @brief initializes the networking buffer pools
 *
 * @param dev_q     the device queue to create the buffer pool for
 * @param numbuf    number of initial buffers
 * @param size      size of the networking buffer
 * @param retbp     buffer pool to initialize
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_buf_init(struct devq *dev_q, size_t numbuf, size_t size,
                                struct net_buf_pool **retbp)
{
    errval_t err;

    assert(retbp);

    NETDEBUG("initializing buffer pool with %zu x %zu buffers...\n", numbuf, size);

    struct net_buf_pool *netbp = calloc(1, sizeof(*netbp));
    if (netbp == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    netbp->dev_q = dev_q;

    err = net_buf_grow(netbp, numbuf, size);
    if (err_is_fail(err)) {
        free(netbp);
        return err;
    }

    *retbp = netbp;

    return SYS_ERR_OK;
}

/**
 * @brief adds a previously allocated frame to the buffer pool
 *
 * @param bp            buffer pool to add the frame to
 * @param frame         frame capability
 * @param buffersize    size of a buffer
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_buf_add(struct net_buf_pool *bp, struct capref frame, size_t buffersize)
{
    errval_t err;


    struct net_buf_region *reg = calloc(1, sizeof(struct net_buf_region));
    if (reg == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    reg->buffer_size = ROUND_UP(buffersize, NETWORKING_BUFFER_ALIGN);
    reg->framecap = frame;
    reg->pool = bp;

    err = invoke_frame_identify(reg->framecap, &reg->frame);
    if (err_is_fail(err)) {
        goto out_err1;
    }

    NETDEBUG("bp=%p, framesize=%zu kB, elementsize=%zu\n", bp,
             reg->frame.bytes >> 10, buffersize);


    size_t numbuf = reg->frame.bytes / reg->buffer_size;


    reg->netbufs = calloc(numbuf, sizeof(struct net_buf_p));
    if (reg->netbufs == NULL) {
        err = LIB_ERR_MALLOC_FAIL;
        goto out_err1;
    }

    err = vspace_map_one_frame_attr(&reg->vbase, reg->frame.bytes, reg->framecap,
                                    NETWORKING_DEFAULT_BUFFER_FLAGS, NULL, NULL);
    if (err_is_fail(err)) {
        goto out_err2;
    }

    NETDEBUG("netbufs mapped at %p\n", reg->vbase);

    if (bp->dev_q) {
        debug_printf("netbuf: registering region with devq...\n");
        err = devq_register(bp->dev_q, reg->framecap, &reg->regionid);
        if (err_is_fail(err)) {
            goto out_err1;
        }
        NETDEBUG("registered region with devq. pbase=%" PRIxGENPADDR ", regionid=%" PRIx32 "\n",
                  reg->frame.base, reg->regionid);
    }

    size_t offset = 0;
    for (size_t i = 0; i < numbuf; i++) {
        struct net_buf_p *nb = &reg->netbufs[i];

        nb->offset = offset;
        nb->vbase = reg->vbase + offset;
        nb->region = reg;
        nb->pbuf.custom_free_function = net_buf_free;
        nb->allocated = 0;

        /* enqueue to freelist */
        nb->pbuf.pbuf.next =  bp->pbufs;
        bp->pbufs = &nb->pbuf.pbuf;
        bp->buffer_count++;
        bp->buffer_free++;
        offset += reg->buffer_size;
    }

    reg->next = bp->regions;
    bp->regions = reg;

    assert(bp->pbufs);

    NETDEBUG("new region added to pool. free count: %zu / %zu\n",
             bp->buffer_free, bp->buffer_count);

    return SYS_ERR_OK;

    out_err2:
    free(reg->netbufs);
    out_err1:
    free(reg);

    return err;
}

/**
 * @brief grows the number of available buffers
 *
 * @param bp        buffer pool to grow
 * @param numbuf    number of buffers to create
 * @param size      size of a buffer
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t net_buf_grow(struct net_buf_pool *bp, size_t numbuf,
                                size_t size)
{
    errval_t err;

    NETDEBUG("bp=%p, numbuf=%zu, size=%zu\n", bp, numbuf, size);

    size = ROUND_UP(size, NETWORKING_BUFFER_ALIGN);

    size_t alloc_size = ROUND_UP(numbuf * size, BASE_PAGE_SIZE);

    NETDEBUG("allocate frame of %zu kB\n", alloc_size >> 10);

    struct capref frame;
    err = frame_alloc(&frame, alloc_size, &alloc_size);
    if (err_is_fail(err)) {
        return err;
    }

    err =  net_buf_add(bp, frame, size);
    if (err_is_fail(err)) {
        cap_destroy(frame);
    }

    return err;
}


struct pbuf *net_buf_alloc(struct net_buf_pool *bp)
{
    if (bp->pbufs) {
        struct net_buf_p *nb = (struct net_buf_p *)bp->pbufs;
#if BENCH_LWIP_STACK
        nb->timestamp = 0;
#endif
        assert(nb->allocated == 0);

        bp->pbufs = bp->pbufs->next;
        bp->buffer_free--;
        struct pbuf* p;
        p = pbuf_alloced_custom(PBUF_RAW, 0, PBUF_REF, &nb->pbuf,
                                nb->vbase, nb->region->buffer_size);

        nb->allocated = 1;
        NETDEBUG("bp=%p, allocated pbuf=%p, free count %zu / %zu\n", bp, p,
                 bp->buffer_free, bp->buffer_count);
        return p;
    }

    NETDEBUG("bp=%p has no free buffers. Free %zu / %zu\n", bp, bp->buffer_free,
                 bp->buffer_count);

    return NULL;
}

void net_buf_free(struct pbuf *p)
{
    NETDEBUG("pbuf=%p\n", p);
    // TODO sanity checks ?
    struct net_buf_p *nb = (struct net_buf_p *)p;
    struct net_buf_pool *bp = nb->region->pool;
    nb->pbuf.pbuf.next =  bp->pbufs;
    bp->pbufs = &nb->pbuf.pbuf;
    bp->buffer_free++;
    assert(nb->allocated == 1);
    nb->allocated = 0;
}

struct pbuf *net_buf_get_by_region(struct net_buf_pool *bp,
                                             uint32_t regionid, size_t offset)
{
    NETDEBUG("bp=%p, rid=%u, offset=%zu\n", bp, regionid, offset);

    struct net_buf_region *reg = bp->regions;
    while(reg) {
        if (reg->regionid == regionid) {
            /* found */
            if (reg->frame.bytes < offset) {
                return NULL;
            }
            assert(reg->netbufs[offset / reg->buffer_size].allocated);
            return &reg->netbufs[offset / reg->buffer_size].pbuf.pbuf;
        }
        reg = reg->next;
    }
    return NULL;
}
