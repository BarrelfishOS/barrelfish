/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>

#include <barrelfish/barrelfish.h>

#include <virtio/virtio.h>

#include "vbuffer.h"

/**
 * \brief   initializes the buffer allocator and allocates memory for the
 *          buffers
 *
 * \param   alloc   the allocator struct to initialize
 * \param   nbufs   number of buffers to allocate
 * \param   bufsz   size of each buffer to allocate
 *
 * \return  SYS_ERR_OK on success
 */
errval_t virtio_buffer_alloc_init(struct virtio_buffer_allocator **alloc,
                                  size_t nbufs,
                                  size_t bufsz)
{
    errval_t err;

    if (!alloc) {
        return VIRTIO_ERR_ARG_INVALID;
    }

    if (nbufs == 0) {
        return VIRTIO_ERR_NO_BUFFER;
    }

    size_t size = ROUND_UP(bufsz, BASE_PAGE_SIZE);
    if (size != bufsz) {
        debug_printf("WARNING: buffer size rounded up to multiple of page size:"
                     "[%lx] -> [%lx]\n",
                     (uint64_t) bufsz, (uint64_t) size);
    }

    struct capref frame;
    err = frame_alloc(&frame, size * nbufs, NULL);
    if (err_is_fail(err)) {
        return err;
    }

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        cap_destroy(frame);
        return err;
    }

    void *buf;
    err = vspace_map_one_frame_attr(&buf,
                                    size * nbufs,
                                    frame,
                                    VIRTIO_VREGION_FLAGS_RING,
                                    NULL,
                                    NULL);
    if (err_is_fail(err)) {
        cap_destroy(frame);
        return err;
    }

    struct virtio_buffer_allocator *vbuf_alloc;

    vbuf_alloc = malloc(sizeof(*vbuf_alloc));
    if (vbuf_alloc == NULL) {
        vspace_unmap(buf);
        return LIB_ERR_MALLOC_FAIL;
    }

    vbuf_alloc->buffers = calloc(nbufs, sizeof(void *));
    if (vbuf_alloc->buffers == NULL) {
        vspace_unmap(buf);
        free(vbuf_alloc);
        return LIB_ERR_MALLOC_FAIL;
    }

    void *buffers = malloc(nbufs * sizeof(struct virtio_buffer));
    if (buffers == NULL) {
        vspace_unmap(buf);
        free(vbuf_alloc->buffers);
        free(vbuf_alloc);
        return LIB_ERR_MALLOC_FAIL;
    }

    vbuf_alloc->cap = frame;
    vbuf_alloc->size = nbufs;
    vbuf_alloc-> top = nbufs;

    struct virtio_buffer *vbuf;
    for (uint32_t i = 0; i < nbufs; ++i) {
        vbuf = buffers + i;
        vbuf = vbuf_alloc->buffers[i];
        vbuf->buf = (((uint8_t*)buf)+i*size);
        vbuf->length = size;
        vbuf->paddr = id.base +i*size;
        vbuf->state = VIRTIO_BUFFER_S_FREE;
        vbuf->a = vbuf_alloc;
        vbuf_alloc->buffers[i] = vbuf;
    }
    vbuf_alloc->size = size;
    vbuf_alloc->top = size;

    *alloc = vbuf_alloc;

    return SYS_ERR_OK;
}

/**
 * \brief   destroys a buffer allocator by freeing up all the resources used
 *          by the buffers
 *
 * \param   alloc   the allocator to destroy
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_buffer_alloc_destroy(struct virtio_buffer_allocator *alloc)
{
    assert(!"NYI: virtio_buffer_alloc_destroy");
    return SYS_ERR_OK;
}

struct virtio_buffer *virtio_buffer_alloc(struct virtio_buffer_allocator *alloc)
{
    if (alloc->top == 0) {
        return NULL;
    }
    struct virtio_buffer *buf = alloc->buffers[--alloc->top];

    assert(buf->state == VIRTIO_BUFFER_S_FREE);
    buf->state = VIRTIO_BUFFER_S_ALLOCED;
    return buf;
}

/**
 * \brief   frees up a unused buffer by returning it to the allocator
 *
 * \param   buf     the buffer to be freed
 */
errval_t virtio_buffer_free(struct virtio_buffer *buf)
{
    struct virtio_buffer_allocator *alloc = buf->a;
    if (alloc->top >= alloc->size) {
        /* this should actually not happen */
        return VIRTIO_ERR_ALLOC_FULL;
    }

    assert(buf->state == VIRTIO_BUFFER_S_ALLOCED);
    buf->state = VIRTIO_BUFFER_S_FREE;

    alloc->buffers[alloc->top++] = buf;

    return SYS_ERR_OK;
}

/**
 * \brief   returns the backing frame capability of a buffer allocator
 */
errval_t virtio_buffer_alloc_get_cap(struct virtio_buffer_allocator *alloc,
                                     struct capref *ret_cap)
{
    *ret_cap = alloc->cap;
    return SYS_ERR_OK;
}

/**
 * \brief initializes a new VirtIO buffer list to be used for chaining buffers
 *
 * \param bl buffer list to initialize
 *
 * \return SYS_ERR_OK on success
 */
errval_t virtio_blist_init(struct virtio_buffer_list *bl)
{
    memset(bl, 0, sizeof(*bl));
    return SYS_ERR_OK;
}

/**
 * \brief frees up the buffer list by returning the buffers to the allocator
 *
 * \param bl buffer list to be freed
 *
 * \returns SYS_ERR_OK on success
 */
errval_t virtio_blist_free(struct virtio_buffer_list *bl)
{
    errval_t err;
    struct virtio_buffer *buf = virtio_blist_get(bl);
    while(buf) {
        err = virtio_buffer_free(buf);
        assert(err_is_ok(err));
        buf = virtio_blist_get(bl);
    }
    return SYS_ERR_OK;
}

/**
 * \brief appends a buffer to the tail of buffer list
 *
 * \param bl    the list to append the buffer to
 * \param buf   the buffer to be appended
 */
errval_t virtio_blist_append(struct virtio_buffer_list *bl,
                             struct virtio_buffer *buf)
{
    if (buf->lhead) {
        return VIRTIO_ERR_BUFFER_USED;
    }
    if (bl->length == 0) {
        bl->head = buf;
        bl->tail = buf;
    } else {
        bl->tail->next = buf;
        bl->tail = buf;
    }
    buf->lhead = bl;
    buf->next = NULL;
    bl->length++;
    return SYS_ERR_OK;
}

/**
 * \brief returns and removes the head of the list
 *
 * \param bl buffer list
 *
 * \returns pointer to virtio_buffer on sucess
 *          NULL on failuer
 */
struct virtio_buffer *virtio_blist_get(struct virtio_buffer_list *bl)
{
    if (bl->length == 0) {
        return NULL;
    }

    struct virtio_buffer *buf;
    buf = bl->head;
    if (bl->length == 1) {
        bl->head = NULL;
        bl->tail = NULL;
    } else {
        bl->head = buf->next;
    }

    bl->length--;

    buf->next = NULL;
    buf->lhead = NULL;

    return buf;
}

