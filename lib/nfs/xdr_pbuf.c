/**
 * \file
 * \brief XDR implementation using LWIP PBuf structures
 *
 * Uses standard XDR structure. Private fields in XDR are used as follows:
 *  * x_private points to the first struct pbuf in a pbuf chain
 *  * x_base points to the current struct pbuf in a pbuf chain
 *  * x_handy is the position (offset) _within the current pbuf_
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <nfs/xdr.h>
#include "xdr_pbuf.h"
#include <net_sockets/net_sockets.h>

/* make space within the buffer, returns NULL if it won't fit */
static inline int32_t *make_space(XDR *xdr, size_t size)
{
    if (xdr->x_handy + size > xdr->size) {
        fprintf(stderr, "xdr_pbuf: make_space(%zu) failing (%zu available)\n",
                size, xdr->size - (size_t)xdr->x_handy);
        return NULL;
    } else {
        int32_t *ret = (int32_t *)((char *)xdr->x_base + xdr->x_handy);
        xdr->x_handy += size;
        return ret;
    }
}

/* get a word from underlying stream */
static bool xdr_pbuf_getint32(XDR *xdr, int32_t *ret)
{
    int32_t *buf = make_space(xdr, sizeof(int32_t));
    if (buf) {
        *ret = ntohl((uint32_t)*buf);
        return true;
    } else {
        return false;
    }
}

/* put a word to underlying stream */
static bool xdr_pbuf_putint32(XDR *xdr, const int32_t *val)
{
    int32_t *buf = make_space(xdr, sizeof(int32_t));
    if (buf) {
        *buf = htonl((uint32_t)(*val));
        return true;
    } else {
        return false;
    }
}

/* common implementation of getbytes and putbytes */
static bool movebytes(bool copyin, XDR *xdr, char *callerbuf, size_t nbytes)
{
    while (nbytes > 0) {
        size_t space = xdr->size - xdr->x_handy;
        if (space > nbytes) {
            space = nbytes;
        }
        int32_t *buf = make_space(xdr, space);
        assert(buf != NULL);
        if (copyin) {
            memcpy(buf, callerbuf, space);
        } else {
            memcpy(callerbuf, buf, space);
        }
        nbytes -= space;
        callerbuf += space;
    }
    return true;
}

/* get some bytes from underlying stream */
static bool xdr_pbuf_getbytes(XDR *xdr, char *retbuf, size_t nbytes)
{
    return movebytes(false, xdr, retbuf, nbytes);
}

/* put some bytes to underlying stream */
static bool xdr_pbuf_putbytes(XDR *xdr, const char *inbuf, size_t nbytes)
{
    return movebytes(true, xdr, (char *)inbuf, nbytes);
}

/* returns bytes off from beginning */
static size_t xdr_pbuf_getpostn(XDR *xdr)
{
    return xdr->x_handy;
}

/* lets you reposition the stream */
static bool xdr_pbuf_setpostn(XDR *xdr, size_t pos)
{
    if (pos > xdr->size) {
        return false;
    } else {
        xdr->x_base = xdr->x_private;
        xdr->x_handy = pos;
        return true;
    }
}

/* buf quick ptr to buffered data */
static int32_t *xdr_pbuf_inline(XDR *xdr, size_t nbytes)
{
    assert(nbytes % BYTES_PER_XDR_UNIT == 0);
    return make_space(xdr, nbytes);
}

/* free privates of this xdr_stream */
static void xdr_pbuf_destroy(XDR *xdr)
{
    net_free(xdr->x_private);
}

/// XDR operations table
static struct xdr_ops xdr_pbuf_ops = {
    .x_getint32 = xdr_pbuf_getint32,
    .x_putint32 = xdr_pbuf_putint32,
    .x_getbytes = xdr_pbuf_getbytes,
    .x_putbytes = xdr_pbuf_putbytes,
    .x_getpostn = xdr_pbuf_getpostn,
    .x_setpostn = xdr_pbuf_setpostn,
    .x_inline = xdr_pbuf_inline,
    .x_destroy = xdr_pbuf_destroy,
};

/**
 * \brief Create XDR and allocate PBUF for serialising data
 *
 * \param xdr Memory for XDR struct, to be initialised
 * \param size Size of pbuf buffers to allocate
 *
 * \returns True on success, false on error
 */
bool xdr_create_send(XDR *xdr, size_t size)
{
    assert(xdr != NULL);
    assert(size % BYTES_PER_XDR_UNIT == 0);
    xdr->x_base = xdr->x_private = net_alloc(size);
    xdr->size = size;
    assert(xdr->x_private);
    xdr->x_op = XDR_ENCODE;
    xdr->x_ops = &xdr_pbuf_ops;
    xdr->x_handy = 0;
    return true;
}

/**
 * \brief Create XDR for deserialising data in given PBUF
 *
 * \param xdr Memory for XDR struct, to be initialised
 * \param pbuf LWIP packet buffer pointer
 *
 * \returns True on success, false on error
 */
void xdr_create_recv(XDR *xdr, void *data, size_t size)
{
    assert(xdr != NULL);
    assert(size % BYTES_PER_XDR_UNIT == 0);
    xdr->x_base = xdr->x_private = data;
    xdr->size = size;
    assert(xdr->x_private);
    memcpy(xdr->x_private, data, size);
    xdr->x_op = XDR_DECODE;
    xdr->x_ops = &xdr_pbuf_ops;
    xdr->x_handy = 0;
}
