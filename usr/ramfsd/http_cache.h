/**
 * \file
 * \brief File cache for HTTP server
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef HTTP_CACHE_H
#define HTTP_CACHE_H
#include "webserver_session.h"
err_t http_cache_init (struct ip_addr server, const char *path,
                     void (*callback)(void));
long decrement_buff_holder_ref (struct buff_holder *bh);
long decrement_reference (struct http_conn *cs);

struct http_cache_entry {
    int                 valid;      /* flag for validity of the data */
    char                *name;      /* name of the cached file */
    size_t              copied;     /* how much data is copied? */
    int                 loading;    /* flag indicating if data is loading */
    struct buff_holder  *hbuff;      /* holder for buffer */
    struct nfs_fh3      file_handle;    /* for NFS purpose */
    struct http_conn *conn;     /* list of connections waiting for data */
    struct http_conn *last;        /* for quick insertions at end */
    struct http_cache_entry *next;   /* for linked list */
};

#endif // HTTP_CACHE_H
