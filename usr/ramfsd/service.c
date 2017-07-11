/**
 * \file
 * \brief ramfs service
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/bulk_transfer.h>
#include <barrelfish/vregion.h>
#include <if/trivfs_defs.h>
#include <if/monitor_defs.h>

#include "ramfs.h"

#define SERVICE_NAME    "ramfs"

#define FHTAB_SIZE_BITS 8
#define FHTAB_SIZE_MASK ((1U << FHTAB_SIZE_BITS) - 1)
#define FHTAB_LEN       (1U << FHTAB_SIZE_BITS)
#define FH_BITS         (sizeof(trivfs_fh_t) * NBBY)
#define FHGEN_BITS      (FH_BITS - FHTAB_SIZE_BITS)

#define NULL_FH         ((trivfs_fh_t)-1u)

struct msgq_elem {
    enum trivfs_msg_enum msgnum;
    union trivfs_rx_arg_union a;
    struct dirent *dirent;
    struct msgq_elem *next;
};

struct client_state {
    struct dirent *root;
    struct dirent *fhtab[FHTAB_LEN];
    unsigned fhindex, fhgen;
    struct msgq_elem *qstart, *qend; ///< queue of pending replies
    struct bulk_transfer_slave bulk;
    struct vregion *bulk_vregion;
};

/* ------------------------------------------------------------------------- */

static void client_state_init(struct client_state *st, struct dirent *root)
{
    st->root = root;
    memset(st->fhtab, 0, sizeof(st->fhtab));
    st->fhindex = 0;
    st->fhgen = 0;
    st->qstart = st->qend = NULL;
    st->bulk_vregion = NULL;
}

static trivfs_fh_t fh_set(struct client_state *st, struct dirent *d)
{
    // use the next index slot
    st->fhtab[st->fhindex] = d;
    ramfs_incref(d);

    // construct fh: generation and index
    trivfs_fh_t fh = ((trivfs_fh_t)st->fhgen << FHTAB_SIZE_BITS) | st->fhindex;

    // update index (and generation if needed)
    if (++st->fhindex == FHTAB_LEN) {
        st->fhindex = 0;
        st->fhgen++;
    }

    return fh;
}

static struct dirent *fh_get(struct client_state *st, trivfs_fh_t fh)
{
    // unpack fh
    unsigned gen = fh >> FHTAB_SIZE_BITS;
    unsigned idx = fh & FHTAB_SIZE_MASK;

    // check if it's still valid
    struct dirent *e = NULL;
    if ((gen == st->fhgen && idx < st->fhindex)
        || (gen == st->fhgen - 1 && idx >= st->fhindex)) {
        e = st->fhtab[idx];
    }

    if (e == NULL) {
        return NULL; // invalid or stale
    }

    if (ramfs_islive(e)) {
        return e; // valid
    }

    // has been deleted
    st->fhtab[idx] = NULL;
    ramfs_decref(e);
    return NULL;
}

/* ------------------------------------------------------------------------- */

static errval_t ramfs_bulk_init(struct trivfs_binding *b, struct capref shared_frame,
                                errval_t *reterr)
{
    struct client_state *st = b->st;
    errval_t err;

    *reterr = SYS_ERR_OK;

    if (st->bulk_vregion != NULL) {
        *reterr = FS_ERR_BULK_ALREADY_INIT;
        cap_destroy(shared_frame);
        return SYS_ERR_OK;
    }

    // Determine size of frame
    struct frame_identity frameid;
    err = invoke_frame_identify(shared_frame, &frameid);
    if (err_is_fail(err)) {
        *reterr = err_push(err, LIB_ERR_FRAME_IDENTIFY);
        cap_destroy(shared_frame);
        return SYS_ERR_OK;
    }

    size_t bulk_size = frameid.bytes;

    // Map the frame in local memory
    void *bulk_pool;
    err = vspace_map_one_frame_attr(&bulk_pool, bulk_size, shared_frame,
                                    VREGION_FLAGS_READ_WRITE_MPB, NULL,
                                    &st->bulk_vregion);
    if (err_is_fail(err)) {
        cap_destroy(shared_frame);
        *reterr = err_push(err, LIB_ERR_VSPACE_MAP);
        return SYS_ERR_OK;
    }
    assert(bulk_pool != NULL);
    assert(st->bulk_vregion != NULL);

    // Init the bulk transfer library
    err = bulk_slave_init(bulk_pool, bulk_size, &st->bulk);
    assert(err_is_ok(err));

    return SYS_ERR_OK;
}

static errval_t getroot(struct trivfs_binding *b, trivfs_fh_t *rootfh)
{
    struct client_state *st = b->st;
    *rootfh = fh_set(st, st->root);

    return SYS_ERR_OK;
}

static errval_t readdir(struct trivfs_binding *b, trivfs_fh_t dir, uint32_t idx,
                        errval_t *reterr, char *name, bool *isdir,
                        trivfs_fsize_t *size)
{
    errval_t err;
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    name[0] = 0;
    *isdir = false;
    *size = 0;

    struct dirent *d = fh_get(st, dir);
    if (d == NULL) {
        *reterr = FS_ERR_INVALID_FH;
        return SYS_ERR_OK;
    }

    struct dirent *e = NULL;
    err = ramfs_readdir(d, idx, &e);
    if (err_is_fail(err)) {
        *reterr = err;
        return SYS_ERR_OK;
    } else if (e == NULL) {
        *reterr = FS_ERR_INDEX_BOUNDS;
        return SYS_ERR_OK;
    }

    ramfs_incref(e);
    strncpy(name, ramfs_get_name(e), trivfs__read_response_data_MAX_ARGUMENT_SIZE);
    *isdir = ramfs_isdir(e);
    *size = ramfs_get_size(e);
    return SYS_ERR_OK;
}

static errval_t lookup(struct trivfs_binding *b, trivfs_fh_t dir, const char *name,
                       errval_t *reterr, trivfs_fh_t *retfh, bool *isdir)
{
    errval_t err;
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    *retfh = NULL_FH;
    *isdir = false;

    struct dirent *d = fh_get(st, dir);
    if (d == NULL) {
        *reterr = FS_ERR_INVALID_FH;
        return SYS_ERR_OK;
    }

    if (name == NULL) {
        *reterr = FS_ERR_NOTFOUND;
        return SYS_ERR_OK;
    }

    struct dirent *e = NULL;
    err = ramfs_lookup(d, name, &e);
    if (err_is_fail(err)) {
        *reterr = err;
        return SYS_ERR_OK;
    } else if (e == NULL) {
        *reterr = FS_ERR_INDEX_BOUNDS;
        return SYS_ERR_OK;
    }

    *retfh = fh_set(st, e);
    *isdir = ramfs_isdir(e);
    return SYS_ERR_OK;
}

static errval_t getattr(struct trivfs_binding *b, trivfs_fh_t fh,
                        errval_t *reterr, bool *isdir, trivfs_fsize_t *size)
{
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    *isdir = false;
    *size = 0;

    struct dirent *e = fh_get(st, fh);
    if (e == NULL) {
        *reterr = FS_ERR_INVALID_FH;
        return SYS_ERR_OK;

    }
    *isdir = ramfs_isdir(e);
    *size = ramfs_get_size(e);
    return SYS_ERR_OK;
}

static errval_t read(struct trivfs_binding *b, trivfs_fh_t fh,
                 trivfs_offset_t offset, trivfs_fsize_t maxlen,
                 errval_t *reterr, uint8_t data[2048], size_t *len)
{
    errval_t err;
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    uint8_t *buf = NULL;
    *len = 0;

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        *reterr = FS_ERR_INVALID_FH;
        return SYS_ERR_OK;
    }

    err = ramfs_read(f, offset, &buf, len);
    if (err_is_fail(err)) {
        *reterr = err;
        return SYS_ERR_OK;
    }

    if (*len > maxlen) {
        *len = maxlen;
    }
    memcpy(data, buf, *len);
    ramfs_incref(f);
    return SYS_ERR_OK;
}

static errval_t write(struct trivfs_binding *b, trivfs_fh_t fh,
                      trivfs_offset_t offset, const uint8_t *data, size_t len,
                      errval_t *reterr)
{
    errval_t err;
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        *reterr = FS_ERR_INVALID_FH;
        return SYS_ERR_OK;
    }

    uint8_t *buf;

    err = ramfs_grow(f, offset, len, &buf);
    if (err_is_fail(err)) {
        *reterr = err;
        return SYS_ERR_OK;
    }

    memcpy(buf, data, len);
    return SYS_ERR_OK;
}

static errval_t read_bulk(struct trivfs_binding *b, trivfs_fh_t fh,
                          trivfs_offset_t offset, trivfs_fsize_t maxlen,
                          trivfs_bulkid_t bulkid, errval_t *reterr,
                          trivfs_fsize_t *retlen)
{
    errval_t err;
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    uint8_t *ramfsbuf = NULL;
    size_t len = 0;

    if (st->bulk_vregion == NULL) {
        *reterr = FS_ERR_BULK_NOT_INIT;
        return SYS_ERR_OK;
    }

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        *reterr = FS_ERR_INVALID_FH;
        return SYS_ERR_OK;
    }

    err = ramfs_read(f, offset, &ramfsbuf, &len);
    if (err_is_fail(err)) {
        *reterr = err;
        return SYS_ERR_OK;
    }

    // determine local address of bulk buffer
    size_t bulk_size;
    void *bulkbuf = bulk_slave_buf_get_mem(&st->bulk, bulkid, &bulk_size);

    // limit max len to size of bulk buffer
    if (maxlen > bulk_size) {
        maxlen = bulk_size;
    }

    // limit read len to maxlen
    if (len > maxlen) {
        len = maxlen;
    }

    *retlen = len;
    // copy data to bulk buffer
    memcpy(bulkbuf, ramfsbuf, len);
    // prepare bulk buffer for reply
    bulk_slave_prepare_send(&st->bulk, bulkid);
    return SYS_ERR_OK;
}

static errval_t write_bulk(struct trivfs_binding *b, trivfs_fh_t fh,
                       trivfs_offset_t offset, trivfs_fsize_t len,
                       trivfs_bulkid_t bulkid, errval_t *reterr)
{
    errval_t err;
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;

    if (st->bulk_vregion == NULL) {
        *reterr = FS_ERR_BULK_NOT_INIT;
        return SYS_ERR_OK;
    }

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        *reterr = FS_ERR_INVALID_FH;
        return SYS_ERR_OK;
    }

    // determine local address of bulk buffer
    size_t maxlen;
    void *bulkbuf = bulk_slave_buf_get_mem(&st->bulk, bulkid, &maxlen);

    // limit len to size of bulk buffer
    if (len > maxlen) {
        len = maxlen;
    }

    uint8_t *ramfsbuf;
    err = ramfs_grow(f, offset, len, &ramfsbuf);
    if (err_is_fail(err)) {
        *reterr = err;
        return SYS_ERR_OK;
    }

    bulk_slave_prepare_recv(&st->bulk, bulkid);

    memcpy(ramfsbuf, bulkbuf, len);
    return SYS_ERR_OK;
}

static errval_t trivfs_truncate(struct trivfs_binding *b, trivfs_fh_t fh,
                         trivfs_fsize_t newsize, errval_t *reterr)
{
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;

    struct dirent *f = fh_get(st, fh);
    if (f == NULL) {
        *reterr = FS_ERR_INVALID_FH;
    } else {
        *reterr = ramfs_resize(f, newsize);
    }
    return SYS_ERR_OK;
}

static errval_t create(struct trivfs_binding *b, trivfs_fh_t dir, const char *name,
                       errval_t *reterr, trivfs_fh_t *fh)
{
    errval_t err;
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    *fh = NULL_FH;

    struct dirent *d = fh_get(st, dir);
    if (d == NULL) {
        *reterr = FS_ERR_INVALID_FH;
        return SYS_ERR_OK;
    }

    if (name == NULL) {
        *reterr = FS_ERR_EXISTS; // XXX
        return SYS_ERR_OK;
    }

    struct dirent *newf;
    err = ramfs_create(d, name, &newf);
    if (err_is_fail(err)) {
        *reterr = err;
        return SYS_ERR_OK;
    }

    *fh = fh_set(st, newf);
    return SYS_ERR_OK;
}

static errval_t mkdir(struct trivfs_binding *b, trivfs_fh_t dir, const char *name,
                      errval_t *reterr, trivfs_fh_t *fh)
{
    errval_t err;
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;
    *fh = NULL_FH;

    struct dirent *d = fh_get(st, dir);
    if (d == NULL) {
        *reterr = FS_ERR_INVALID_FH;
        return SYS_ERR_OK;
    }

    if (name == NULL) {
        *reterr = FS_ERR_EXISTS; // XXX
        return SYS_ERR_OK;
    }

    struct dirent *newd;
    err = ramfs_mkdir(d, name, &newd);
    if (err_is_fail(err)) {
        *reterr = err;
        return SYS_ERR_OK;
    }

    *fh = fh_set(st, newd);
    return SYS_ERR_OK;
}

static errval_t delete(struct trivfs_binding *b, trivfs_fh_t fh, errval_t *reterr)
{
    *reterr = SYS_ERR_OK;
    struct client_state *st = b->st;

    struct dirent *d = fh_get(st, fh);
    if (d == NULL) {
        *reterr = FS_ERR_INVALID_FH;
    } else {
        *reterr = ramfs_delete(d);
    }
    return SYS_ERR_OK;
}

/* ------------------------------------------------------------------------- */

static struct trivfs_rpc_rx_vtbl rpc_rx_vtbl = {
    .bulk_init_call = ramfs_bulk_init,
    .getroot_call = getroot,
    .readdir_call = readdir,
    .lookup_call = lookup,
    .getattr_call = getattr,
    .read_call = read,
    .write_call = write,
    .read_bulk_call = read_bulk,
    .write_bulk_call = write_bulk,
    .truncate_call = trivfs_truncate,
    .create_call = create,
    .mkdir_call = mkdir,
    .delete_call = delete,
};

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    struct monitor_binding *mb = get_monitor_binding();
    err = mb->tx_vtbl.set_ramfs_iref_request(mb, NOP_CONT, iref);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to send set_ramfs_iref_request to monitor");
    }
}

static errval_t connect_cb(void *st, struct trivfs_binding *b)
{
    // copy my message receive handler vtable to the binding
    b->rpc_rx_vtbl = rpc_rx_vtbl;

    // init state
    struct client_state *bst = malloc(sizeof(struct client_state));
    assert(bst != NULL);
    client_state_init(bst, st);
    b->st = bst;

    return SYS_ERR_OK;
}

errval_t start_service(struct dirent *root)
{
    // Offer the fs service
    return trivfs_export(root, export_cb, connect_cb, get_default_waitset(),
                         IDC_EXPORT_FLAGS_DEFAULT);
}
