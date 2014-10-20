/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <virtio/virtio.h>
#include <virtio/virtio_device.h>
#include <virtio/virtqueue_host.h>
#include <virtio/virtio_host.h>

#include <if/virtio_defs.h>
#include <if/virtio_rpcclient_defs.h>

#include "channel.h"
#include "device.h"
#include "debug.h"

static uint8_t device_open = 0x0;

static iref_t virtio_rpc_svc_iref;

enum virtio_rpc_host_state {
    RPC_HOST_STATE_INVALID,
    RPC_HOST_STATE_EXPORTING,
    RPC_HOST_STATE_FAILED,
    RPC_HOST_STATE_READY
};

static enum virtio_rpc_host_state rpc_client_state = RPC_HOST_STATE_INVALID;

/* -------------------- virtio_open() --------------------------------------- */

struct open_response_state
{
    struct virtio_binding *b;
    errval_t err;
    struct capref frame;
};

struct open_response_state open_err_st;

static void virtio_open_response_cb(void *a)
{
    if (a != &open_err_st) {
        free(a);
    }
}

static void virtio_open_response(void *a)
{
    errval_t err;
    struct open_response_state *st = a;

    struct event_closure txcont = MKCONT(virtio_open_response_cb, st);
    err = virtio_open_response__tx(st->b, txcont, st->err, st->frame);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(virtio_open_response, st);
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "register send failed\n");
                free(st);
            }
        } else {
            DEBUG_ERR(err, "sending reply failed\n");
            free(st);
        }
    }
}

static void virtio_open_call__rx(struct virtio_binding *_binding,
                                  uint8_t backend)
{
    errval_t err;

    VIRTIO_DEBUG_CHAN("Received device_open rpc call\n");

    if (device_open) {
        open_err_st.err = VIRTIO_ERR_DEVICE_STATUS;
        virtio_open_response(&open_err_st);
        return;
    } else {
        device_open = 0x1;
    }

    assert(_binding->st);

    struct virtio_device *vdev = _binding->st;

    struct open_response_state *st = malloc(sizeof(struct open_response_state));
    if (st == NULL) {
        device_open = 0x0;
        open_err_st.err = LIB_ERR_MALLOC_FAIL;
        virtio_open_response(&open_err_st);
        return;
    }

    assert(vdev->cb_h);
    assert(vdev->cb_h->open);

    err = vdev->cb_h->open(vdev, backend, &st->frame);
    if (err_is_fail(err)) {
        device_open = 0x0;
        open_err_st.err = err;
        virtio_open_response(&open_err_st);
    }

    st->b = _binding;
    st->err = err;

    virtio_open_response(st);
}

/* -------------------- virtio_close() -------------------------------------- */

static void virtio_close_response(void *a)
{

}

static  void virtio_close_call__rx(struct virtio_binding *_binding)
{
    virtio_close_response(_binding);
}

/* -------------------- virtio_add() ---------------------------------------- */

struct add_response_state
{
    struct virtio_binding *b;
    errval_t err;
};

struct open_response_state add_err_st;

static void virtio_add_response_cb(void *a)
{
    if (a != &add_err_st) {
        free(a);
    }
}

static void virtio_add_response(void *a)
{
    errval_t err;
    struct add_response_state *st = a;

    struct event_closure txcont = MKCONT(virtio_add_response_cb, st);
    err = virtio_add_response__tx(st->b, txcont, st->err);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(virtio_add_response, st);
            err = st->b->register_send(st->b, get_default_waitset(), txcont);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "virtio_add_response: register send failed\n");
                free(st);
            }
        } else {
            DEBUG_ERR(err, "virtio_add_response: sending reply failed\n");
            free(st);
        }
    }
}

static  void virtio_add_call__rx(struct virtio_binding *_binding,
                                 uint16_t vq_id,
                                 uint16_t ndesc,
                                 uint8_t has_buffers,
                                 struct capref vring)
{
    VIRTIO_DEBUG_CHAN("Received virtq_add rpc call\n");

    if (!_binding->st) {
        /* cannot open a device twice! */
        add_err_st.err = VIRTIO_ERR_DEVICE_STATUS;
        virtio_open_response(&add_err_st);
        return;
    }

    struct open_response_state *st = malloc(sizeof(struct open_response_state));
    if (st == NULL) {
        add_err_st.err = LIB_ERR_MALLOC_FAIL;
        virtio_open_response(&add_err_st);
        return;
    }

    struct virtio_device *vdev = _binding->st;

    assert(vdev->cb_h);
    assert(vdev->cb_h->add);

    st->b = _binding;

    st->err = virtio_vq_host_init_vring(vdev, vring, vq_id, ndesc, has_buffers);
    if (err_is_fail(st->err)) {
        virtio_add_response(st);
        return;
    }

    st->b = _binding;
    st->err = vdev->cb_h->add(vdev,vring, ndesc, has_buffers, vq_id);

    virtio_add_response(st);
}


/* -------------------- virtio_ext() ---------------------------------------- */

static void virtio_ext_response(void *a)
{

}

static  void virtio_extend_call__rx(struct virtio_binding *_binding,
                                    uint16_t vq_id,
                                    struct capref vbuf)
{
    virtio_ext_response(_binding);
}

/* -------------------- virtio_req() ---------------------------------------- */

static void virtio_req_response(void *a)
{

}

static  void virtio_req_call__rx(struct virtio_binding *_binding,
                                 uint64_t size)
{
    virtio_req_response(_binding);
}




struct virtio_rx_vtbl s_rx_vtbl = {
    .open_call = virtio_open_call__rx,
    .close_call = virtio_close_call__rx,
    .add_call = virtio_add_call__rx,
    .extend_call = virtio_extend_call__rx,
    .req_call = virtio_req_call__rx,
};


static errval_t connect_cb(void *st, struct virtio_binding *b)
{
    VIRTIO_DEBUG_CHAN("New guest connection\n");

    /*
     * this would be the point where we should initialize a new VirtIO device,
     * however we support just one device at this stage.
     *
     * We assign the current device as the binding state.
     */
    b->st = st;

    b->rx_vtbl = s_rx_vtbl;

    return SYS_ERR_OK;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        rpc_client_state = RPC_HOST_STATE_FAILED;
        DEBUG_ERR(err, "Export failed");
        return;
    }

    struct virtio_device *vdev = st;

    virtio_rpc_svc_iref = iref;

    VIRTIO_DEBUG_CHAN("Registering [%s] with iref [%u]\n", vdev->hc_iface, iref);
    err = nameservice_register(vdev->hc_iface, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        rpc_client_state = RPC_HOST_STATE_FAILED;
        return;
    }

    rpc_client_state = RPC_HOST_STATE_READY;
}




errval_t virtio_host_flounder_init(struct virtio_device *vdev)
{
    errval_t err;

    VIRTIO_DEBUG_CHAN("initiate exporting service\n");

    rpc_client_state = RPC_HOST_STATE_EXPORTING;

    err = virtio_export(vdev,
                        export_cb, connect_cb,
                        get_default_waitset(),
                        IDC_BIND_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        return err;
    }

    VIRTIO_DEBUG_CHAN("Waiting for export reply\n");
    while(rpc_client_state == RPC_HOST_STATE_EXPORTING) {
        messages_wait_and_handle_next();
    }

    if (rpc_client_state == RPC_HOST_STATE_FAILED) {
        VIRTIO_DEBUG_CHAN("Export failed\n");
        return FLOUNDER_ERR_BIND;
    }

    VIRTIO_DEBUG_CHAN("Service ready\n");

    return SYS_ERR_OK;
}
