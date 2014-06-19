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
#include <virtio/virtqueue.h>
#include <virtio/virtio_guest.h>

#include <if/virtio_defs.h>
#include <if/virtio_rpcclient_defs.h>

#include "channel.h"
#include "debug.h"


static struct virtio_rpc_client virtio_rpc_client;

static iref_t virtio_rpc_svc_iref;

enum virtio_rpc_client_state {
    RPC_CLIENT_STATE_INVALID,
    RPC_CLIENT_STATE_BINDING,
    RPC_CLIENT_STATE_FAILED,
    RPC_CLIENT_STATE_READY
};

static enum virtio_rpc_client_state rpc_client_state = RPC_CLIENT_STATE_INVALID;


/**
 *
 */
static  errval_t open_device(uint8_t backend,
                             struct capref *ret_frame)
{
    errval_t err, msg_err;

    VIRTIO_DEBUG_CHAN("open device\n");

    if (rpc_client_state != RPC_CLIENT_STATE_READY) {
        /* TODO: error code */
        return -1;
    }

    err = virtio_rpc_client.vtbl.open(&virtio_rpc_client,
                                      backend,
                                      &msg_err,
                                      ret_frame);
    if (err_is_fail(err)) {
        return err;
    }
    return msg_err;
}

/**
 *
 */
static  errval_t close_device(void)
{
    errval_t err;

    if (rpc_client_state != RPC_CLIENT_STATE_READY) {
        /* TODO: error code */
        return -1;
    }

    err =  virtio_rpc_client.vtbl.close(&virtio_rpc_client);
    if (err_is_fail(err)) {
        return err;
    }
    return SYS_ERR_OK;
}

/**
 *
 */
static  errval_t add_vring(struct virtqueue *vq)
{
    errval_t err, msg_err;

    if (rpc_client_state != RPC_CLIENT_STATE_READY) {
        /* TODO: error code */
        return -1;
    }

    struct capref frame;
    virtio_virtqueue_get_vring_cap(vq, &frame);

    uint16_t id = virtio_virtqueue_get_queue_index(vq);

    uint8_t buffers = virtio_virtqueue_has_buffers(vq);

    uint16_t ndesc = virtio_virtqueue_get_num_desc(vq);

    err =  virtio_rpc_client.vtbl.add(&virtio_rpc_client, id, ndesc, buffers, frame, &msg_err);
    if (err_is_fail(err)) {
        return err;
    }
    return msg_err;
}

/**
 *
 */
static  errval_t extend_vring(uint16_t vq_id,
                              struct capref vbuf)
{
    errval_t err, msg_err;

    if (rpc_client_state != RPC_CLIENT_STATE_READY) {
        /* TODO: error code */
        return -1;
    }

    err =  virtio_rpc_client.vtbl.extend(&virtio_rpc_client, vq_id, vbuf, &msg_err);
    if (err_is_fail(err)) {
        return err;
    }
    return msg_err;
}

/**
 *
 */
static  errval_t request_mem(uint64_t size,
                             struct capref *cap)
{
    errval_t err, msg_err;

    if (rpc_client_state != RPC_CLIENT_STATE_READY) {
        /* TODO: error code */
        return -1;
    }

    err =  virtio_rpc_client.vtbl.req(&virtio_rpc_client, size, &msg_err, cap);
    if (err_is_fail(err)) {
        return err;
    }
    return msg_err;
}


static struct virtio_guest_chan_fn vguest_fc_fn =  {
    .open = open_device,
    .close = close_device,
    .add = add_vring,
    .ext = extend_vring,
    .req = request_mem,
};

static void bind_cb(void *st, errval_t err, struct virtio_binding *b)
{
    if(err_is_fail(err)) {
        rpc_client_state = RPC_CLIENT_STATE_FAILED;
        return;
    }

    VIRTIO_DEBUG_CHAN("Initializing RPC client\n");
    err = virtio_rpc_client_init(&virtio_rpc_client, b);
    if (err_is_fail(err)) {
        rpc_client_state = RPC_CLIENT_STATE_FAILED;
    }

    vguest_chan_fn = &vguest_fc_fn;

    rpc_client_state = RPC_CLIENT_STATE_READY;
}





errval_t virtio_guest_flounder_init(char *iface)
{
    errval_t err;

    VIRTIO_DEBUG_CHAN("looking up iface [%s]\n", iface);
    err = nameservice_blocking_lookup(iface, &virtio_rpc_svc_iref);
    if (err_is_fail(err)) {
        return err;
    }

    VIRTIO_DEBUG_CHAN("initiate binding to iref [%u]\n", virtio_rpc_svc_iref);

    rpc_client_state = RPC_CLIENT_STATE_BINDING;

    err = virtio_bind(virtio_rpc_svc_iref,
                      bind_cb,
                      NULL,
                      get_default_waitset(),
                      IDC_BIND_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        return err;
    }

    VIRTIO_DEBUG_CHAN("Waiting for binding reply\n");
    while(rpc_client_state == RPC_CLIENT_STATE_BINDING) {
        messages_wait_and_handle_next();
    }

    if (rpc_client_state == RPC_CLIENT_STATE_FAILED) {
        VIRTIO_DEBUG_CHAN("Bind failed\n");
        return FLOUNDER_ERR_BIND;
    }

    VIRTIO_DEBUG_CHAN("Host channel ready\n");

    return SYS_ERR_OK;
}
