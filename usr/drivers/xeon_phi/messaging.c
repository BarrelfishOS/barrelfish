/**
 * \file
 * \brief Boot module for the Xeon Phi
 *
 * Loads the co processor OS onto the card and boots it
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/xeon_phi_messaging_defs.h>

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_messaging.h>

#include "xeon_phi.h"
#include "messaging.h"
#include "spawn.h"
#include "sysmem_caps.h"
/*
 * This messaging infrastructure is only used to bootstrap other messaging
 * implementations such as UMP or VirtIO.
 */

/**
 * \brief initializes the messaging boostrap infrastructure between the
 *        host and the card
 *
 * \param phi the xeon phi to initialize the basic messaging bootstrap
 *
 * \return SYS_ERR_OK on success
 */
errval_t messaging_init(struct xeon_phi *phi,
                        struct capref frame)
{
    errval_t err;

    assert(phi->msg == NULL);

    struct msg_info *mi = malloc(sizeof(struct msg_info));
    if (mi == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    if (capref_is_null(frame)) {
        err = frame_alloc(&mi->frame, XEON_PHI_MSG_INIT_SIZE, &mi->size);
        if (err_is_fail(err)) {
            return err;
        }
    } else {
        mi->frame = frame;
    }

    struct frame_identity id;
    err = invoke_frame_identify(mi->frame, &id);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    mi->base = id.base;
    mi->size = 1UL << id.bits;

    void *addr;
    err = vspace_map_one_frame(&addr, mi->size, mi->frame, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    XMESSAGING_DEBUG("Messaginf frame mapped: [%016lx -> %016lx, size = %lx ]\n",
                     mi->base,
                     (uintptr_t )addr,
                     mi->size);

    mi->meta = addr;
    if (phi->is_client) {
        mi->in = mi->meta->h2c.data;
        mi->out = mi->meta->c2h.data;
    } else {
        mi->in = mi->meta->c2h.data;
        mi->out = mi->meta->h2c.data;
    }

    if (capref_is_null(frame)) {
        memset(addr, 0, mi->size);
    }

    phi->msg = mi;

    if (!phi->is_client) {
        struct xeon_phi_boot_params *bp;
        bp = (struct xeon_phi_boot_params *) (phi->apt.vbase + phi->os_offset);
        bp->msg_base = mi->base;
    }

    return SYS_ERR_OK;
}


struct msg_open_state {
    struct capref frame;
    uint8_t type;
    struct xeon_phi_messaging_binding *b;
};

static void msg_send_open_cb(void *a)
{
    XMESSAGING_DEBUG("sent..\n");
    free(a);
}

static void msg_send_open(void *a)
{
    XMESSAGING_DEBUG("send open..\n");
    errval_t err;
    struct msg_open_state *s = a;
    struct xeon_phi_messaging_binding *b = s->b;
    struct event_closure txcont = MKCONT(msg_send_open_cb, s);

    err = xeon_phi_messaging_open__tx(b, txcont, s->frame, s->type);

    if (err_is_fail(err)) {
      DEBUG_ERR(err, "error sending msg_string message\n");
      if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
        txcont = MKCONT(msg_send_open, s);
            err = b->register_send(b, ws, txcont);
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
            }
        }
    }
}


static void msg_bind_cb(void *st, errval_t err, struct xeon_phi_messaging_binding *b)
{
    XMESSAGING_DEBUG("bind callback..\n");
    if (err_is_fail(err)) {
        debug_printf("binding failed\n");
    }

    struct msg_open_state *s = st;
    s->b = b;

    msg_send_open(st);
}

static errval_t handle_msg_open(struct xeon_phi *phi,
                                struct xeon_phi_msg_open *open)
{
    errval_t err;
    iref_t iref;
    XMESSAGING_DEBUG("nameservice lookup\n");
    err = nameservice_blocking_lookup(open->name, &iref);
    if (err_is_fail(err)) {
        return err;
    }

    struct msg_open_state *st = malloc(sizeof(struct msg_open_state));

    err = sysmem_cap_request(open->base, open->size, &st->frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to get the cap request\n");
        return err;
    }

    st->type = open->type;
    XMESSAGING_DEBUG("binding...\n");
    err = xeon_phi_messaging_bind(iref, msg_bind_cb, st, get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "binding failes\n");
    }
    return err;
}

static errval_t handle_msg_err(struct xeon_phi *phi,
                               struct xeon_phi_msg_err *err)
{
    return SYS_ERR_OK;
}

static errval_t handle_msg_recv(struct xeon_phi *phi,
                                struct xeon_phi_msg_data *data)
{
    errval_t err;
    switch(data->ctrl.flags.cmd) {
        case XEON_PHI_MSG_CMD_OPEN :
            XMESSAGING_DEBUG("Received Open Command\n");
            err = handle_msg_open(phi, &data->data.open);
            break;
        case  XEON_PHI_MSG_CMD_SPAWN :
            XMESSAGING_DEBUG("Received Spawn Command\n");
            err = xeon_phi_spawn_spawn(phi, &data->data.spawn);
            break;
        case  XEON_PHI_MSG_CMD_ERROR :
            XMESSAGING_DEBUG("Received Error Command\n");
            err = handle_msg_err(phi, &data->data.err);
            break;
        case  XEON_PHI_MSG_CMD_OTHER :
            XMESSAGING_DEBUG("Received Other Command\n");
            err = -1;
            break;
        default:
            USER_PANIC("Received message with unknown command\n");
            break;
    }

    if (err_is_fail(err)) {

    }

    return SYS_ERR_OK;
}

/**
 * \brief polls the shared messaging frame for a new message
 *
 * \param phi the xeon phi to poll
 *
 * \return SYS_ERR_OK on success
 */
errval_t messaging_poll(struct xeon_phi *phi)
{
    errval_t err;

    if (phi->msg == NULL) {
        return -1;
    }

    struct xeon_phi_msg_data *data = phi->msg->in;
    if (data->ctrl.valid == XEON_PHI_MSG_STATE_VALID) {
        err = handle_msg_recv(phi, data);

        struct xeon_phi_msg_chan *chan;
        if (phi->is_client) {
            chan = &phi->msg->meta->h2c;
        } else {
            chan = &phi->msg->meta->c2h;
        }

        data->ctrl.valid = XEON_PHI_MSG_STATE_CLEAR;

        data = data + 1;
        if (data == (chan->data + XEON_PHI_MSG_CHANS)) {
            data = chan->data;
        }
        phi->msg->in = data;

    }

    return SYS_ERR_OK;
}

/**
 * \brief registers a new frame for the shared messaging channel to be used
 *        for communication purposes
 *
 * \param phi   the card to initialize the messaging for
 * \param frame capability representing the frame to be used
 */
errval_t messaging_channel_open(struct xeon_phi *phi,
                                struct capref frame)
{

    return SYS_ERR_OK;
}

/**
 * \brief sends a new message over the host-card channel
 *
 * \param phi    pointer to the xeon phi data structure
 * \param data   pointer to the data to send
 * \param length amount of data to be sent
 *
 * \return
 */
errval_t messaging_send(struct xeon_phi *phi,
                        struct xeon_phi_msg_hdr hdr,
                        void *data)
{
    XMESSAGING_DEBUG("Sending Message. [%016lx]\n", *((uint64_t *)data));
    struct xeon_phi_msg_data *msg = phi->msg->out;

    // allowing only single messages for now
    assert(hdr.size <= sizeof(msg->data));

    // in case the other side has not yet read this message, we have to wait
    while (msg->ctrl.valid != XEON_PHI_MSG_STATE_CLEAR) {
        thread_yield();
    }

    /* copy the payload  */
    memcpy(msg->data.raw, data, hdr.size);

    /* ensure that the state of the message is clear before copying it */
    hdr.valid = XEON_PHI_MSG_STATE_CLEAR;
    msg->ctrl = hdr;

    /* set the valid field to signal the other side */
    msg->ctrl.valid = XEON_PHI_MSG_STATE_VALID;

    XMESSAGING_DEBUG("Message sent.\n");

    /* update the next out pointer */
    struct xeon_phi_msg_chan *chan;
    if (phi->is_client) {
        chan = &phi->msg->meta->c2h;
    } else {
        chan = &phi->msg->meta->h2c;
    }

    msg = msg + 1;
    if (msg == (chan->data + XEON_PHI_MSG_CHANS)) {
        msg = chan->data;
    }
    phi->msg->out = msg;

    return SYS_ERR_OK;
}
