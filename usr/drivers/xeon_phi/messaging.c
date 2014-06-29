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

#include "xeon_phi_internal.h"
#include "messaging.h"
#include "spawn.h"
#include "smpt.h"
#include "sysmem_caps.h"

struct xeon_phi *xeon_phi;

/*
 * Keeping track of connected domains
 */
struct xpm_domain
{
    struct xeon_phi_messaging_binding *b;
    iref_t iref;
    uint8_t xphi_id;
    struct xpm_domain *next;
    char name[0];
};

static struct xpm_domain *msg_domains;

static struct xeon_phi_messaging_binding *xpm_get_binding(char *name)
{
    struct xpm_domain *dom = msg_domains;
    while (dom) {
        if (!strcmp(dom->name, name)) {
            return dom->b;
        }
        dom = dom->next;
    }
    return NULL;
}

static void xpm_insert_binding(struct xpm_domain *dom)
{
    dom->next = msg_domains;
    msg_domains = dom;
}

/*
 * This messaging infrastructure is only used to bootstrap other messaging
 * implementations such as UMP or VirtIO.
 */

/**
 * \brief initializes the messaging boostrap infrastructure between the
 *        two Xeon Phi cards
 *
 * \param phi the xeon phi to initialize the basic messaging bootstrap
 *
 * \return SYS_ERR_OK on success
 */
errval_t messaging_init_xphi(uint8_t xphi,
                             struct xeon_phi *phi,
                             struct capref frame,
                             uint8_t is_client)
{
    errval_t err;

    XMESSAGING_DEBUG("initializing intra Xeon Phi [%u <-> %u]\n", phi->id, xphi);

    assert(xphi < XEON_PHI_NUM_MAX);
    assert(xphi != phi->id);

    assert(phi->topology[xphi].msg == NULL);

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
    mi->is_client = is_client;

#ifdef __k1om__
    /*
     * XXX: the host does not need to do this
     */
    void *addr;
    err = vspace_map_one_frame(&addr, mi->size, mi->frame, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    XMESSAGING_DEBUG("Messaging frame mapped: [%016lx -> %016lx, size = %lx ]\n",
                     mi->base,
                     (uintptr_t )addr,
                     mi->size);

    mi->meta = addr;
    if (mi->is_client) {
        mi->in = mi->meta->h2c.data;
        mi->out = mi->meta->c2h.data;
    } else {
        mi->in = mi->meta->c2h.data;
        mi->out = mi->meta->h2c.data;
    }

    if (capref_is_null(frame)) {
        memset(addr, 0, mi->size);
    }

#else
    err = messaging_send_bootstrap(mi->base, id.bits, xphi, mi->is_client);
    if (err_is_fail(err)) {
        free(mi);
        return err;
    }
#endif

    phi->topology[xphi].msg = mi;
    phi->topology[xphi].state = XNODE_STATE_READY;
    return SYS_ERR_OK;
}

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

    xeon_phi = phi;

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

    XMESSAGING_DEBUG("Messaging frame mapped: [%016lx -> %016lx, size = %lx ]\n",
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

    mi->is_client = phi->is_client;

    if (capref_is_null(frame)) {
        memset(addr, 0, mi->size);
    }

    phi->msg = mi;

    phi->topology[phi->id].msg = mi;
    phi->topology[phi->id].local = phi;
    phi->topology[phi->id].state = XNODE_STATE_READY;

    if (!phi->is_client) {
        struct xeon_phi_boot_params *bp;
        bp = (struct xeon_phi_boot_params *) (phi->apt.vbase + phi->os_offset);
        bp->msg_base = mi->base;
    }

    return SYS_ERR_OK;
}

struct msg_open_state
{
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
    struct msg_open_state *st = a;
    assert(st);
    assert(st->b);

    struct event_closure txcont = MKCONT(msg_send_open_cb, a);

    err = xeon_phi_messaging_open__tx(st->b, txcont, st->frame, st->type);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error sending msg_string message\n");
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
            txcont = MKCONT(msg_send_open, st);
            err = st->b->register_send(st->b, ws, txcont);
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
            }
        }
    }
}

static void msg_bind_cb(void *st,
                        errval_t err,
                        struct xeon_phi_messaging_binding *b)
{
    struct xpm_domain *dom = st;

    XMESSAGING_DEBUG("Binding to to %s @ iref:%u %s\n",
                     dom->name,
                     dom->iref,
                     (err_is_ok(err)) ? "SUCCEEDED" : "FAILED");

    if (err_is_fail(err)) {
        dom->b = (void *) -1;
    }

    b->st = st;
    dom->b = b;

    xpm_insert_binding(dom);
}

static errval_t handle_msg_bootstrap(struct xeon_phi *phi,
                                     struct xeon_phi_msg_bootstrap *bootstrap)
{
    errval_t err;

    struct xnode *node = &phi->topology[bootstrap->xphi_id];
    if (node->msg) {
        XMESSAGING_DEBUG("NOTE: already established channel to %u\n",
                         bootstrap->xphi_id);
        return SYS_ERR_OK;  // XXX: maybe indicate error?
    }

    struct capref frame;

    err = sysmem_cap_request(bootstrap->base, bootstrap->bits, &frame);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to get the cap request\n");
        return err;
    }

    err = messaging_init_xphi(bootstrap->xphi_id, phi, frame, bootstrap->is_client);
    if (err_is_fail(err)) {
        return err;
    }

    // the client acks to the host
    if (bootstrap->is_client) {
        err = messaging_send_ready(bootstrap->xphi_id);
        assert(err_is_ok(err));
    }


    return SYS_ERR_OK;
}

static errval_t handle_msg_ready(struct xeon_phi *phi,
                                 struct xeon_phi_msg_ready *ready)
{
    XMESSAGING_DEBUG("Received ready command from [%u]\n", ready->xphi_id);

    assert(!phi->topology[ready->xphi_id].msg->is_client);

    return SYS_ERR_OK;
}

static errval_t handle_msg_open(struct xeon_phi *phi,
                                uint8_t xphi_id,
                                struct xeon_phi_msg_open *open)
{
    errval_t err;

    struct xeon_phi_messaging_binding *b = xpm_get_binding(open->iface);
    if (b == NULL) {
        XMESSAGING_DEBUG("Establishing connection to %s\n", open->iface);

        size_t name_len = strlen(open->iface) + 1;
        struct xpm_domain *dom = malloc(sizeof(struct xpm_domain) + name_len);
        if (dom == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }

        dom->b = NULL;

        strncpy(dom->name, open->iface, name_len);
        XMESSAGING_DEBUG("Establishing connection to %s\n", dom->name);
        err = nameservice_blocking_lookup(dom->name, &dom->iref);
        if (err_is_fail(err)) {
            return err;
        }

        XMESSAGING_DEBUG("Binding to %s @ iref:%u\n", dom->name, dom->iref);
        err = xeon_phi_messaging_bind(dom->iref,
                                      msg_bind_cb,
                                      dom,
                                      get_default_waitset(),
                                      IDC_BIND_FLAGS_DEFAULT);
        if (err_is_fail(err)) {
            free(dom);
            return err;
        }

        while (dom->b == NULL) {
            messages_wait_and_handle_next();
        }

        if (dom->b == (void *) -1) {
            return FLOUNDER_ERR_BIND;
        }

        b = dom->b;
    }

    struct msg_open_state *st = malloc(sizeof(struct msg_open_state));

    /* we have to add the offset, this will be zero if the IDs are the same */
    lpaddr_t offset = smpt_get_coprocessor_address(xeon_phi, xphi_id);

    open->base += offset;


    err = sysmem_cap_request(open->base, open->bits, &st->frame);
    if (err_is_fail(err)) {
        free(st);
        USER_PANIC_ERR(err, "failed to get the cap request\n");
        return err;
    }

    st->type = open->type;
    st->b = b;
    msg_send_open(st);

    return SYS_ERR_OK;
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
    switch (data->ctrl.flags.cmd) {
        case XEON_PHI_MSG_CMD_OPEN:
            XMESSAGING_DEBUG("Received Open Command\n");
            err = handle_msg_open(phi, data->ctrl.flags.xphi_id, &data->data.open);
            break;
        case XEON_PHI_MSG_CMD_BOOTSTRAP:
            XMESSAGING_DEBUG("Received Bootstrap Command\n");
            err = handle_msg_bootstrap(phi, &data->data.bootstrap);
            break;
        case XEON_PHI_MSG_CMD_READY:
            XMESSAGING_DEBUG("Received Ready Command\n");
            err = handle_msg_ready(phi, &data->data.ready);
            break;
        case XEON_PHI_MSG_CMD_SPAWN:
            XMESSAGING_DEBUG("Received Spawn Command\n");
            err = xeon_phi_spawn_spawn(phi, &data->data.spawn);
            break;
        case XEON_PHI_MSG_CMD_ERROR:
            XMESSAGING_DEBUG("Received Error Command\n");
            err = handle_msg_err(phi, &data->data.err);
            break;
        case XEON_PHI_MSG_CMD_OTHER:
            XMESSAGING_DEBUG("Received Other Command\n");
            err = -1;
            break;
        default:
            USER_PANIC("Received message with unknown command\n")
            ;
            break;
    }

    if (err_is_fail(err)) {

    }

    return SYS_ERR_OK;
}

static errval_t messaging_do_poll(struct xeon_phi *phi,
                                  struct msg_info *msg_ctrl)
{
    uint8_t had_data = 0x0;
    errval_t err;

    assert(msg_ctrl);
    assert(msg_ctrl->meta);
    struct xeon_phi_msg_data *data = msg_ctrl->in;
    while (data->ctrl.valid == XEON_PHI_MSG_STATE_VALID) {
        had_data = 0x1;
        err = handle_msg_recv(phi, data);

        struct xeon_phi_msg_chan *chan;
        if (msg_ctrl->is_client) {
            chan = &msg_ctrl->meta->h2c;
        } else {
            chan = &msg_ctrl->meta->c2h;
        }

        data->ctrl.valid = XEON_PHI_MSG_STATE_CLEAR;

        data = data + 1;
        if (data == (chan->data + XEON_PHI_MSG_CHANS)) {
            data = chan->data;
        }
        msg_ctrl->in = data;
    }

    if (had_data) {
        return SYS_ERR_OK;
    } else {
        return LIB_ERR_NO_EVENT;
    }
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

    assert(phi);
    assert(phi->msg != NULL);

    uint8_t is_idle = 0x1;
#ifdef __k1om__
    for (uint32_t i = 0; i < XEON_PHI_NUM_MAX; ++i) {
        if (phi->topology[i].state != XNODE_STATE_READY) {
            continue;
        }
        assert(phi->topology[i].id == i);
        err = messaging_do_poll(phi, phi->topology[i].msg);
        is_idle = is_idle && (err_no(err) == LIB_ERR_NO_EVENT);
    }
#else
    err = messaging_do_poll(phi, phi->msg);
    is_idle = is_idle && (err_no(err) == LIB_ERR_NO_EVENT);
#endif
    if (is_idle) {
        return LIB_ERR_NO_EVENT;
    } else {
        return SYS_ERR_OK;
    }

}

/**
 * \brief sends an bootstrap message to the Xeon Phi driver in order to
 *        establish a new Intra-Phi communication channel
 *
 * \param frame   the frame capability backing the bootstrap channel
 * \param xphi_id the id of the Xeon Phi we want to establish the connection to
 *
 * \return SYS_ERR_OK on success
 */
errval_t messaging_send_bootstrap(lpaddr_t base,
                                  uint8_t bits,
                                  uint8_t xphi_id,
                                  uint8_t is_client)
{
    XMESSAGING_DEBUG("Sending BOOTSTRAP message [%u] -> [%u]\n",
                     xeon_phi->id,
                     xphi_id);

    assert(xeon_phi->id != xphi_id);

    /* we need to take the host-card channel */
    struct xeon_phi_msg_data *msg = xeon_phi->msg->out;

    // in case the other side has not yet read this message, we have to wait
    while (msg->ctrl.valid != XEON_PHI_MSG_STATE_CLEAR) {
        thread_yield();
    }

    msg->data.bootstrap.base = base;
    msg->data.bootstrap.bits = bits;
    msg->data.bootstrap.xphi_id = xphi_id;
    msg->data.bootstrap.is_client = is_client;

    msg->ctrl.size = sizeof(struct xeon_phi_msg_bootstrap);
    msg->ctrl.flags.xphi_id = xeon_phi->id;
    msg->ctrl.flags.cmd = XEON_PHI_MSG_CMD_BOOTSTRAP;

    /* set the valid field to signal the other side */
    msg->ctrl.valid = XEON_PHI_MSG_STATE_VALID;

    /* update the next out pointer */
    struct xeon_phi_msg_chan *chan;
    if (xeon_phi->is_client) {
        chan = &xeon_phi->msg->meta->c2h;
    } else {
        chan = &xeon_phi->msg->meta->h2c;
    }

    msg = msg + 1;
    if (msg == (chan->data + XEON_PHI_MSG_CHANS)) {
        msg = chan->data;
    }
    xeon_phi->msg->out = msg;

    return SYS_ERR_OK;
}

/**
 * \brief sends a ready message to the other Xeon Phi card indicating
 *        that the card intra Phi bootstrap frame is ready
 *
 * \param xphi the id of the other card
 */
errval_t messaging_send_ready(uint8_t xphi)
{
    XMESSAGING_DEBUG("Sending READY message [%u] -> [%u]\n",
                     xeon_phi->id,
                     xphi);

    assert(xeon_phi->topology[xphi].msg);

    /* we need to take the host-card channel */
    struct xeon_phi_msg_data *msg = xeon_phi->topology[xphi].msg->out;

    // in case the other side has not yet read this message, we have to wait
    while (msg->ctrl.valid != XEON_PHI_MSG_STATE_CLEAR) {
        thread_yield();
    }

    msg->data.ready.xphi_id = xeon_phi->id;


    msg->ctrl.size = sizeof(struct xeon_phi_msg_ready);

    msg->ctrl.flags.cmd = XEON_PHI_MSG_CMD_READY;
    msg->ctrl.flags.xphi_id = xeon_phi->id;

    /* set the valid field to signal the other side */
    msg->ctrl.valid = XEON_PHI_MSG_STATE_VALID;

    /* update the next out pointer */
    struct xeon_phi_msg_chan *chan;
    if (xeon_phi->is_client) {
        chan = &xeon_phi->msg->meta->c2h;
    } else {
        chan = &xeon_phi->msg->meta->h2c;
    }

    msg = msg + 1;
    if (msg == (chan->data + XEON_PHI_MSG_CHANS)) {
        msg = chan->data;
    }
    xeon_phi->msg->out = msg;

    return SYS_ERR_OK;
}

static errval_t messaging_send_open_common(struct msg_info *msg_ctrl,
                                           struct capref frame,
                                           uint8_t type,
                                           char *iface)
{
    errval_t err;

    if (capref_is_null(frame)) {
        return SYS_ERR_CAP_NOT_FOUND;
    }

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    if (err_is_fail(err)) {
        return err;
    }

    XMESSAGING_DEBUG("Sending OPEN message. [0x%016lxl, 0x%lx]\n",
                     id.base,
                     1UL << id.bits);

    struct xeon_phi_msg_data *msg = msg_ctrl->out;

// in case the other side has not yet read this message, we have to wait
    while (msg->ctrl.valid != XEON_PHI_MSG_STATE_CLEAR) {
        thread_yield();
    }

    msg->ctrl.size = 10;

    msg->data.open.base = id.base;
    msg->data.open.bits = id.bits;
    msg->data.open.type = type;
    msg->ctrl.size += snprintf(msg->data.open.iface,
                               sizeof(msg->data.open.iface),
                               "%s",
                               iface);

    msg->ctrl.flags.cmd = XEON_PHI_MSG_CMD_OPEN;
    msg->ctrl.flags.xphi_id = xeon_phi->id;

    /* set the valid field to signal the other side */
    msg->ctrl.valid = XEON_PHI_MSG_STATE_VALID;

    /* update the next out pointer */
    struct xeon_phi_msg_chan *chan;
    if (msg_ctrl->is_client) {
        chan = &msg_ctrl->meta->c2h;
    } else {
        chan = &msg_ctrl->meta->h2c;
    }

    msg = msg + 1;
    if (msg == (chan->data + XEON_PHI_MSG_CHANS)) {
        msg = chan->data;
    }
    msg_ctrl->out = msg;

    return SYS_ERR_OK;
}

/**
 * \brief registers a new frame for the shared messaging channel to be used
 *        for communication purposes
 *
 * \param frame capability representing the frame to be used
 * \param type  type identifier of the channel
 * \param iface name of the exported interface
 *
 * \returns SYS_ERR_OK on success
 */
errval_t messaging_send_open(struct capref frame,
                             uint8_t type,
                             char *iface)
{
    return messaging_send_open_common(xeon_phi->msg, frame, type, iface);
}

/**
 * \brief registers a new frame for the shared messaging channel to be used
 *        for communication purposes with a specific Xeon Phi chard
 *
 * \param xphi  the id of the Xeon Phi Card to send the open command to
 * \param frame capability representing the frame to be used
 * \param type  type identifier of the channel
 * \param iface the name of the exported interface
 *
 * \returns SYS_ERR_OK on success
 */
errval_t messaging_send_open_to_xphi(uint8_t xphi,
                                     struct capref frame,
                                     uint8_t type,
                                     char *iface)
{
#ifndef __k1om__
    return messaging_send_open(frame, type, iface);
#else
    assert(xphi < XEON_PHI_NUM_MAX);
    assert(xphi < XEON_PHI_NUM_MAX);
    struct xnode *node = &xeon_phi->topology[xphi];
    if (node->state != XNODE_STATE_READY) {
        return -1;  //TODO: error code
    }
    struct msg_info *msg_ctrl = node->msg;
    return messaging_send_open_common(msg_ctrl, frame, type, iface);
#endif
}

static errval_t messaging_send_spawn_common(struct msg_info *msg_ctrl,
                                            coreid_t core,
                                            char *name)
{
    assert(name != NULL);

    XMESSAGING_DEBUG("Sending SPAWN message. [%s.%d]\n", name, core);

    struct xeon_phi_msg_data *msg = msg_ctrl->out;

// in case the other side has not yet read this message, we have to wait
    while (msg->ctrl.valid != XEON_PHI_MSG_STATE_CLEAR) {
        thread_yield();
    }

    msg->ctrl.size = 1;

    msg->data.spawn.core = core;
    msg->ctrl.size += snprintf(msg->data.spawn.name,
                               sizeof(msg->data.spawn.name),
                               "%s",
                               name);

    msg->ctrl.flags.cmd = XEON_PHI_MSG_CMD_SPAWN;
    msg->ctrl.flags.xphi_id = xeon_phi->id;

    /* set the valid field to signal the other side */
    msg->ctrl.valid = XEON_PHI_MSG_STATE_VALID;

    /* update the next out pointer */
    struct xeon_phi_msg_chan *chan;
    if (msg_ctrl->is_client) {
        chan = &msg_ctrl->meta->c2h;
    } else {
        chan = &msg_ctrl->meta->h2c;
    }

    msg = msg + 1;
    if (msg == (chan->data + XEON_PHI_MSG_CHANS)) {
        msg = chan->data;
    }
    msg_ctrl->out = msg;

    return SYS_ERR_OK;
}

/**
 * \brief sends a spawn command over the Xeon Phi channel
 *
 * \param core id of the core on which to spawn the program
 * \param name the name of the program to spawn
 *
 * \returns SYS_ERR_OK on success
 */
errval_t messaging_send_spawn(coreid_t core,
                              char *name)
{
    return messaging_send_spawn_common(xeon_phi->msg, core, name);
}

/**
 * \brief sends a spawn command over the Xeon Phi channel to
 *        a specific card.
 *
 * \param core id of the core on which to spawn the program
 * \param name the name of the program to spawn
 *
 * \returns SYS_ERR_OK on success
 */
errval_t messaging_send_spawn_to_xphi(uint8_t xphi,
                                      coreid_t core,
                                      char *name)
{
#ifndef __k1om__
    return messaging_send_spawn(core, name);
#else
    assert(xphi < XEON_PHI_NUM_MAX);
    struct xnode *node = &xeon_phi->topology[xphi];
    if (node->state != XNODE_STATE_READY) {
        return -1;  //TODO: error code
    }
    struct msg_info *msg_ctrl = node->msg;
    return messaging_send_spawn_common(msg_ctrl, core, name);
#endif
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
    XMESSAGING_DEBUG("Sending Message. [%016lx]\n", *((uint64_t * )data));
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
    msg->ctrl.flags.xphi_id = xeon_phi->id;

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

