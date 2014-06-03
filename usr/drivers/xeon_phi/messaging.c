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

#include <xeon_phi/xeon_phi.h>
#include <xeon_phi/xeon_phi_messaging.h>

#include "xeon_phi.h"
#include "messaging.h"


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

    XMESSAGING_DEBUG("Messaging Base = %016lx\n", mi->base);
    void *addr;
    err = vspace_map_one_frame(&addr, mi->size, mi->frame, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    mi->meta = addr;

    if (capref_is_null(frame)) {
        memset(addr, 0, mi->size);
    }

    phi->msg = mi;

    if (!phi->is_client) {
        struct xeon_phi_boot_params *bp;
        bp = (struct xeon_phi_boot_params *)(phi->apt.vbase + phi->os_offset);
        bp->msg_base = mi->base;
    }

    return SYS_ERR_OK;
}

/**
 * \brief handles the event when a new channel is added between the host
 *        and the card
 */
static errval_t handle_messaging_change(struct xeon_phi *phi,
                                        struct xeon_phi_msg_meta *meta,
                                        uint32_t channel)
{
    struct xeon_phi_msg_chan *chan = &meta->chan[channel];
    if (phi->is_client) {
        struct xeon_phi_msg_chan *cnew = chan+1;
        memcpy(cnew, chan, sizeof(*chan));
        cnew->base = 0xcafebabe;

        meta->changed[channel] = 0x1;
        meta->meta_changed |= XEON_PHI_MSG_META_NEW;
    }

    XMESSAGING_DEBUG("Messaging Base = %016lx\n", 123UL);

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
    if (phi->msg == NULL) {
        return -1;
    }

    // check if there is a new channel information available
    struct xeon_phi_msg_meta *meta = phi->msg->meta;
    if (meta->meta_changed) {
        if (phi->is_client) {
            if (meta->meta_changed & XEON_PHI_MSG_META_CARD) {
                // we are the card and we have changed, just return
                return SYS_ERR_OK;
            }
        } else {
            if (meta->meta_changed & XEON_PHI_MSG_META_HOST) {
                // we are the host and we have changed, just return
                return SYS_ERR_OK;
            }
        }

        for (uint32_t i = 0; i < XEON_PHI_MSG_CHANS; ++i) {
            if (meta->changed[i] == XEON_PHI_MSG_META_CHANGE) {
                handle_messaging_change(phi, meta, i);
                meta->changed[i] = 0x00;
            } else if (meta->changed[i] == XEON_PHI_MSG_META_NEW) {
                meta->changed[i] = XEON_PHI_MSG_META_CHANGE;
            }
        }
        if (meta->meta_changed & XEON_PHI_MSG_META_NEW) {
            if (phi->is_client) {
                meta->meta_changed = XEON_PHI_MSG_META_CARD;
            } else {
                meta->meta_changed = XEON_PHI_MSG_META_HOST;
            }
        } else {
            meta->meta_changed = 0x00;
        }
    }

    return SYS_ERR_OK;
}


/**
 * \brief registers a new frame for the shared messaging channel to be used
 *        for communication purposes
 *
 * \param phi   the card to initialize the messaging for
 * \param frame capabilitz representing the frame to be used
 */
errval_t messaging_channel_open(struct xeon_phi *phi,
                                struct capref frame)
{
    return SYS_ERR_OK;
}
