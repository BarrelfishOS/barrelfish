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
errval_t messaging_init(struct xeon_phi *phi)
{
    errval_t err;

    assert(phi->msg == NULL);

    struct msg_info *mi = malloc(sizeof(struct msg_info));
    if (mi == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }


    err = frame_alloc(&mi->frame, 4096, &mi->size);
    if (err_is_fail(err)) {
        return err;
    }

    struct frame_identity id;
    err = invoke_frame_identify(mi->frame, &id);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    mi->base = id.base;

    err = vspace_map_one_frame(&mi->addr, mi->size, mi->frame, NULL, NULL);
    if (err_is_fail(err)) {
        cap_destroy(mi->frame);
        free(mi);
        return err;
    }

    memset(mi->addr, 0, mi->size);

    uint32_t *test = mi->addr;
    *test = 0x12345678;

    phi->msg = mi;

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

    uint32_t *test = phi->msg->addr;
    assert(*test == 0x12345678);
    test++;
    if (*test) {
        debug_printf("got message: %x\n", *test);
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
errval_t messaging_register(struct xeon_phi *phi,
                            struct capref frame)
{
    return SYS_ERR_OK;
}
