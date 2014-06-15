/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <virtio/virtio_guest.h>

#include "guest/channel.h"

struct virtio_guest_chan_fn *vguest_chan_fn = NULL;

/**
 *
 */
errval_t virtio_guest_init(enum virtio_guest_channel backend,
                           char *iface)
{
    errval_t err;

    switch (backend) {
    case VIRTIO_GUEST_CHAN_FLOUNDER:
        err = virtio_guest_flounder_init(iface);
        break;

    case VIRTIO_GUEST_CHAN_XEON_PHI:
        err = virtio_guest_xeon_phi_init();
        break;
    default:
        err = -1;
        break;
    }

    if (err_is_fail(err)) {
        return err;
    }

    assert(vguest_chan_fn);

    return SYS_ERR_OK;
}
