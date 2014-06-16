/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include <virtio/virtio.h>
#include <virtio/virtio_device.h>
#include <virtio/virtio_host.h>

#include "device.h"

#include "backends/virtio_mmio.h"
#include "backends/virtio_pci.h"

#include "host/channel.h"


/**
 *
 */
/**
 *
 */
errval_t virtio_host_init(struct virtio_host **host,
                          struct virtio_host_setup *setup)
{
    errval_t err = SYS_ERR_OK;

    switch(setup->backend) {
    case  VIRTIO_DEVICE_BACKEND_PCI:
        assert(!"NYI: PCI backend");
    break;

    case VIRTIO_DEVICE_BACKEND_MMIO:
        err = virtio_device_mmio_init_host(host, setup);
        break;
    case VIRTIO_DEVICE_BACKEND_IO:
        assert(!"NYI: IO backend");
        break;
    default:
        err = VIRTIO_ERR_ARG_INVALID;
        break;
    }

    if (err_is_fail(err)) {
        return err;
    }



    switch (setup->channel_type) {
    case VIRTIO_HOST_CHAN_FLOUNDER:
        err = virtio_host_flounder_init(setup->iface, setup->callback);
        break;

    case VIRTIO_HOST_CHAN_XEON_PHI:
        err = virtio_host_xeon_phi_init();
        break;
    default:
        err = -1;
        break;
    }

    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

errval_t virtio_host_poll_device(struct virtio_host *host)
{
    if (host->poll) {
        return host->poll(host);
    }
    return VIRTIO_ERR_BACKEND;
}

errval_t virtio_host_get_device_cap(struct virtio_host *host,
                                    struct capref *ret_cap)
{
    if(ret_cap) {
        *ret_cap = host->dev_frame;
    }
    return SYS_ERR_OK;
}
