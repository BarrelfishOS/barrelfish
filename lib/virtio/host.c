/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>

#include <barrelfish/barrelfish.h>

#include <virtio/virtio.h>
#include <virtio/virtqueue.h>
#include <virtio/virtqueue_host.h>
#include <virtio/virtio_device.h>
#include <virtio/virtio_host.h>

#include "device.h"

#include "backends/virtio_mmio.h"
#include "backends/virtio_pci.h"

#include "host/channel.h"


static lpaddr_t paddr_offset = 0x0;

/**
 *
 */

/**
 *
 */
errval_t virtio_host_init(struct virtio_device **host,
                          struct virtio_device_setup *setup)
{
    errval_t err = SYS_ERR_OK;

    switch(setup->backend.type) {
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

    struct virtio_device *vdev = *host;

    strncpy(vdev->dev_name, setup->dev_name, sizeof(vdev->dev_name));
    vdev->dev_t_st = setup->dev_t_st;
    strncpy(vdev->hc_iface, setup->hc_iface, sizeof(vdev->hc_iface));

    vdev->cb_h = setup->hc_cb;

    vdev->vq_num = setup->vq_num;

    setup->vq_setup->device = vdev;

    err = virtio_vq_host_alloc(&vdev->vqh, setup->vq_setup, setup->vq_num);
    assert(err_is_ok(err));

    switch (setup->hc_type) {
    case VIRTIO_HOST_CHAN_FLOUNDER:
        err = virtio_host_flounder_init(vdev);
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

errval_t virtio_host_poll_device(struct virtio_device *host)
{
    errval_t err;
    if (host->f->poll) {
        err = host->f->poll(host);
        if (err_is_fail(err)) {
            return err;
        }
    } else {
        return VIRTIO_ERR_BACKEND;
    }
    return virtio_vq_host_poll(host->vqh, host->vq_num);

}

errval_t virtio_host_get_device_cap(struct virtio_device *host,
                                    struct capref *ret_cap)
{
    if(ret_cap) {
        *ret_cap = host->dev_cap;
    }
    return SYS_ERR_OK;
}

lpaddr_t virtio_host_translate_host_addr(lpaddr_t host_phys)
{
    lpaddr_t guest_addr = (host_phys - paddr_offset);
    assert(guest_addr <=  host_phys);
    return guest_addr;
}

lpaddr_t virtio_host_translate_guest_addr(lpaddr_t guest_phys)
{
    lpaddr_t host_addr = (guest_phys + paddr_offset);
    assert(guest_phys <=  host_addr);
    return host_addr;
}

