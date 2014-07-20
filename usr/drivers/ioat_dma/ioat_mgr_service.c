/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>

#include "ioat_mgr_service.h"

struct ioat_dev_handle
{
    struct capref devframe;
    uint8_t in_use;
    struct ioat_dma_mgr_binding *binding;
    struct ioat_dev_handle *next;
};

static uint8_t device_count = 0;

static uint8_t avail_count = 0;

static struct ioat_dev_handle *avail_devices = NULL;

static struct ioat_dev_handle *used_devices = NULL;

errval_t ioat_mgr_svc_init(void)
{
    return SYS_ERR_OK;
}

errval_t ioat_mgr_svc_add_device(struct capref *frame)
{
    struct ioat_dev_handle *dev = calloc(1, sizeof(*dev));
    if (dev == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    dev->devframe = *frame;

    dev->next = avail_devices;
    avail_devices = dev;

    avail_count++;
    device_count++;

    return SYS_ERR_OK;
}

errval_t ioat_mgr_svc_acquire(struct ioat_dma_mgr_binding *binding,
                              struct ioat_dev_handle **handle)
{
    if (avail_count == 0) {
        assert(avail_devices == NULL);
        return -1; // TODO: error number
    }
    struct ioat_dev_handle *dev = avail_devices;
    avail_devices = dev->next;

    dev->next = used_devices;
    used_devices = dev;

    assert(dev->in_use == 0);

    avail_count--;

    dev->binding = binding;
    dev->in_use = 1;

    *handle = dev;
    return SYS_ERR_OK;
}

errval_t ioat_mgr_svc_release(struct ioat_dev_handle *handle)
{
    handle->next = avail_devices;
    avail_devices = handle;
    avail_count++;
    handle->binding = NULL;
    handle->in_use = 0;
    return SYS_ERR_OK;
}



