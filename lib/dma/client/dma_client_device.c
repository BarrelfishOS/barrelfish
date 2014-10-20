/*
 * Copyright (c) 2014, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <bench/bench.h>

#include <dma/dma_manager_client.h>
#include <client/dma_client_internal.h>
#include <client/dma_client_device_internal.h>
#include <client/dma_client_channel_internal.h>
#include <client/dma_client_request_internal.h>

#include <debug.h>

/**
 * IOAT DMA device representation
 */
struct dma_client_device
{
    struct dma_device common;         ///< common part for all DMA devices
    struct dma_mgr_driver_info info;  ///< information about the device driver
};

/// counter for device ID enumeration
static dma_dev_id_t device_id = 1;

static errval_t dma_client_device_poll(struct dma_device *dev)
{
    return DMA_ERR_DEVICE_IDLE;
}

/*
 * ----------------------------------------------------------------------------
 * device initialization / termination
 * ----------------------------------------------------------------------------
 */

/**
 * \brief initializes a DMA client device with the giving capability
 *
 * \param info stores information how to find the device driver service
 * \param dev  returns a pointer to the device structure
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_client_device_init(struct dma_client_info *info,
                                struct dma_client_device **dev)
{
    errval_t err;

    struct dma_client_device *cdev = calloc(1, sizeof(*cdev));
    if (cdev == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

#if DMA_BENCH_ENABLED
     bench_init();
#endif

    struct dma_device *dma_dev = (struct dma_device *) cdev;

    CLIENTDEV_DEBUG("initialzing new client device\n", device_id);

    iref_t service_iref = 0;

    switch (info->type) {
        case DMA_CLIENT_INFO_TYPE_ADDR:
            assert(!"NYI: lookup based on physical address range");
            break;
        case DMA_CLIENT_INFO_TYPE_IREF:
            service_iref = info->args.iref;
            break;
        case DMA_CLIENT_INFO_TYPE_NAME:
            CLIENTDEV_DEBUG("looking up iref for name {%s}\n", device_id,
                            info->args.name);
            err = nameservice_blocking_lookup(info->args.name, &service_iref);
            if (err_is_fail(err)) {
                free(cdev);
                return err;
            }
            CLIENTDEV_DEBUG("driver service {%s} @ iref:%"PRIxIREF"\n", device_id,
                            info->args.name, service_iref);
            break;
        default:
            return DMA_ERR_DEVICE_UNSUPPORTED;
            break;
    }

    if (cdev->info.iref == 0) {
        err = dma_manager_lookup_by_iref(service_iref, &cdev->info);
        if (err_is_fail(err)) {
            CLIENTDEV_DEBUG("ERROR: obtaining driver info from DMA manager: %s\n",
                            device_id, err_getstring(err));
            free(cdev);
            return err;
        }
    }

    assert(service_iref != 0);
    dma_dev->type = DMA_DEV_TYPE_CLIENT;
    dma_dev->id = device_id++;
    dma_dev->channels.count = DMA_CLIENT_DEVICE_CONNECTIONS;
    dma_dev->channels.c = calloc(dma_dev->channels.count,
                                 sizeof(*dma_dev->channels.c));
    if (dma_dev->channels.c == NULL) {
        free(cdev);
        return LIB_ERR_MALLOC_FAIL;
    }

    /* channel enumeration */

    CLIENTDEV_DEBUG("doing channel enumeration. discovered %u channels\n",
                    cdev->common.id, cdev->common.channels.count);

    for (uint8_t i = 0; i < dma_dev->channels.count; ++i) {
        struct dma_channel **chan = &dma_dev->channels.c[i];
        err = dma_client_channel_init(cdev, i, service_iref,
                                      (struct dma_client_channel **) chan);
        if (err_is_fail(err)) {
            free(cdev->common.channels.c);
            free(cdev);
            return err;
        }
    }

    dma_dev->f.deregister_memory = dma_client_deregister_memory;
    dma_dev->f.register_memory = dma_client_register_memory;
    dma_dev->f.poll = dma_client_device_poll;

    *dev = cdev;

    return SYS_ERR_OK;
}

/**
 * \brief terminates the device operation and frees up the allocated resources
 *
 * \param dev IOAT DMA device to shutdown
 *
 * \returns SYS_ERR_OK on success
 *          errval on error
 */
errval_t dma_client_device_shutdown(struct dma_client_device *dev)
{
    assert(!"NYI");
    return SYS_ERR_OK;
}

/*
 * ----------------------------------------------------------------------------
 * Device Status Queries
 * ----------------------------------------------------------------------------
 */

dma_dev_type_t dma_client_get_device_type(struct dma_client_device *dev)
{
    assert(dev->info.type != DMA_DEV_TYPE_CLIENT);
    return dev->info.type;
}

/**
 * \brief returns the supported range of a connection
 *
 * \param conn      DMA client connection
 * \param mem_low   minimum physical address supported
 * \param mem_high  maximum physical address supported
 */
void dma_client_device_get_mem_range(struct dma_client_device *dev,
                                     lpaddr_t *mem_low,
                                     lpaddr_t *mem_high)
{
    if (mem_low) {
        *mem_low = dev->info.mem_low;
    }
    if (mem_high) {
        *mem_high = dev->info.mem_high;
    }
}

