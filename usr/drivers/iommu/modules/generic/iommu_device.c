/**
 * \file
 * \brief IOMMU Devices
 */
/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */

#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <skb/skb.h>
#include <numa.h>

#include "common.h"
#include "../intel_vtd/intel_vtd.h"

///< stores the devices
static struct iommu_device **iommu_devices[IOMMU_BUS_MAX];


static errval_t device_put_by_pci(struct iommu_device *dev)
{
    assert(dev->addr.pci.segment < IOMMU_SEGMENTS_MAX);

    if (iommu_devices[dev->addr.pci.bus] == NULL) {
        iommu_devices[dev->addr.pci.bus] = calloc(IOMMU_DEVFUN_MAX,
                                                  sizeof(void *));
        if (iommu_devices[dev->addr.pci.bus] == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
    }

    uint8_t idx = iommu_devfn_to_idx(dev->addr.pci.device,
                                     dev->addr.pci.function);
    if (iommu_devices[dev->addr.pci.bus][idx]) {
        return IOMMU_ERR_DEV_USED;
    }

    iommu_devices[dev->addr.pci.bus][idx] = dev;

    return SYS_ERR_OK;
}

static struct iommu_device *device_get_by_pci(uint16_t seg, uint8_t bus,
                                              uint8_t dev, uint8_t fun)
{
    assert(seg < IOMMU_SEGMENTS_MAX);
    if (iommu_devices[bus] == NULL) {
        return NULL;
    }
    return iommu_devices[bus][iommu_devfn_to_idx(dev, fun)];
}



errval_t iommu_device_create_by_pci(struct iommu *iommu, uint16_t seg,
                                    uint8_t bus, uint8_t dev, uint8_t fun,
                                    struct iommu_device **iodev)
{
    errval_t err;

    debug_printf("[iommu] create device by pci %u.%u.%u\n", bus, dev, fun);

    struct iommu_device *device;

    /*
     * check if device already exists
     */

    device = device_get_by_pci(seg, bus, dev, fun);
    if (device) {
        /* XXX: should not create the same device 2*/
        debug_printf("XXX: created the same device twice: %u.%u.%u.%u\n",
                     seg, bus, dev, fun);

        assert(!device);
        return SYS_ERR_OK;
    }

    debug_printf("[iommu] creating hw specific device.\n");

    /*
     * call iommu specific device creation function
     */
    assert(iommu->f.create_device);
    err = iommu->f.create_device(iommu, seg, bus, dev, fun, &device);
    if (err_is_fail(err)) {
        return err;
    }

    *iodev = device;

    return device_put_by_pci(*iodev);
}


errval_t iommu_device_destroy(struct iommu_device *iodev)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}




errval_t iommu_device_lookup_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                    uint8_t fun, struct iommu_device **rdev)
{
    debug_printf("[iommu] lookup device by pci %u.%u.%u\n", bus, dev, fun);

    struct iommu_device *d = device_get_by_pci(seg, bus, dev, fun);
    if (d == NULL) {
        return IOMMU_ERR_DEV_NOT_FOUND;
    }

    if (rdev) {
        *rdev = d;
    }

    return SYS_ERR_OK;
}





//////////////// TODO Implement

errval_t iommu_device_create_by_address(struct iommu *iommu, uint64_t addr,
                                        struct iommu_device **iodev)
{
    USER_PANIC("[iommu] create by address NYI\n");

    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t iommu_device_lookup_iommu_by_address(uint64_t address,
                                              struct iommu ** iommu)
{
    USER_PANIC("[iommu] lookup iommu by address NYI\n");

    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t iommu_device_lookup_by_address(uint64_t address,
                                        struct iommu_device **rdev)
{
    USER_PANIC("[iommu] lookup by address NYI\n");

    return LIB_ERR_NOT_IMPLEMENTED;
}
