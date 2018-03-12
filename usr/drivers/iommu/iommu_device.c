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
#include "modules/intel_vtd/intel_vtd.h"

static struct iommu_device **iommu_devices[IOMMU_BUS_MAX];



static errval_t device_put(struct iommu_device *dev)
{
    if (!(dev->id.segment < IOMMU_SEGMENTS_MAX)) {
        debug_printf("dev->id.segment < IOMMU_SEGMENTS_MAX   %u\n", dev->id.segment);
    }
    assert(dev->id.segment < IOMMU_SEGMENTS_MAX);
    if (iommu_devices[dev->id.bus] == NULL) {
        iommu_devices[dev->id.bus] = calloc(IOMMU_DEVFUN_MAX, sizeof(void *));
        if (iommu_devices[dev->id.bus] == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
    }

    uint8_t idx = iommu_devfn_to_idx(dev->id.device, dev->id.function);
    if (iommu_devices[dev->id.bus][idx]) {
        return IOMMU_ERR_DEV_USED;
    }

    iommu_devices[dev->id.bus][idx] = dev;
    return SYS_ERR_OK;
}

static struct iommu_device *device_get(uint16_t seg, uint8_t bus, uint8_t dev,
                                                 uint8_t fun)
{
    assert(seg < IOMMU_SEGMENTS_MAX);
    if (iommu_devices[bus] == NULL) {
        return NULL;
    }
    return iommu_devices[bus][iommu_devfn_to_idx(dev, fun)];
}


static errval_t iommu_device_create_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                           uint8_t fun, struct iommu_device **iodev)
{
    errval_t err;

    debug_printf("[iommu] create device by pci %u.%u.%u\n", bus, dev, fun);

    struct iommu_device *device;

    device = device_get(seg, bus, dev, fun);
    if (device) {
        /* XXX: should not create the same device 2*/
        debug_printf("XXX: created the same device twice: %u.%u.%u.%u\n",
                     seg, bus, dev, fun);

        assert(!device);
        return SYS_ERR_OK;
    }

    debug_printf("[iommu] looking up iommu for device %u %u.%u.%u\n", seg, bus, dev, fun);


    struct iommu *iommu;
    err = iommu_device_lookup_iommu_by_pci(seg, bus, dev, fun, &iommu);
    if (err_is_fail(err)) {
        debug_printf("failed to lookup device for IOMMU %s\n", err_getstring(err));
        return IOMMU_ERR_IOMMU_NOT_FOUND;
    }

    debug_printf("[iommu] creating hw specific device\n");

    switch(iommu->type) {
        case HW_PCI_IOMMU_INTEL:
            debug_printf("[iommu] creating vtd device %u %u.%u.%u\n", seg, bus, dev, fun);
            err = vtd_device_create_by_pci(seg, bus, dev, fun, (struct vtd*)iommu,
                                    (struct vtd_device **)&device);
            break;
        case HW_PCI_IOMMU_AMD:
        case HW_PCI_IOMMU_ARM:
            return LIB_ERR_NOT_IMPLEMENTED;
        default:
            return IOMMU_ERR_IOMMU_NOT_FOUND;
    }

    if (err_is_fail(err)) {
        return err;
    }

    device->iommu       = iommu;
    device->id.segment  = seg;
    device->id.bus      = bus;
    device->id.device   = dev;
    device->id.function = fun;
    /* TODO: device->id.type     = 0; */

    *iodev = device;

    return device_put(*iodev);
}


errval_t iommu_device_create(struct capref dev, struct iommu_device **iodev)
{
    errval_t err;

    struct device_identity id;
    err = invoke_device_identify(dev, &id);
    if (err_is_fail(err)) {
        return err;
    }

    return iommu_device_create_by_pci(id.segment, id.bus, id.device,
                                      id.function, iodev);
}


errval_t iommu_device_destroy(struct iommu_device *iodev)
{
    USER_PANIC("NYI");
    return SYS_ERR_OK;
}


errval_t iommu_device_lookup_iommu_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                       uint8_t fun, struct iommu **iommu)
{
    /* find the device scope */
    errval_t err;
    uint32_t idx, type;

    debug_printf("[iommu] look-up iommu by device scope information\n");

    err = skb_execute_query("iommu_device(T,I, _, _, addr(%" PRIu16 ", "
                            "%" PRIu8 ", %" PRIu8 ", %" PRIu8 "), _),"
                            "write(u(T,I)).", seg, bus, dev, fun);
    if (err_is_ok(err)) {
        err = skb_read_output("u(%d,%d)", &type, &idx);
        debug_printf("[iommu] found device at iommu with idx %u\n", idx);

        assert(err_is_ok(err));
        return iommu_get_by_idx(type, idx, iommu);
    }

    debug_printf("[iommu] look-up iommu by PCI segment with all flags\n");
    err = skb_execute_query("iommu(T,I,1, %" PRIu16 "),write(u(T,I,1)).", seg);
    if (err_is_fail(err)) {
        debug_printf("[iommu] look-up iommu by PCI segment\n");
        err = skb_execute_query("iommu(T,I,F, %" PRIu16 "),write(u(T,I,F)).", seg);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to obtain iommu for PCI segment");
            return IOMMU_ERR_IOMMU_NOT_FOUND;
        }
    }

    uint32_t flags;
    err = skb_read_output("u(%d,%d,%d)", &type, &idx, &flags);
    assert(err_is_ok(err));

    err = iommu_get_by_idx(type, idx, iommu);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to get the device\n");
    }
    debug_printf("iommu_device_lookup_iommu_by_pci succeeded!\n");
    return SYS_ERR_OK;
}


errval_t iommu_device_lookup_iommu(struct capref dev, struct iommu **iommu)
{
    errval_t err;

    struct device_identity id;
    err = invoke_device_identify(dev, &id);
    if (err_is_fail(err)) {
        return err;
    }
    debug_printf("[iommu] looking up device byu cap\n");
    return iommu_device_lookup_iommu_by_pci(id.segment, id.bus, id.device,
                                            id.function, iommu);
}


errval_t iommu_device_lookup_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                    uint8_t fun, struct iommu_device **rdev)
{
    debug_printf("[iommu] lookup device by pci %u.%u.%u\n", bus, dev, fun);

    struct iommu_device *d = device_get(seg, bus, dev, fun);
    if (d == NULL) {
        return IOMMU_ERR_DEV_NOT_FOUND;
    }

    if (rdev) {
        *rdev = d;
    }

    return SYS_ERR_OK;
}


errval_t iommu_device_lookup(struct capref dev, struct iommu_device **rdev)
{
    errval_t err;

    struct device_identity id;
    err = invoke_device_identify(dev, &id);
    if (err_is_fail(err)) {
        return IOMMU_ERR_DEV_NOT_FOUND;
    }

    return iommu_device_lookup_by_pci(id.segment, id.bus, id.device,
                                      id.function, rdev);
}


errval_t iommu_device_get_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                 uint8_t fun, struct iommu_device **rdev)
{
    errval_t err;

    debug_printf("[iommu] get device by pci %u.%u.%u\n", bus, dev, fun);

    err = iommu_device_lookup_by_pci(seg, bus, dev, fun, rdev);
    if (err_is_ok(err)) {
        return SYS_ERR_OK;
    }

    debug_printf("[iommu] lookup failed, creating device %u.%u.%u\n", bus,
                 dev, fun);

    return iommu_device_create_by_pci(seg, bus, dev, fun, rdev);


}

errval_t iommu_device_get(struct capref dev, struct iommu_device **rdev)
{
    errval_t err;

    debug_printf("[iommu] get by cap\n");

    struct device_identity id;
    err = invoke_device_identify(dev, &id);
    if (err_is_fail(err)) {
        return err;
    }
    return iommu_device_get_by_pci(id.segment, id.bus, id.device, id.function,
                                   rdev);
}

