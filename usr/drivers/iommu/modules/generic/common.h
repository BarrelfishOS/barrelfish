/**
 * \file
 * \brief IOMMU Driver
 */
/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOMMU_COMMON_H_
#define IOMMU_COMMON_H_ 1

#include <hw_records.h>

#define IOMMU_DEBUG(x...)

errval_t iommu_service_init(void);

#define SKB_SCHEMA_IOMMU_DEVICE \
    "iommu_device(%" PRIu32 ", %" PRIu32 ", %" PRIu8 ", %" PRIu8 ", "\
                  "addr(%" PRIu16 ", %" PRIu8 ", %" PRIu8 ", %" PRIu8 "), "\
                  "%" PRIu8  ")."

/*
 * iommus
 */


struct iommu
{
    hw_pci_iommu_t type;
};

errval_t iommu_get_by_idx(hw_pci_iommu_t type, uint32_t idx, struct iommu **iommu);
errval_t iommu_set_by_idx(hw_pci_iommu_t type, uint32_t idx, struct iommu *iommu);


static inline hw_pci_iommu_t iommu_get_type(struct iommu *i)
{
    return i->type;
}


/*
 * devices
 */

#define IOMMU_SEGMENTS_MAX 1
#define IOMMU_BUS_MAX 256
#define IOMMU_DEVFUN_MAX 256

#define iommu_idx_to_dev(idx) (idx >> 3)
#define iommu_idx_to_fun(idx) (idx & 0x7)
#define iommu_devfn_to_idx(dev, fun) ((uint8_t)((dev << 3) | fun))

/**
 * @brief represents a device in the IOMMU context
 */
struct iommu_device
{
    ///< the iommu responsible for this device
    struct iommu           *iommu;

    ///< the iommu binding
    struct iommu_binding   *binding;

    ///< the mapping cap for this device
    struct capref           mappingcap;

    ///< the device identity
    struct device_identity  id;
};

#include <hw_records.h>

errval_t iommu_device_create(struct capref dev, struct iommu_device **iodev);
errval_t iommu_device_destroy(struct iommu_device *iodev);

errval_t iommu_device_lookup(struct capref dev, struct iommu_device **rdev);
errval_t iommu_device_lookup_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                    uint8_t fun, struct iommu_device **rdev);

errval_t iommu_device_get(struct capref dev, struct iommu_device **rdev);
errval_t iommu_device_get_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                    uint8_t fun, struct iommu_device **rdev);

errval_t iommu_device_lookup_iommu(struct capref dev, struct iommu ** iommu);
errval_t iommu_device_lookup_iommu_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                          uint8_t fun, struct iommu ** iommu);

static inline struct iommu *iommu_device_get_iommu(struct iommu_device *d)
{
    return d->iommu;
}



errval_t iommu_service_new_endpoint(struct capref ep, struct iommu_device *dev,
                                    idc_endpoint_t type);

#endif //IOMMU_COMMON_H_
