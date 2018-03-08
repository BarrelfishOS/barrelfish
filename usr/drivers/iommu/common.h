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

errval_t iommu_service_init(void);

/*
 * iommus
 */
struct iommu
{

};

/*
 * devices
 */

/**
 * @brief represents a device in the IOMMU context
 */
struct iommu_dev
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

errval_t iommu_device_create(struct capref dev, struct iommu_dev *iodev);
errval_t iommu_device_destroy(struct iommu_dev *iodev);

errval_t iommu_device_lookup(struct capref dev, struct iommu_dev **rdev);
errval_t iommu_device_lookup_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                    uint8_t fun, struct iommu_dev **rdev);


errval_t iommu_device_find_iommu(struct capref dev, struct iommu **iommu);
errval_t iommu_device_find_iommu_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                       uint8_t fun, struct iommu **iommu);



#endif //IOMMU_COMMON_H_
