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

#define IOMMU_MAX 4

static struct iommu *iommus[IOMMU_MAX] = {0};

errval_t iommu_get_by_idx(hw_pci_iommu_t type, uint32_t idx, struct iommu **iommu)
{
    switch(type) {
        case HW_PCI_IOMMU_INTEL:
            debug_printf("[iommu] get intel vtd with index %u\n", idx);
            if (idx < IOMMU_MAX) {
                *iommu = iommus[idx];
                return iommus[idx] ? SYS_ERR_OK : IOMMU_ERR_IOMMU_NOT_FOUND;
            }
            return IOMMU_ERR_IOMMU_NOT_FOUND;
        case HW_PCI_IOMMU_AMD:
        case HW_PCI_IOMMU_ARM:
            debug_printf("[iommu] ARM and AMD not supported at the moment\n");
            return IOMMU_ERR_IOMMU_NOT_FOUND;
        default:
            return IOMMU_ERR_IOMMU_NOT_FOUND;
    }
}

errval_t iommu_set_by_idx(hw_pci_iommu_t type, uint32_t idx, struct iommu *iommu)
{
    assert(idx < IOMMU_MAX);
    if (type != HW_PCI_IOMMU_INTEL) {
        return LIB_ERR_NOT_IMPLEMENTED;
    }
    iommus[idx] = iommu;
    return SYS_ERR_OK;
}