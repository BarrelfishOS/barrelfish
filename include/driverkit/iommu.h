/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */

#ifndef DRIVERKIT_IOMMU_H
#define DRIVERKIT_IOMMU_H 1

#include <barrelfish/types.h>
#include <errors/errno.h>

#define DRIVERKIT_IOMMU_SERVICE_NAME "iommu_svc"

/**
 * @brief initializes the IOMMU client library
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_service_init(void);

/**
 * @brief initializes the IOMMU client library
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_client_init(void);


/**
 * @brief checks if there is an IOMMU present
 *
 * @return True if there is an IOMMU present
 *         False if there is no IOMMU present
 */
bool driverkit_iommu_present(void);



/**
 * @brief creates a new protection domain
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_create_domain(void);

/**
 * @brief deletes a previously created protection domain
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_delete_domain(void);



/**
 * @brief adds a device to a protection domain
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_add_device(void);

/**
 * @brief removes a device from a protection domain
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_remove_device(void);




#endif // DRIVERKIT_IOMMU_H
