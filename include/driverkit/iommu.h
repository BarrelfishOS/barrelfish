/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH8092 Zurich.
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
errval_t driverkit_iommu_client_init(void);
errval_t driverkit_iommu_client_init_with_endpoint(struct capref);

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
static inline errval_t driverkit_iommu_create_domain(struct capref rootpt, struct capref dev)
{
USER_PANIC("DEPRECATED!");
return LIB_ERR_NOT_IMPLEMENTED;
}
/**
 * @brief deletes a previously created protection domain
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
static inline errval_t driverkit_iommu_delete_domain(struct capref rootpt)
{
USER_PANIC("DEPRECATED!");
return LIB_ERR_NOT_IMPLEMENTED;
}


/**
 * @brief adds a device to a protection domain
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
static inline errval_t driverkit_iommu_add_device(struct capref rootpt, struct capref dev)
{
    USER_PANIC("DEPRECATED!");
    return LIB_ERR_NOT_IMPLEMENTED;
}
/**
 * @brief removes a device from a protection domain
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
static inline errval_t driverkit_iommu_remove_device(struct capref rootpt, struct capref dev)
{
    USER_PANIC("DEPRECATED!");
    return LIB_ERR_NOT_IMPLEMENTED;
}


errval_t driverkit_iommu_set_root(struct capref rootvnode);

/* memory allocator for the frames or vnodes */
errval_t driverkit_iommu_alloc_frame(size_t bytes, struct capref *retframe);
errval_t driverkit_iommu_alloc_vnode(enum objtype type, struct capref *retvnode);

/* mapping of*/
errval_t driverkit_iommu_map(struct capref dst, uint16_t *slot, struct capref src);
errval_t driverkit_iommu_unmap(struct capref dst, uint16_t slot);
errval_t driverkit_iommu_modify(struct capref dest, uint16_t slot, uint64_t attrs);



#endif // DRIVERKIT_IOMMU_H
