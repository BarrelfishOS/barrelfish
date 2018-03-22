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


/*
 * ============================================================================
 * Low-level interface
 * ============================================================================
 */


/**
 * @brief sets the root table pointer of the IOMMU
 *
 * @param rootvnode the root page table (vnode)
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_set_root(struct capref rootvnode);


/**
 * @brief obtains the capability type for the root level vnode
 *
 * @param type returned capability type
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_get_root_vnode_type(enum objtype *type);


/**
 * @brief obtains the maximu supported page size
 *
 * @param pgsize  the maximum supported page size
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_get_max_pagesize(size_t *pgsize);


/**
 * @brief allocates a vnode for the iommu
 *
 * @param type      vnode type to be allocated
 * @param retvnode  returned capability to the vnode
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_alloc_vnode(enum objtype type, struct capref *retvnode);


/**
 * @brief maps a vnode or a frame cap into a vnode cap
 *
 * @param dst   destination vnode to map into
 * @param slot  the slot to map into
 * @param src   the source capability to be mapped
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_map(struct capref dst, uint16_t *slot, struct capref src);


/**
 * @brief unmaps a slot in a vnode
 *
 * @param dst   the vnode containing the mapping
 * @param slot  the slot to be unmapped
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_unmap(struct capref dst, uint16_t slot);


/**
 * @brief changes the flags of the mapping
 *
 * @param dest  the destination vnode to change the mapping
 * @param slot  the slot to change the mapping
 * @param attrs the new attributes to set
 *
 * @return SYS_ERR_OK on success, erval on failure
 */
errval_t driverkit_iommu_modify(struct capref dest, uint16_t slot, uint64_t attrs);


/*
 * ============================================================================
 * High-level Interface
 * ============================================================================
 */


///< represents a device address
typedef lpaddr_t dmem_daddr_t;

/**
 * @brief represents a region of device memory
 *
 * this region is intended to be used between the device and the driver.
 */
struct dmem
{
    ///< address as seen by the device
    dmem_daddr_t    devaddr;

    ///< address as seen by the driver
    void           *vbase;

    ///< capability referring to the memory resource
    struct capref   mem;

    ///< size of the memory region in bytes
    gensize_t       size;
};


/**
 * @brief maps a frame in the device and driver space
 *
 * @param frame the frame to be mapped
 * @param flags attributes for the mapping
 * @param dmem  the device memory struct
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_map(struct capref frame, vregion_flags_t flags,
                                    struct dmem *dmem);


/**
 * @brief unmaps a previoiusly mapped device memory region
 *
 * @param dmem  the device memory region
 *
 * @return SYS_ERR_OK on succes, errval on failure
 */
errval_t driverkit_iommu_vspace_unmap(struct dmem *dmem);

/**
 * @brief modifies an existing mapping
 *
 * @param dmem  the device mem region
 * @param flags new attributes for the mapping
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_modify_flags(struct dmem *dmem,
                                             vregion_flags_t flags);


/**
 * @brief represents an iommu vspace management policy
 */
typedef enum {
    IOMMU_VSPACE_POLICY_MIRROR,
    IOMMU_VSPACE_POLICY_SHARED,
    IOMMU_VSPACE_POLICY_INDEPENDENT
} iommu_vspace_policy_t;


/**
 * @brief sets the iommu vspace managemet policy
 *
 * @param policy the new policy
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_set_policy(iommu_vspace_policy_t policy);


/*
 * ============================================================================
 * Memory Allocation
 * ============================================================================
 */


/**
 * @brief allocates a frame to be mapped accessible by the device and the driver
 *
 * @param bytes     number of bytes to allocate
 * @param retframe  returned frame capability
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_alloc_frame(size_t bytes, struct capref *retframe);


/**
 * @brief allocates and maps a region of memory
 *
 * @param bytes bytes to be allocated
 * @param mem   returned dmem
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_mmap(size_t bytes, struct dmem *mem);


/*
 * ============================================================================
 * IOMMU aware heap style memory allocation
 * ============================================================================
 */


/**
 * @brief allocates memory on the device heap
 *
 * @param bytes number of bytes to allocate
 *
 * @return pointer to the allocate region
 *
 * the returned pointer is valid on the CPU and the device
 * This does not work with the IOMMU_VSPACE_POLICY_INDEPENDENT.
 */
void *driverkit_iommu_malloc(size_t bytes);


/**
 * @brief allocates and zeroesa a region on the device heap
 * @param blocks    number of blocks to be allocated
 * @param bytes     size of a block
 *
 * @return pointer to the allocated regoin
 *
 * the returned pointer is valid on the CPU and the device
 * This does not work with the IOMMU_VSPACE_POLICY_INDEPENDENT.
 */
void *driverkit_iommu_calloc(size_t blocks, size_t bytes);


/**
 * @brief frees a region on the devic heap
 *
 * @param ptr pointer ot the region to be freed
 *
 * the returned pointer is valid on the CPU and the device
 * This does not work with the IOMMU_VSPACE_POLICY_INDEPENDENT.
 */
void driverkit_iommu_free(void *ptr);


#endif // DRIVERKIT_IOMMU_H
