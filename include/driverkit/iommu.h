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

/* forward declaration of the iommu client state */
struct iommu_client;


/**
 * @brief initializes the IOMMU client library with the IOMMU endpoint
 *
 * @param ep the IOMMU endpoint
 * @param cl returns a pointer ot the iommu client
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * This function initializes the connection, allocates the root vnode etc.
 */
errval_t driverkit_iommu_client_init_cl(struct capref ep, struct iommu_client **cl);
errval_t driverkit_iommu_client_init(struct capref ep);


/**
 * @brief connects to the IOMMU service
 *
 * @param ep the IOMMU endpoint
 * @param cl returns a pointer ot the iommu client
 *
 * @return SYS_ERR_OK on success, errval on failure
 *
 * This just initializes the connecton to the IOMMU
 */
errval_t driverkit_iommu_client_connect_cl(struct capref ep,
                                           struct iommu_client **cl);
errval_t driverkit_iommu_client_connect(struct capref ep);


/**
 * @brief tears down a connection to the IOMMU service
 *
 * @param cl the iommu client
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_client_disconnect_cl(struct iommu_client *cl);
errval_t driverkit_iommu_client_disconnect(void);


/**
 * @brief checks if there is an IOMMU present
 *
 * @param the pointer ot the IOMMU client state
 *
 * @return True if there is an IOMMU present
 *         False if there is no IOMMU present
 */
bool driverkit_iommu_present(struct iommu_client *cl);


/**
 * @brief sets the default iommu client to be used
 *
 * @param cl    the iommu client should be taken as default
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_set_default_client(struct iommu_client *cl);


/**
 * @brief returns the default iommu client
 *
 * @return pointer to the default iommu state
 */
struct iommu_client *driverkit_iommu_get_default_client(void);


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
errval_t driverkit_iommu_set_root_vnode(struct iommu_client *cl,
                                        struct capref rootvnode);


/**
 * @brief obtains the capability type for the root level vnode
 *
 * @return 
 */
enum objtype driverkit_iommu_get_root_vnode_type(struct iommu_client *cl);


/**
 * @brief obtains the maximu supported page size
 *
 * @return 
 */
size_t driverkit_iommu_get_max_pagesize(struct iommu_client *cl);

/**
 * @brief obtains the model node id of the  protected device
 *
 * @return the model node id
 */
int32_t driverkit_iommu_get_nodeid(struct iommu_client *cl);


/**
 * @brief maps a vnode or a frame cap into a vnode cap
 *
 * @param cl    the iommu client
 * @param dst   destination vnode to map into
 * @param src   the source capability to be mapped
 * @param slot  the slot to map into
 * @param attr  attributes for the mapping
 * @param off   offset into the frame
 * @param count number of page-table entries to be mapped
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_map(struct iommu_client *cl, struct capref dst,
                             struct capref src, uint16_t slot, uint64_t attr,
                             uint64_t off, uint64_t count);


/**
 * @brief unmaps a slot in a vnode
 *
 * @param cl    the iommu client
 * @param dst   the vnode containing the mapping
 * @param slot  the slot to be unmapped
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_unmap(struct iommu_client *cl, struct capref dst,
                               uint16_t slot);


/**
 * @brief changes the flags of the mapping
 *
 * @param cl    the iommu client
 * @param dest  the destination vnode to change the mapping
 * @param slot  the slot to change the mapping
 * @param attrs the new attributes to set
 *
 * @return SYS_ERR_OK on success, erval on failure
 */
errval_t driverkit_iommu_modify(struct iommu_client *cl, struct capref dest,
                                uint16_t slot, uint64_t attrs);


/*
 * ============================================================================
 * High-level VSpace Management Interface
 * ============================================================================
 */


///< represents a device address
typedef genpaddr_t dmem_daddr_t;

/**
 * @brief represents a region of device memory
 *
 * this region is intended to be used between the device and the driver.
 */
struct dmem
{
    ///< address as seen by the device
    dmem_daddr_t            devaddr;

    ///< address as seen by the driver
    lvaddr_t                vbase;

    ///< capability referring to the memory resource
    struct capref           mem;

    ///< size of the memory region in bytes
    gensize_t               size;

    ///< iommu client state
    struct iommu_client    *cl;
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
errval_t driverkit_iommu_vspace_map_fixed_cl(struct iommu_client *cl, struct capref frame,
                                             vregion_flags_t flags, struct dmem *dmem);
errval_t driverkit_iommu_vspace_map_cl(struct iommu_client *cl, struct capref frame,
                                       vregion_flags_t flags, struct dmem *dmem);
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
 * @brief allocates and maps a region of memory
 *
 * @param cl    the iommu client
 * @param bytes bytes to be allocated
 * @param mem   returned dmem
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_mmap_cl(struct iommu_client *cl, size_t bytes,
                                 vregion_flags_t flags, struct dmem *mem);
errval_t driverkit_iommu_mmap(size_t bytes, vregion_flags_t flags,
                              struct dmem *mem);

errval_t driverkit_iommu_munmap(struct dmem *mem);

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
 * @param cl     the iommu client to set the policy for
 * @param policy the new policy
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_set_policy(struct iommu_client *cl,
                                           iommu_vspace_policy_t policy);


/**
 * @brief sets the default iommu vspace managemet policy
 *
 * @param policy the new policy
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_vspace_set_default_policy(iommu_vspace_policy_t policy);


/*
 * ============================================================================
 * Memory Allocation
 * ============================================================================
 */


/**
 * @brief allocates a vnode for the iommu
 *
 * @param cl        the iommu client
 * @param type      vnode type to be allocated
 * @param retvnode  returned capability to the vnode
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_alloc_vnode_cl(struct iommu_client *cl,
                                        enum objtype type,
                                        struct capref *retvnode);
errval_t driverkit_iommu_alloc_vnode(enum objtype type, struct capref *retvnode);


/**
 * @brief allocates a frame to be mapped accessible by the device and the driver
 *
 * @param cl        the iommu client
 * @param bytes     number of bytes to allocate
 * @param retframe  returned frame capability
 *
 * @return SYS_ERR_OK on success, errval on failure
 */
errval_t driverkit_iommu_alloc_frame(struct iommu_client *cl, size_t bytes,
                                     struct capref *retframe);




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


static inline errval_t driverkit_iommu_client_init2(void)
{
    USER_PANIC("DEPRECATED!");
    return LIB_ERR_NOT_IMPLEMENTED;
}

#endif // DRIVERKIT_IOMMU_H
