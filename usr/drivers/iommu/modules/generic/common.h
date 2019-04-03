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
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOMMU_COMMON_H_
#define IOMMU_COMMON_H_ 1

#include <hw_records.h>

#define IOMMU_DEBUG(x...)

errval_t iommu_service_init(void);

struct iommu;
struct iommu_device;

/*
 * iommus
 */

typedef errval_t (*map_fn)(struct iommu_device *io, struct capref dest, struct capref src, capaddr_t slot,
                           uint64_t attr, uint64_t off, uint64_t pte_count, struct capref mapping);
typedef errval_t (*unmap_fn)(struct iommu_device *io, struct capref vnode, uint16_t slot);

typedef errval_t (*set_root_fn)(struct iommu_device *, struct capref src);
typedef errval_t (*create_dev_fn)(struct iommu *io, uint16_t seg, uint8_t bus,
                                  uint8_t dev, uint8_t fun,
                                  struct iommu_device **d);

/**
 * @brief represents a generic iommu
 */
struct iommu
{
    hw_pci_iommu_t type;
    uint32_t id;
    struct {
        create_dev_fn create_device;
    } f;

    enum objtype root_vnode_type;

    uint8_t max_page_bits;

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

union iommu_devaddr
{
    struct {
        uint16_t segment;
        uint8_t  bus;
        uint8_t  device;
        uint8_t  function;
    } pci;
    uint64_t address;
};

/**
 * @brief represents a device in the IOMMU context
 */
struct iommu_device
{
    ///< the iommu responsible for this device
    struct iommu           *iommu;

    ///< the iommu binding
    struct iommu_binding   *binding;

    ///< the device address
    union iommu_devaddr addr;

    ////< the root vnode for this device
    struct capref root_vnode;

    struct {
        set_root_fn set_root;
        map_fn      map;
        unmap_fn    unmap;
    } f;
};

#include <hw_records.h>

errval_t iommu_device_create_by_address(struct iommu *iommu, uint64_t addr,
                                    struct iommu_device **iodev);
errval_t iommu_device_create_by_pci(struct iommu *iommu, uint16_t seg,
                                    uint8_t bus, uint8_t dev, uint8_t fun,
                                    struct iommu_device **iodev);
errval_t iommu_device_destroy(struct iommu_device *iodev);

errval_t iommu_device_lookup_by_address(uint64_t address, struct iommu_device **rdev);
errval_t iommu_device_lookup_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                    uint8_t fun, struct iommu_device **rdev);

errval_t iommu_device_lookup_iommu_by_address(uint64_t address, struct iommu ** iommu);
errval_t iommu_device_lookup_iommu_by_pci(uint16_t seg, uint8_t bus, uint8_t dev,
                                          uint8_t fun, struct iommu ** iommu);

static inline struct iommu *iommu_device_get_iommu(struct iommu_device *d)
{
    return d->iommu;
}


/**
 * @brief retypes the provided capability into a new type, and sets it read only
 *
 * @param src   the source capablity to be retyped
 * @param type  the target type
 * @param ret   returns the capability to be retyped
 *
 * @return SYS_ERR_OK on sucess, errval on failure
 */
static inline errval_t iommu_retype_read_only(struct capref src, enum objtype type,
                                              struct capref *ret)
{
    *ret = src;
    return SYS_ERR_OK;
}

struct vnodest
{
    struct vnodest *next;
    struct vnode_identity id;
    struct capref cap;
};



errval_t iommu_service_new_endpoint(struct capref ep, struct iommu_device *dev,
                                    idc_endpoint_t type);

errval_t iommu_bind_to_pci(struct capref ep, struct iommu* iommu);

errval_t iommu_request_endpoint(uint8_t type, struct capref* cap, struct iommu* iommu);
#endif //IOMMU_COMMON_H_
