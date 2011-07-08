/**
 * \file
 * \brief Header file for the driver's part of the PCI memory management
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_MEM_H_
#define LIB_MEM_H_

struct device_mem {
    uint8_t type;       // 0 = memory BAR, 1 = IO BAR
    void* vaddr; //assigned by the device driver when calling maP-device()
    genpaddr_t paddr;
    struct capref *phys_cap, *frame_cap, io_cap;
    uint8_t bits;    //size of the physaddr cap in bits
    size_t bytes;
    uint32_t nr_caps;
    struct memobj *memobj;
    struct vregion *vregion;
};

errval_t map_device(struct device_mem *mem);
errval_t map_bars(struct device_mem *bars, int nr_mapped_bars);

#endif // LIB_MEM_H_

