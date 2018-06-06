/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef HELPER_H_
#define HELPER_H_

#include <errors/errno.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/vregion.h>

struct iommu_client;
struct dmem;

errval_t alloc_and_map_frame(struct iommu_client* cl, vregion_flags_t attr, size_t size, 
                         struct dmem* mem);

#endif // ndef HELPER_H_
