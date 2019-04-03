/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTEL_VTD_PASID_CACHE_H_
#define INTEL_VTD_PASID_CACHE_H_ 1


#include "intel_vtd.h"
#include "intel_vtd_debug.h"

#define PASID_CACHE_INVALIDATE_TIMEOUT 0x1000

static inline void vtd_pasid_cache_invalidate(struct vtd *vtd)
{
    INTEL_VTD_DEBUG_PASIDCACHE("invalidate global\n");


}




static inline void vtd_pasid_cache_enable(struct vtd *vtd)
{

}

static inline void vtd_pasid_cache_disable(struct vtd *vtd)
{

}

#endif /// INTEL_VTD_PASID_CACHE_H_
