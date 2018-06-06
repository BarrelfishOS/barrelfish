/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "e10k_helper.h"

#include <stdio.h>
#include <stdint.h>
#include <driverkit/iommu.h>


/* allocate a single frame, mapping it into our vspace with given attributes */
errval_t alloc_and_map_frame(struct iommu_client* cl, vregion_flags_t attr, 
                             size_t size, struct dmem* mem)
{
    errval_t err;
    
    if (cl == NULL) {
        err = frame_alloc(&(mem->mem), size, (size_t *)&(mem->size));
        if (err_is_fail(err)) {
            return err;
        }

        err = vspace_map_one_frame_attr((void**) &(mem->vbase), size, mem->mem, attr,
                                        NULL, NULL);
        if (err_is_fail(err)) {
            cap_destroy(mem->mem);
            DEBUG_ERR(err, "vspace_map_one_frame failed");
            return err;
        }

    } else  {
        err = driverkit_iommu_mmap_cl(cl, size, attr, mem);
        if (err_is_fail(err)) {
            slot_free(mem->mem);
            return err;
        }
    }

    return err;
}

