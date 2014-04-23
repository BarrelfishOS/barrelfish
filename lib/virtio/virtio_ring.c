/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <virtio/virtio_ring.h>




/**
 * \brief allocates a new vring structure
 *
 * \param vr    pointer to the vring structure
 * \param num   the number of queue elements
 * \param align the alignment constraints for the vring
 */
errval_t
vring_alloc(struct vring *vr,
            uint16_t num,
            uintptr_t align)
{



}

/**
 * \brief frees the resources used by the vring structure
 */
errval_t
vring_free(struct vring *vr)
{


}

/**
 * \brief    Initializes a vring structure with the given caps
 *
 * \param vr The vring to initialize
 *
 * The capabilities must already been set in the vring structure. The caps
 * will be mapped into the vspace and the addresses set accordingly
 */
errval_t
vring_init(struct vring *vr)
{


}
