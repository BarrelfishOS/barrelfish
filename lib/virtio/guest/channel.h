/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VIRTIO_GUEST_CHANNEL_H
#define VIRTIO_GUEST_CHANNEL_H


errval_t virtio_guest_flounder_init(char *iface);



errval_t virtio_guest_xeon_phi_init(void);


#endif // VIRTIO_GUEST_CHANNEL_H
