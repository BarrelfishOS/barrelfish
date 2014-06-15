/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VBLOCK_HOST_H_
#define VBLOCK_HOST_H_



/**
 * \brief Initializes
 */
errval_t vblock_host_get_device_cap(struct capref *cap);

errval_t vblock_host_device_poll(void);

void *vblock_host_translate_phys2virt(lpaddr_t paddr);

#endif /* VBLOCK_HOST_H_ */
