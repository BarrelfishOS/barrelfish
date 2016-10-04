/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef LOOPBACK_DEVICE_H_
#define LOOPBACK_DEVICE_H_ 1


#include <barrelfish/barrelfish.h>
#include "queue_interface.h"

// Loopback device functions
errval_t devq_loopback_setup(uint32_t coreid, uint64_t flags, uint64_t *features, 
                             uint32_t* default_qsize, uint32_t* default_bufsize, 
                             bool* reconnect, char* name);
errval_t devq_loopback_destroy(struct devq *q);

errval_t devq_loopback_create(struct devq *q, uint64_t flags);   
errval_t devq_loopback_notify(struct devq *q, uint8_t num_slots);

errval_t devq_loopback_register(struct devq *q, struct capref cap,
                                regionid_t region_id);
errval_t devq_loopback_deregister(struct devq *q, regionid_t region_id);

errval_t devq_loopback_control(struct devq *q, uint64_t request,
                               uint64_t value);

#endif /* LOOPBACK_DEVICE_H_ */
