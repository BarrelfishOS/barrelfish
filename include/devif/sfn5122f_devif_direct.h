/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef SFN5122F_DEVIF_DIRECT_H_
#define SFN5122F_DEVIF_DIRECT_H_ 1

#include <devif/queue_interface.h>

errval_t sfn5122f_create_direct(struct devq* q, uint64_t flags);
errval_t sfn5122f_register_direct(struct devq* q, struct capref cap,
                                  regionid_t rid);
errval_t sfn5122f_deregister_direct(struct devq* q, regionid_t rid);
errval_t sfn5122f_control_direct(struct devq* q, uint64_t cmd, uint64_t value);
errval_t sfn5122f_destroy_direct(struct devq* q);
errval_t sfn5122f_notify_direct(struct devq* q, uint8_t num_slots);
errval_t sfn5122f_enqueue_direct(struct devq* q, regionid_t rid, bufferid_t bid, 
                                 lpaddr_t base, size_t len, uint64_t flags);
errval_t sfn5122f_dequeue_direct(struct devq* q, regionid_t* rid, bufferid_t* bid, 
                                 lpaddr_t* base, size_t* len, uint64_t* flags);
#endif
