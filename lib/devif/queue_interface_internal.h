/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef QUEUE_INTERFACE_INTERNAL_H_
#define QUEUE_INTERFACE_INTERNAL_H_ 1

#include <devif/queue_interface.h>

errval_t devq_init(struct devq *q, bool exp);
errval_t devq_destroy(struct devq *q);

errval_t devq_add_region(struct devq*, struct capref cap,
                         regionid_t rid);

errval_t devq_remove_region(struct devq*, regionid_t rid);

#endif /* QUEUE_INTERFACE_INTERNAL_H_ */
