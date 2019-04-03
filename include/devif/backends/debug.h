/*
 * Copyright (c) 2016 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DEVIF_DEBUG_H_
#define DEVIF_DEBUG_H_ 1


#include <barrelfish/barrelfish.h>


struct debug_q;

/**
 */
errval_t debug_create(struct debug_q** q,
                      struct devq* other_q);

errval_t debug_dump_region(struct debug_q* que, regionid_t rid);

void debug_dump_history(struct debug_q* q);


/**
 * @brief Adding region to debug queue. When using two endpoints
 *        Only the lowest layer is consistent with the regions
 *        To mache the debugging layer consistent, this method adds the region
 *        to the known regions so that the ckecks when dequeueing work
 *
 * @param q                     Return pointer to the descriptor queue
 * @param cap                   cap to the region
 * @param rid                  the regionid of the region
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t debug_add_region(struct debug_q*, struct capref cap,
                         regionid_t rid);

/**
 * @brief Removing region from debug queue
 *
 * @param q                     Return pointer to the descriptor queue
 * @param rid                  the regionid of the region
 *
 * @returns error on failure or SYS_ERR_OK on success
 */
errval_t debug_remove_region(struct debug_q*, regionid_t rid);
#endif /* DEVIF_DEBUG_H_ */
