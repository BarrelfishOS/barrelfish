/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CAPOPS_H
#define CAPOPS_H

typedef void (*copy_result_handler_t)(errval_t, capaddr_t, void*);
errval_t copy(struct capref src, coreid_t dest,
              copy_result_handler_t result_handler, void *st);

typedef void (*move_result_handler_t)(errval_t, void*);
errval_t move(struct capref cap, coreid_t dest,
              move_result_handler_t result_handler, void *st);

typedef void (*delete_result_handler_t)(errval_t, void*);
errval_t delete(struct capref cap, delete_result_handler_t result_handler,
                void *st);

typedef void (*revoke_result_handler_t)(errval_t, void*);
errval_t revoke(struct capref cap, revoke_result_handler_t result_handler,
                void *st);

#endif
