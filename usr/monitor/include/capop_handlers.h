/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CAPOPS_HANDLERS_H
#define CAPOPS_HANDLERS_H

#include <if/intermon_defs.h>

void find_cap__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st);

void find_cap_result__rx_handler(struct intermon_binding *b, errval_t result, genvaddr_t st);

void find_descendants__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st);

void find_descendants_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st);

void owner_updated__rx_handler(struct intermon_binding *b, genvaddr_t st);

void update_owner__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st);

void recv_copy_result__rx_handler(struct intermon_binding *b, errval_t status, capaddr_t capaddr, uint8_t vbits, cslot_t slot, genvaddr_t st);

void recv_copy__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st);

void request_copy__rx_handler(struct intermon_binding *b, coreid_t dest, intermon_caprep_t caprep, genvaddr_t st);

void delete_remote__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st);

void delete_remote_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st);

void move_request__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, uint8_t relations, genvaddr_t st);

void move_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st);

void request_retype__rx_handler(struct intermon_binding *b, intermon_caprep_t srcrep, int desttype, uint32_t destbits, genvaddr_t st);

void retype_response__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st);

void request_revoke__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep, genvaddr_t st);

void revoke_result__rx_handler(struct intermon_binding *b, errval_t status, genvaddr_t st);

#endif
