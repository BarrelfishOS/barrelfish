/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CAPOPS_INTERNAL_H
#define CAPOPS_INTERNAL_H

#include <stdlib.h>
#include <errors/errno.h>
#include <barrelfish/waitset.h>
#include <barrelfish/debug.h>
#include <capops.h>
#include <if/intermon_defs.h>

typedef void (*gen_result_cont_fn)(errval_t, void*);
struct result_closure {
    gen_result_cont_fn handler;
    void *arg;
};
#define MKRESCONT(h,a) ((struct result_closure){ .handler = (h), .arg = (a) })
#define CALLRESCONT(c,e) ((c).handler((e), (c).arg))

#define malloce(size, ret) \
    ((*(ret) = malloc(size)) \
     ? SYS_ERR_OK \
     : LIB_ERR_MALLOC_FAIL)

#define calloce(n, size, ret) \
    ((*(ret) = calloc(n, size)) \
     ? SYS_ERR_OK \
     : LIB_ERR_MALLOC_FAIL)

#define GOTO_IF_ERR(err, label) do { \
    if (err_is_fail(err)) { \
        DEBUG_ERR(err, "%s:%u -> goto err\n", __FUNCTION__, __LINE__); \
        goto label; \
    } \
} while (0)

#define PANIC_IF_ERR(err, msg) do { \
    errval_t tmp_err__ = (err); \
    if (err_is_fail(tmp_err__)) { \
        USER_PANIC_ERR(tmp_err__, (msg)); \
    } \
} while (0)

#define PANIC_IF_ERR2(err2, msg2, err, msg) do { \
    errval_t tmp_err2__ = (err2); \
    if (err_is_fail(tmp_err2__)) { \
        DEBUG_ERR((err), (msg)); \
        USER_PANIC_ERR(tmp_err2__, (msg2)); \
    } \
} while (0)

#define DEBUG_IF_ERR(err, msg) do { \
    errval_t tmp_err__ = (err); \
    if (err_is_fail(tmp_err__)) { \
        DEBUG_ERR(tmp_err__, (msg)); \
    } \
} while (0)

void find_cap__rx_handler(struct intermon_binding *b, intermon_caprep_t caprep,
                          genvaddr_t st);
void find_cap_result__rx_handler(struct intermon_binding *b, errval_t result,
                                 genvaddr_t st);
void find_descendants__rx_handler(struct intermon_binding *b,
                                  intermon_caprep_t caprep, genvaddr_t st);
void find_descendants_result__rx_handler(struct intermon_binding *b,
                                         errval_t status, genvaddr_t st);
void owner_updated__rx_handler(struct intermon_binding *b, genvaddr_t st);
void update_owner__rx_handler(struct intermon_binding *b,
                              intermon_caprep_t caprep, genvaddr_t st);
void recv_copy_result__rx(struct intermon_binding *b, errval_t status,
                          capaddr_t capaddr, uint8_t vbits, cslot_t slot,
                          genvaddr_t st);
void recv_copy__rx(struct intermon_binding *b, intermon_caprep_t caprep,
                   uint8_t owner_relations, genvaddr_t st);
void request_copy__rx(struct intermon_binding *b, coreid_t dest,
                      intermon_caprep_t caprep, genvaddr_t st);
void delete_remote__rx(struct intermon_binding *b,
                               intermon_caprep_t caprep, genvaddr_t st);
void delete_remote_result__rx(struct intermon_binding *b,
                                      errval_t status, genvaddr_t st);
void move_request__rx_handler(struct intermon_binding *b,
                              intermon_caprep_t caprep, uint8_t relations,
                              genvaddr_t st);
void move_result__rx_handler(struct intermon_binding *b, errval_t status,
                             genvaddr_t st);
void retrieve_request__rx(struct intermon_binding *b,
                          intermon_caprep_t caprep,
                          genvaddr_t st);
void retrieve_result__rx(struct intermon_binding *b,
                         errval_t status, uint8_t relations,
                         genvaddr_t st);
void retype_request__rx(struct intermon_binding *b, intermon_caprep_t srcrep,
                        uint32_t desttype, uint32_t destbits, genvaddr_t st);
void retype_response__rx(struct intermon_binding *b, errval_t status,
                                 genvaddr_t st);
void revoke_mark__rx(struct intermon_binding *b,
                     intermon_caprep_t caprep,
                     genvaddr_t st);
void revoke_ready__rx(struct intermon_binding *b, genvaddr_t st);
void revoke_commit__rx(struct intermon_binding *b, genvaddr_t st);
void revoke_done__rx(struct intermon_binding *b, genvaddr_t st);

size_t num_monitors_online(void);

#endif
