/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MONITOR_SEND_CAP_H
#define MONITOR_SEND_CAP_H

#include <if/intermon_defs.h>

struct captx_prepare_state;
typedef void (*captx_send_cont)(errval_t, struct captx_prepare_state*,
                                intermon_captx_t*, void*);
struct captx_prepare_state {
    intermon_captx_t captx;
    captx_send_cont send_cont;
    void *st;
};
void captx_prepare_send(struct capref cap, coreid_t dest, bool give_away,
                        struct captx_prepare_state *state,
                        captx_send_cont send_cont, void *st);

struct captx_recv_state;
typedef void (*captx_recv_cont)(errval_t, struct captx_recv_state*,
                                struct capref, void*);
struct captx_recv_state {
};
void captx_handle_recv(intermon_captx_t *captx, struct captx_recv_state *state,
                       captx_recv_cont recv_cont, void *st);


struct captx_abort_state;
typedef void (*captx_abort_cont)(errval_t, struct captx_abort_state*, void*);
struct captx_abort_state {
    captx_abort_cont abort_cont;
    void *st;
};
void captx_abort_recv(intermon_captx_t *captx, struct captx_abort_state *state,
                      captx_abort_cont abort_cont, void *st);

#endif
