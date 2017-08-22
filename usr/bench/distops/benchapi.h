/**
 * \file
 * \brief Distops benchmark API
 */

/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BENCH_DISTOPS_BENCHAPI_H
#define BENCH_DISTOPS_BENCHAPI_H

#define PANIC_IF_ERR(err, msg...) \
do { \
    if (err_is_fail(err)) { \
        USER_PANIC_ERR(err, msg); \
    } \
} while(0)

//{{{1 Mgmt node API

void broadcast_cmd(uint32_t cmd);

void broadcast_caps(uint32_t cmd, struct capref cap1, struct capref cap2);

errval_t mgmt_init_benchmark(void **st, int nodecount);

errval_t mgmt_init_node(void **st);

void mgmt_run_benchmark(void *st);

void mgmt_cmd(uint32_t cmd, struct bench_distops_binding *b);

void mgmt_cmd_caps(uint32_t cmd, struct capref cap1, struct capref cap2,
                  struct bench_distops_binding *b);

void *get_global_state(struct bench_distops_binding *b);

//{{{1 Node API

void init_node(struct bench_distops_binding *b);

void node_cmd(uint32_t cmd, struct bench_distops_binding *b);

void node_cmd_caps(uint32_t cmd, struct capref cap1, struct capref cap2,
                   struct bench_distops_binding *b);

#endif // BENCH_DISTOPS_BENCHAPI_H
