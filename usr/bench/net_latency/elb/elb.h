/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ELB_H_
#define ELB_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>


extern void *buffer_base;
extern size_t buffer_size;


void benchmark_init(size_t buffers);
void benchmark_argument(const char *arg);
void benchmark_rx_done(size_t idx, size_t len);
void benchmark_tx_done(size_t idx);
void benchmark_do_pending_work(void);

void buffer_tx_add(size_t idx, size_t len);
void buffer_rx_add(size_t idx);

static inline void *buffer_address(size_t idx) {
    return (void*) ((uintptr_t) buffer_base + idx * buffer_size);
}

void terminate_benchmark(void);


#endif // ndef ELB_H_


