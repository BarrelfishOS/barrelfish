/** \file
 *  \brief Memory server benchmark tracing
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __MEMTEST_TRACE_H__
#define __MEMTEST_TRACE_H__

#include <barrelfish/barrelfish.h>

// #define TRACE_SUBSYS_MEMTEST (TRACE_SUBSYS_MEMSERV | 0x0FF)
#define TRACE_SUBSYS_MEMTEST 0xA0FF
#define TRACE_EVENT_MEMTEST_START 0x0000
#define TRACE_EVENT_MEMTEST_STOP 0x0001
#define TRACE_EVENT_MEMTEST_STARTED 0x0020
#define TRACE_EVENT_MEMTEST_WAIT 0x0030
#define TRACE_EVENT_MEMTEST_RUN 0x0040
#define TRACE_EVENT_MEMTEST_DONE 0x0050
#define TRACE_EVENT_MEMTEST_ALLOC 0x0066
#define TRACE_EVENT_NSTEST_MASTER 0x0444
#define TRACE_EVENT_NSTEST_WORKER 0x0555

errval_t init_tracing(void);
void start_tracing(void);
void stop_tracing(void);
void prepare_dump(void);
void dump_trace(void);

#endif /* __MEMTEST_TRACE_H__ */
