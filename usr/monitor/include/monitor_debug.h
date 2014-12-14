/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DEBUG_MONITOR_H
#define DEBUG_MONITOR_H

// XXX: should be done via Config.hs
//#define DEBUG_MONITOR_CAPOPS
//#define DEBUG_MONITOR_INVOCATIONS

#if defined(DEBUG_MONITOR_CAPOPS) || defined(DEBUG_MONITOR_ALL) || defined(GLOBAL_DEBUG)
#define DEBUG_CAPOPS(x...) debug_printf(x)
#else
#define DEBUG_CAPOPS(x...) ((void)0)
#endif
#if defined(DEBUG_MONITOR_INVOCATIONS) || defined(DEBUG_MONITOR_ALL) || defined(GLOBAL_DEBUG)
#define DEBUG_INVOCATION(x...) debug_printf(x)
#else
#define DEBUG_INVOCATION(x...) ((void)0)
#endif


#endif // DEBUG_MONITOR_H
