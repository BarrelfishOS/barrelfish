/**
 * \file
 * \brief internal header of libnuma
 *
 * This is derived from:
 *
 * Linux man pages "numa"
 * libnuma from http://oss.sgi.com/projects/libnuma/
 *
 */

/*
 * Copyright (c) 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich.
 * Attn: Systems Group.
 */

#ifndef NUMA_DEBUG_H_
#define NUMA_DEBUG_H_

#define NUMA_DEBUG_ENABLED 0

#if NUMA_DEBUG_ENABLED
#define NUMA_DEBUG_PRINT(x...) debug_printf(x);
#else
#define NUMA_DEBUG_PRINT(x...)
#endif

#define NUMA_DEBUG_INIT(x...)  NUMA_DEBUG_PRINT("[numa  init] " x);

#define NUMA_DEBUG_ALLOC(x...) NUMA_DEBUG_PRINT("[numa alloc] " x);

#define NUMA_ERROR(fmt, ...) \
                debug_printf("[numa error] " fmt " in %s():", \
                             __VA_ARGS__, __FUNCTION__);
#define NUMA_WARNING(fmt, ...) \
                debug_printf("[numa  warn] " fmt " in %s():", \
                             __VA_ARGS__, __FUNCTION__);

#endif /* NUMA_DEBUG_H_ */
