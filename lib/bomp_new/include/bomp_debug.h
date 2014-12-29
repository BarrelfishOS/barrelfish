/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 64, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __LIBBOMP_DEBUG_H
#define	__LIBBOMP_DEBUG_H

#define BOMP_DEBUG_ENABLE 1
#define BOMP_DEBUG_INIT_ENABLE 1
#define BOMP_DEBUG_EXEC_ENABLE 1
#define BOMP_DEBUG_CTRL_ENABLE 1
#define BOMP_DEBUG_THREAD_ENABLE 1
#define BOMP_DEBUG_NODE_ENABLE 1

#define BOMP_ERROR(x, ...) debug_printf("[libbomp] ERROR: " x, __VA_ARGS__)
#define BOMP_WARNING(x, ...)
#define BOMP_NOTICE(x, ...) debug_printf("[libbomp] NOTICE: " x, __VA_ARGS__)

#if BOMP_DEBUG_ENABLE
#define BOMP_DEBUG_PRINT(x...) debug_printf("[libbomp] " x);
#else
#define BOMP_DEBUG_PRINT(x...)
#endif

#if BOMP_DEBUG_INIT_ENABLE
#define BOMP_DEBUG_INIT(x...) BOMP_DEBUG_PRINT("[init  ] " x)
#else
#define BOMP_DEBUG_INIT(x...)
#endif

#if BOMP_DEBUG_EXEC_ENABLE
#define BOMP_DEBUG_EXEC(x...) BOMP_DEBUG_PRINT("[exec  ] " x)
#else
#define BOMP_DEBUG_EXEC(x...)
#endif

#if BOMP_DEBUG_CTRL_ENABLE
#define BOMP_DEBUG_CTRL(x...) BOMP_DEBUG_PRINT("[ctrl  ] " x)
#else
#define BOMP_DEBUG_CTRL(x...)
#endif

#if BOMP_DEBUG_THREAD_ENABLE
#define BOMP_DEBUG_THREAD(x...) BOMP_DEBUG_PRINT("[thread] " x)
#else
#define BOMP_DEBUG_THREAD(x...)
#endif

#if BOMP_DEBUG_NODE_ENABLE
#define BOMP_DEBUG_NODE(x...) BOMP_DEBUG_PRINT("[node  ] " x)
#else
#define BOMP_DEBUG_NODE(x...)
#endif


#endif	/* __LIBBOMP_DEBUG_H */
