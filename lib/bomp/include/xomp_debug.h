/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_XOMP_DEBUG_H_
#define LIB_XOMP_DEBUG_H_

/// XOMP main debug switch
#define XOMP_DEBUG_ENABLED 0

/// XOMP subsystems debug switches
#define XOMP_DEBUG_MASTER_ENABLED 1
#define XOMP_DEBUG_WORKER_ENABLED 0

#define XOMP_DEBUG_INIT_ENABLED 0
#define XOMP_DEBUG_PROCESS_ENABLED 1
#define XOMP_DEBUG_REPLICATIION_ENABLED 0
#define XOMP_DEBUG_GATEWAY_ENABLED 0

#if XOMP_DEBUG_ENABLED
#define XOMP_DEBUG_PRINT(x...) debug_printf(x)
#else
#define XOMP_DEBUG_PRINT(x...)
#endif

/*
 * ----------------------------------------------------------------------------
 * XOMP Master Debug
 * ----------------------------------------------------------------------------
 */
#if XOMP_DEBUG_MASTER_ENABLED
#define XOMP_DEBUG_MASTER_PRINT(x...) XOMP_DEBUG_PRINT(x)
#else
#define XOMP_DEBUG_MASTER_PRINT(x...)
#endif
#if XOMP_DEBUG_INIT_ENABLED
#define XMI_DEBUG(x...) XOMP_DEBUG_MASTER_PRINT("[xomp.m.init] " x)
#else
#define XMI_DEBUG(x...)
#endif
#if XOMP_DEBUG_PROCESS_ENABLED
#define XMP_DEBUG(x...) XOMP_DEBUG_MASTER_PRINT("[xomp.m.proc] " x)
#else
#define XMP_DEBUG(x...)
#endif


/*
 * ----------------------------------------------------------------------------
 * XOMP Worker Debug
 * ----------------------------------------------------------------------------
 */
#if XOMP_DEBUG_WORKER_ENABLED
#define XOMP_DEBUG_WORKER_PRINT(x...) XOMP_DEBUG_PRINT(x)
#else
#define XOMP_DEBUG_WORKER_PRINT(x...)
#endif
#if XOMP_DEBUG_INIT_ENABLED
#define XWI_DEBUG(x...) XOMP_DEBUG_WORKER_PRINT("[xomp.w.init] " x)
#else
#define XWI_DEBUG(x...)
#endif
#if XOMP_DEBUG_PROCESS_ENABLED
#define XWP_DEBUG(x...) XOMP_DEBUG_WORKER_PRINT("[xomp.w.proc] " x)
#else
#define XWP_DEBUG(x...)
#endif
#if XOMP_DEBUG_REPLICATIION_ENABLED
#define XWR_DEBUG(x...) XOMP_DEBUG_WORKER_PRINT("[xomp.w.repl] " x)
#else
#define XWR_DEBUG(x...)
#endif
#if XOMP_DEBUG_GATEWAY_ENABLED
#define XWG_DEBUG(x...) XOMP_DEBUG_WORKER_PRINT("[xomp.w.gw] " x)
#else
#define XWG_DEBUG(x...)
#endif

#endif /* LIB_XOMP_DEBUG_H_ */
