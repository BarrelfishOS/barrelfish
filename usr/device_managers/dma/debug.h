/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DMA_MGR_DEBUG_H_
#define DMA_MGR_DEBUG_H_


/*
 * Debug output switches
 */
#define DEBUG_ENABLED     0
#define DEBUG_SVC_ENABLED 1
#define DEBUG_DS_ENABLED 1


/*
 * --------------------------------------------------------------------------
 * Debug output generation
 */
#if DEBUG_ENABLED
#define DEBUGPRINT(x...) debug_printf(x)
#else
#define DEBUGPRINT(x... )
#endif
#if DEBUG_SVC_ENABLED
#define SVC_DEBUG(x...) DEBUGPRINT("svc | " x)
#else
#define SVC_DEBUG(x...)
#endif
#if DEBUG_DS_ENABLED
#define DS_DEBUG(x...)  DEBUGPRINT("ds  | " x)
#else
#define DS_DEBUG(x...)
#endif


#define ERRPRINT(x...) debug_printf(x)
#define SVC_ERR(x...) ERRPRINT("svc | ERROR: " x)
#define DS_ERR(x...)  ERRPRINT("ds  | ERROR: " x)

#endif /* DMA_MGR_DEBUG_H_ */
