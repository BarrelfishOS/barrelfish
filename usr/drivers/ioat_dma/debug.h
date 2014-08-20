/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAT_DEBUG_H_
#define IOAT_DEBUG_H_


/*
 * Debug output switches
 */
#define DEBUG_ENABLED   0
#define DEBUG_DEVICE    1
#define DEBUG_INTR      1
#define DEBUG_DMA_SVC   1
#define DEBUG_MGR_SVC   1


/*
 * --------------------------------------------------------------------------
 * Debug output generation
 */
#if DEBUG_ENABLED
#define DEBUGPRINT(x...) debug_printf(x)
#else
#define DEBUGPRINT(x... )
#endif
#if DEBUG_DEVICE
#define DEV_DEBUG(x...) DEBUGPRINT("device | " x)
#else
#define DEV_DEBUG(x...)
#endif
#if DEBUG_INTR
#define INTR_DEBUG(x...) DEBUGPRINT("intr  | " x)
#else
#define INTR_DEBUG(x...)
#endif
#if DEBUG_DMA_SVC
#define DMA_DEBUG(x...) DEBUGPRINT("dmasvc | " x)
#else
#define DMA_DEBUG(x...)
#endif
#if DEBUG_MGR_SVC
#define MGR_DEBUG(x...) DEBUGPRINT("mgrsvc | " x)
#else
#define MGR_DEBUG(x...)
#endif

#define ERRPRINT(x...) debug_printf(x)

#define DEV_ERR(x...) ERRPRINT("device | ERROR: " x)
#define DMA_ERR(x...) ERRPRINT("dmasvc | ERROR: " x)
#define DMA_MGR(x...) ERRPRINT("mgrsvc | ERROR: " x)

#endif /* IOAT_DEBUG_H_ */
