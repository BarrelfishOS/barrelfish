/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_DEBUG_H_
#define XEON_PHI_DEBUG_H_


/*
 * Debug output switches
 */
#define XDEBUG_ENABLED   1
#define XDEBUG_BOOT      1
#define XDEBUG_DMA       1
#define XDEBUG_INT       1
#define XDEBUG_SMPT      1
#define XDEBUG_SERVICE   1
#define XDEBUG_MESSAGING 1
#define XDEBUG_SYSMEM    1
#define XDEBUG_SPAWN     1

/*
 * --------------------------------------------------------------------------
 * Debug output generation
 */
#if XDEBUG_ENABLED
#define XDEBUG(x...) debug_printf(x)
#else
#define XDEBUG_PRINT(x... )
#endif
#if XDEBUG_BOOT
#define XBOOT_DEBUG(x...) XDEBUG(" BOOT | " x)
#else
#define XBOOT_DEBUG(x...)
#endif
#if XDEBUG_DMA
#define XDMA_DEBUG(x...) XDEBUG(" DMA | " x)
#else
#define XDMA_DEBUG(x...)
#endif
#if XDEBUG_INT
#define XINT_DEBUG(x...) XDEBUG(" INT | " x)
#else
#define XINT_DEBUG(x...)
#endif
#if XDEBUG_SMPT
#define XSMPT_DEBUG(x...) XDEBUG(" SMPT | " x)
#else
#define XSMPT_DEBUG(x...)
#endif
#if XDEBUG_SERVICE
#define XSERVICE_DEBUG(x...) XDEBUG(" SVC | " x)
#else
#define XSERVICE_DEBUG(x...)
#endif
#if XDEBUG_MESSAGING
#define XMESSAGING_DEBUG(x...) XDEBUG(" MSG | " x)
#else
#define XMESSAGING_DEBUG(x...)
#endif
#if XDEBUG_SYSMEM
#define XSYSMEM_DEBUG(x...) XDEBUG(" SYSM | " x)
#else
#define XSYSMEM_DEBUG(x...)
#endif
#if XDEBUG_SPAWN
#define XSPAWN_DEBUG(x...) XDEBUG(" SPAWN | " x)
#else
#define XSPAWN_DEBUG(x...)
#endif


#endif /* XEON_PHI_DEBUG_H_ */
