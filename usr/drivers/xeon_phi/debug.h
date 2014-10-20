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
#define XDEBUG_DMA       0
#define XDEBUG_DMA_V     0
#define XDEBUG_INT       1
#define XDEBUG_SMPT      0
#define XDEBUG_SERVICE   0
#define XDEBUG_MESSAGING 0
#define XDEBUG_SYSMEM    0
#define XDEBUG_SPAWN     0
#define XDEBUG_INTERPHI  0

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
#define XBOOT_DEBUG(x...) XDEBUG("boot > " x)
#else
#define XBOOT_DEBUG(x...)
#endif
#if XDEBUG_DMA
#define XDMA_DEBUG(x...) XDEBUG(" dma > " x)
#if XDEBUG_DMA_V
#define XDMAV_DEBUG(x...) XDEBUG("dmav > " x)
#else
#define XDMAV_DEBUG(x...)
#endif
#else
#define XDMA_DEBUG(x...)
#define XDMAV_DEBUG(x...)
#endif

#if XDEBUG_INT
#define XINT_DEBUG(x...) XDEBUG("intr > " x)
#else
#define XINT_DEBUG(x...)
#endif
#if XDEBUG_SMPT
#define XSMPT_DEBUG(x...) XDEBUG("smpt > " x)
#else
#define XSMPT_DEBUG(x...)
#endif
#if XDEBUG_SERVICE
#define XSERVICE_DEBUG(x...) XDEBUG(" svc > " x)
#else
#define XSERVICE_DEBUG(x...)
#endif
#if XDEBUG_MESSAGING
#define XMESSAGING_DEBUG(x...) XDEBUG(" msg > " x)
#else
#define XMESSAGING_DEBUG(x...)
#endif
#if XDEBUG_SYSMEM
#define XSYSMEM_DEBUG(x...) XDEBUG("sysm > " x)
#else
#define XSYSMEM_DEBUG(x...)
#endif
#if XDEBUG_SPAWN
#define XSPAWN_DEBUG(x...) XDEBUG("spawn > " x)
#else
#define XSPAWN_DEBUG(x...)
#endif
#if XDEBUG_INTERPHI
#define XINTER_DEBUG(x...) XDEBUG("inter > " x)
#else
#define XINTER_DEBUG(x...)
#endif



#endif /* XEON_PHI_DEBUG_H_ */
