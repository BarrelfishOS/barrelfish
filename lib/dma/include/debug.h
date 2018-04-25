/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_DMA_DEBUG_H
#define LIB_DMA_DEBUG_H

/// master debug switch
#define LIB_DMA_DEBUG_ENABLED     0

/*
 * ---------------------------------------------------------------------------
 *  Generic DMA debug switches
 */
#define DMA_DEBUG_ENABLED         1
#define DMA_DEBUG_CHAN_ENABLED    1
#define DMA_DEBUG_DEVICE_ENABLED  1
#define DMA_DEBUG_REQUEST_ENABLED 1
#define DMA_DEBUG_INTR_ENABLED    1
#define DMA_DEBUG_SVC_ENABLED     1
#define DMA_DEBUG_MEM_ENABLED     1
#define DMA_DEBUG_CLIENT_ENABLED  1
#define DMA_DEBUG_MGR_ENABLED     1
#define DMA_DEBUG_DESC_ENABLED    1
/*
 * ---------------------------------------------------------------------------
 *  IOAT debug switches
 */
#define IOAT_DEBUG_ENABLED         1
#define IOAT_DEBUG_CHAN_ENABLED    1
#define IOAT_DEBUG_DEVICE_ENABLED  1
#define IOAT_DEBUG_REQUEST_ENABLED 1
#define IOAT_DEBUG_DESC_ENABLED    1
#define IOAT_DEBUG_INTR_ENABLED    1
#define IOAT_DEBUG_DCA_ENABLED     1

/*
 * ---------------------------------------------------------------------------
 *  Xeon Phi debug switches
 */
#define XEON_PHI_DEBUG_ENABLED     1
#define XPHI_DEBUG_CHAN_ENABLED    1
#define XPHI_DEBUG_DEVICE_ENABLED  1
#define XPHI_DEBUG_REQUEST_ENABLED 1
#define XPHI_DEBUG_DESC_ENABLED    1
#define XPHI_DEBUG_INTR_ENABLED    1

/*
 * ---------------------------------------------------------------------------
 *  DMA client debug switches
 */
#define DMA_CLIENT_DEBUG_ENABLED 1
#define DMA_CLIENT_DEBUG_CHAN_ENABLED    1
#define DMA_CLIENT_DEBUG_DEVICE_ENABLED  1
#define DMA_CLIENT_DEBUG_REQUEST_ENABLED 1

/*
 * --------------------------------------------------------------------------
 *  Generic DMA debug output generation
 */

#if (LIB_DMA_DEBUG_ENABLED && DMA_DEBUG_ENABLED)
#define DMA_DEBUG_PRINT(x...) debug_printf(x)
#else
#define DMA_DEBUG_PRINT(x... )
#endif
#if DMA_DEBUG_INTR_ENABLED
#define DMAINT_DEBUG(x...) DMA_DEBUG_PRINT("[dma intr] " x)
#else
#define DMAINT_DEBUG(x...)
#endif
#if DMA_DEBUG_CHAN_ENABLED
#define DMACHAN_DEBUG(x...) DMA_DEBUG_PRINT("[dma chan.%04x] " x)
#else
#define DMACHAN_DEBUG(x...)
#endif
#if DMA_DEBUG_REQUEST_ENABLED
#define DMAREQ_DEBUG(x...) DMA_DEBUG_PRINT("[dma req] " x)
#else
#define DMAREQ_DEBUG(x...)
#endif
#if DMA_DEBUG_DEVICE_ENABLED
#define DMADEV_DEBUG(x...) DMA_DEBUG_PRINT("[dma dev.%02x] " x)
#else
#define DMADEV_DEBUG(x...)
#endif
#if DMA_DEBUG_SVC_ENABLED
#define DMASVC_DEBUG(x...) DMA_DEBUG_PRINT("[dma svc] " x)
#else
#define DMASVC_DEBUG(x...)
#endif
#if DMA_DEBUG_MEM_ENABLED
#define DMAMEM_DEBUG(x...) DMA_DEBUG_PRINT("[dma mem] " x)
#else
#define DMAMEM_DEBUG(x...)
#endif
#if DMA_DEBUG_CLIENT_ENABLED
#define DMACLIENT_DEBUG(x...) DMA_DEBUG_PRINT("[dma client] " x)
#else
#define DMACLIENT_DEBUG(x...)
#endif
#if DMA_DEBUG_MGR_ENABLED
#define DMAMGR_DEBUG(x...) DMA_DEBUG_PRINT("[dma mgr] " x)
#else
#define DMAMGR_DEBUG(x...)
#endif
#if DMA_DEBUG_DESC_ENABLED
#define DMADESC_DEBUG(x...) DMA_DEBUG_PRINT("[dma desc] " x)
#else
#define DMADESC_DEBUG(x...)
#endif

/*
 * --------------------------------------------------------------------------
 *  IOAT Debug output generation
 */

#if (LIB_DMA_DEBUG_ENABLED && IOAT_DEBUG_ENABLED)
#define IOAT_DEBUG_PRINT(x...) debug_printf(x)
#else
#define IOAT_DEBUG_PRINT(x... )
#endif
#if IOAT_DEBUG_INTR_ENABLED
#define IOATINT_DEBUG(x...) IOAT_DEBUG_PRINT("[ioat intr] " x)
#else
#define IOATINT_DEBUG(x...)
#endif
#if IOAT_DEBUG_CHAN_ENABLED
#define IOATCHAN_DEBUG(x...) IOAT_DEBUG_PRINT("[ioat chan.%04x] " x)
#else
#define IOATCHAN_DEBUG(x...)
#endif
#if IOAT_DEBUG_REQUEST_ENABLED
#define IOATREQ_DEBUG(x...) IOAT_DEBUG_PRINT("[ioat req] " x)
#else
#define IOATREQ_DEBUG(x...)
#endif
#if IOAT_DEBUG_DEVICE_ENABLED
#define IOATDEV_DEBUG(x...) IOAT_DEBUG_PRINT("[ioat dev.%02x] " x)
#else
#define IOATDEV_DEBUG(x...)
#endif
#if IOAT_DEBUG_DESC_ENABLED
#define IOATDESC_DEBUG(x...) IOAT_DEBUG_PRINT("[ioat desc] " x)
#else
#define IOATDESC_DEBUG(x...)
#endif
#if IOAT_DEBUG_DCA_ENABLED
#define IOATDCA_DEBUG(x...) IOAT_DEBUG_PRINT("[ioat dca] " x)
#else
#define IOATDCA_DEBUG(x...)
#endif


/*
 * --------------------------------------------------------------------------
 *  XPHI Debug output generation
 */

#if (LIB_DMA_DEBUG_ENABLED && XEON_PHI_DEBUG_ENABLED)
#define XPHI_DEBUG_PRINT(x...) debug_printf(x)
#else
#define XPHI_DEBUG_PRINT(x... )
#endif
#if XPHI_DEBUG_INTR_ENABLED
#define XPHIINT_DEBUG(x...) XPHI_DEBUG_PRINT("[xdma intr] " x)
#else
#define XPHIINT_DEBUG(x...)
#endif
#if XPHI_DEBUG_CHAN_ENABLED
#define XPHICHAN_DEBUG(x...) XPHI_DEBUG_PRINT("[xdma chan.%04x] " x)
#else
#define XPHICHAN_DEBUG(x...)
#endif
#if XPHI_DEBUG_REQUEST_ENABLED
#define XPHIREQ_DEBUG(x...) XPHI_DEBUG_PRINT("[xdma req] " x)
#else
#define XPHIREQ_DEBUG(x...)
#endif
#if XPHI_DEBUG_DEVICE_ENABLED
#define XPHIDEV_DEBUG(x...) XPHI_DEBUG_PRINT("[xdma dev.%02x] " x)
#else
#define XPHIDEV_DEBUG(x...)
#endif
#if XPHI_DEBUG_DESC_ENABLED
#define XPHIDESC_DEBUG(x...) XPHI_DEBUG_PRINT("[xdma desc] " x)
#else
#define XPHIDESC_DEBUG(x...)
#endif


/*
 * --------------------------------------------------------------------------
 *  DMA Client debug output generation
 */
#if (LIB_DMA_DEBUG_ENABLED && DMA_CLIENT_DEBUG_ENABLED)
#define CLIENT_DEBUG_PRINT(x...) debug_printf(x)
#else
#define CLIENT_DEBUG_PRINT(x... )
#endif
#if DMA_CLIENT_DEBUG_CHAN_ENABLED
#define CLIENTCHAN_DEBUG(x...) CLIENT_DEBUG_PRINT("[client chan.%02x] " x)
#else
#define CLIENTCHAN_DEBUG(x...)
#endif
#if DMA_CLIENT_DEBUG_DEVICE_ENABLED
#define CLIENTDEV_DEBUG(x...) CLIENT_DEBUG_PRINT("[client dev.%02x] " x)
#else
#define CLIENTDEV_DEBUG(x...)
#endif
#if DMA_CLIENT_DEBUG_REQUEST_ENABLED
#define CLIENTREQ_DEBUG(x...) CLIENT_DEBUG_PRINT("[client req] " x)
#else
#define CLIENTREQ_DEBUG(x...)
#endif



#endif /* LIB_DMA_DEBUG_H */
