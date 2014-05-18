/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_H_
#define XEON_PHI_H_

#include <xeon_phi/xeon_phi.h>

/*
 * Common setting values
 */

/// the name of the Xeon Phi bootloader image
#define XEON_PHI_BOOTLOADER "xeon_phi_bootloader"

/// the name of the Xeon Phi multiboot image containint the modules
#define XEON_PHI_MULTIBOOT "xeon_phi_multiboot_ramfs"

/// if we use MSIX interrupts or legacy interrupts
#define XEON_PHI_MSIX_ENABLED 1

/// the number of MSIX interrupts we use
#define XEON_PHI_MSIX_NUM     1

/*
 * Debug output switches
 */
#define XDEBUG_ENABLED  1
#define XDEBUG_BOOT     1
#define XDEBUG_DMA      1
#define XDEBUG_INT      1
#define XDEBUG_SMPT     1
#define XDEBUG_SERVICE  1

/*
 * This defines are used to reference the MMIO registers on the host side.
 *
 * The Mackerel Specifications use the SBOX or DBOX baseaddress as their
 * register base. however the SBOX or DBOX have a certain offset into the
 * MMIO range.
 */
#define HOST_DBOX_OFFSET      0x00000000
#define HOST_SBOX_OFFSET      0x00010000

#define XEON_PHI_MMIO_TO_SBOX(phi) \
    ((mackerel_addr_t)((lvaddr_t)(phi->mmio.vbase)+HOST_SBOX_OFFSET))
#define XEON_PHI_MMIO_TO_DBOX(phi) \
    ((mackerel_addr_t)((lvaddr_t)(phi->mmio.vbase)+HOST_DBOX_OFFSET))

/*
 * --------------------------------------------------------------------------
 * Debug output generation
 */
#if XDEBUG_ENABLED
#define XDEBUG_PRINT(x...) debug_printf(x)
#else
#define XDEBUG_PRINT(x... )
#endif
#if XDEBUG_BOOT
#define XBOOT_DEBUG(x...) XDEBUG_PRINT(" BOOT | " x)
#else
#define XBOOT_DEBUG(x...)
#endif
#if XDEBUG_DMA
#define XDMA_DEBUG(x...) XDEBUG_PRINT(" DMA | " x)
#else
#define XDMA_DEBUG(x...)
#endif
#if XDEBUG_INT
#define XINT_DEBUG(x...) XDEBUG_PRINT(" INT | " x)
#else
#define XINT_DEBUG(x...)
#endif
#if XDEBUG_SMPT
#define XSMPT_DEBUG(x...) XDEBUG_PRINT(" SMPT | " x)
#else
#define XSMPT_DEBUG(x...)
#endif
#if XDEBUG_SERVICE
#define XSERVICE_DEBUG(x...) XDEBUG_PRINT(" SVC | " x)
#else
#define XSERVICE_DEBUG(x...)
#endif

/*
 * --------------------------------------------------------------------------
 * Xeon Phi Management structure
 */

/// represents the state of the Xeon Phi
typedef enum xeon_phi_state
{
    XEON_PHI_STATE_NULL,    ///< The card has not yet been initialized
    XEON_PHI_STATE_PCI_OK,  ///< The card has been registered with PCI
    XEON_PHI_STATE_RESET,   ///< The card has been reset
    XEON_PHI_STATE_READY,   ///< The card is ready to load the os
    XEON_PHI_STATE_BOOTING,  ///< The card is in the booting state
    XEON_PHI_STATE_ONLINE   ///< the card has booted and is online
} xeon_phi_state_t;

typedef enum xnode_state {
    XNODE_STATE_NONE,
    XNODE_STATE_REGISTERING,
    XNODE_STATE_READY,
    XNODE_STATE_FAILURE
} xnode_state_t;

/// represents the memory ranges occupied by the Xeon Phi card
struct mbar
{
    lvaddr_t vbase;
    lpaddr_t pbase;
    size_t length;
    /*
     * XXX: may it be useful to store the cap here aswell ?
     */
};

struct xnode
{
    struct xeon_phi_binding *binding;
    iref_t                   iref;
    xnode_state_t state;
    uint8_t         id;
    struct xeon_phi *local;
};

struct xeon_phi
{
    xeon_phi_state_t state;
    struct mbar mmio;       ///< pointer to the MMIO address range
    struct mbar apt;        ///< pointer to the aperture address range

    uint8_t id;             ///< card id for identifying the card
    iref_t iref;
    uint32_t apicid;        ///< APIC id used for sending the boot interrupt

    uint8_t      connected;
    struct xnode topology[XEON_PHI_NUM_MAX];

    char *cmdline;          ///< pointer to the bootloader cmdline

    struct smpt_info *smpt;  ///< pointer to the SMPT information struct
    struct irq_info *irq;  ///< pointer to the IRQ information struct
    struct dma_infi *dma;  ///< pointer to the DMA information struct
};

/**
 * \brief starts the serial receive thread
 *
 * \param phi   pointer to the card information
 */
errval_t xeon_phi_serial_start_recv_thread(struct xeon_phi *phi);

/**
 * \brief initializes the serial receiver
 *
 * \param phi   pointer to the card information
 */
errval_t xeon_phi_serial_init(struct xeon_phi *phi);

/**
 * \brief boots the card with the given loader and multiboot image
 *
 * \param phi           pointer to the card information
 * \param xloader_img   pointer to the card bootloader image
 * \param multiboot_img pointer to the card multiboot image
 */
errval_t xeon_phi_boot(struct xeon_phi *phi,
                       char *xloader_img,
                       char *multiboot_img);

/**
 * \brief performs a soft reset of the card
 *
 * \param phi   pointer to the card information
 */
errval_t xeon_phi_reset(struct xeon_phi *phi);

/**
 * \brief initializes the coprocessor card
 *
 * \param phi pointer to the information structure
 */
errval_t xeon_phi_init(struct xeon_phi *phi);

/**
 * \brief Bootstraps the host driver to get the multiboot images of the
 *        xeon phi loader and the xeon phi multiboot image
 */
errval_t host_bootstrap(void);

#endif /* XEON_PHI_H_ */
