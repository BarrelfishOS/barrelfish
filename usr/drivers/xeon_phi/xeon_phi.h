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


#define XEON_PHI_BOOTLOADER "xeon_phi_bootloader"
#define XEON_PHI_MULTIBOOT "xeon_phi_multiboot"

#define XEON_PHI_MSIX_ENABLED 1

#define HOST_DBOX_OFFSET      0x00000000
#define HOST_SBOX_OFFSET      0x00010000

#define XEON_PHI_MMIO_TO_SBOX(phi) \
    ((mackerel_addr_t)((lvaddr_t)(phi->mmio.vbase)+HOST_SBOX_OFFSET))
#define XEON_PHI_MMIO_TO_DBOX(phi) \
    ((mackerel_addr_t)((lvaddr_t)(phi->mmio.vbase)+HOST_DBOX_OFFSET))

#define XEON_PHI_NUM_CARDS 8

#define XBOOT_DEBUG(x...) debug_printf(" BOOT | " x)
//#define XBOOT_DEBUG(x...)

#define MAX(a, b)   ( ((a) > (b)) ? (a) : (b) )
#define MIN(a, b)   ( ((a) < (b)) ? (a) : (b) )

typedef enum xeon_phi_state {
    XEON_PHI_STATE_NULL,
    XEON_PHI_STATE_PCI_OK,
    XEON_PHI_STATE_RESET,
    XEON_PHI_STATE_READY,
    XEON_PHI_STATE_BOOTING,
    XEON_PHI_STATE_ONLINE
} xeon_phi_state_t;

/*
 * TODO: Verify these values if they are really needed
 */
#define MEMORY_RESERVE_PERCENT 50
#define UOS_RESERVE_SIZE_MIN    ((128) * 1024 * 1024)
#define UOS_RESERVE_SIZE_MAX    (((4) * 1024 * 1024 * 1024ULL) - ((4) * 1024))

struct mbar {
    lvaddr_t vbase;
    lpaddr_t pbase;
    size_t   length;
};

struct xeon_phi
{
    xeon_phi_state_t state;
    struct mbar mmio;
    struct mbar apt;

    uint8_t pci_init;
    uint8_t id;             ///< card id for identifying the card

    uint32_t apicid;        ///< APIC id used for sending the boot interrupt

    char *cmdline;          ///< pointer to the bootloader cmdline

    struct smpt_info       *smpt;


    struct irq_info         *irq;
};

/**
 * \brief initializes the serial receive thread from the xeon phi
 *
 * \param phi   pointer to the card information
 */
errval_t serial_start_recv_thread(struct xeon_phi *phi);

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
