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

#define HOST_DBOX_OFFSET      0x00000000
#define HOST_SBOX_OFFSET      0x00010000

#define XEON_PHI_MMIO_TO_SBOX(x) \
    ((mackerel_addr_t)((lvaddr_t)(x)+HOST_SBOX_OFFSET))

#define XEON_PHI_NUM_CARDS 8

#define XBOOT_DEBUG(x...) debug_printf(" BOOT | " x)
//#define XBOOT_DEBUG(x...)

struct xeon_phi
{
    lvaddr_t mmio_base;     ///< base address of the MMIO register space
    lvaddr_t mmio_lengh;    ///< length of the MMIO register space
    lvaddr_t aper_base;     ///< base address of the cards GDDR
    lvaddr_t aper_length;   ///< length of the cards GDDR

    uint8_t id;             ///< card id for identifying the card

    uint32_t apicid;        ///< APIC id used for sending the boot interrupt

    char *cmdline;          ///< pointer to the bootloader cmdline
};

/**
 * \brief initializes the serial receive thread from the xeon phi
 *
 * \param phi   pointer to the card information
 */
errval_t
serial_start_recv_thread(struct xeon_phi *phi);

/**
 * \brief boots the card with the given loader and multiboot image
 *
 * \param phi           pointer to the card information
 * \param xloader_img   pointer to the card bootloader image
 * \param multiboot_img pointer to the card multiboot image
 */
errval_t
xeon_phi_boot(struct xeon_phi *phi,
              char *xloader_img,
              char *multiboot_img);

/**
 * \brief performs a soft reset of the card
 *
 * \param phi   pointer to the card information
 */
errval_t
xeon_phi_reset(struct xeon_phi *phi);

#endif /* XEON_PHI_H_ */
