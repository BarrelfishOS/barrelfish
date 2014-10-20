/**
 * \file
 * \brief Driver for booting the Xeon Phi Coprocessor card on a Barrelfish Host
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef XEON_PHI_SMPT_H
#define XEON_PHI_SMPT_H

#include <dev/xeon_phi/xeon_phi_smpt_dev.h>


struct smpt_info {
    xeon_phi_smpt_t         smpt_register;
    xeon_phi_smpt_entry_t   entries[xeon_phi_smpt_system_page_num];
    lpaddr_t                offsets[xeon_phi_smpt_system_page_num];
    uint8_t                 smpt_enabled;
};

/**
 * \brief Sets an entry in the system memory pagetable to a give address
 *
 * \param phi      reference to the card structure
 * \param slot     pagetable entry to set
 * \param address  host address to set
 * \param snooping enable snooping = 1, disable snooping = 0
 *
 * \return SYS_ERR_OK on success
 */
void smpt_set_address(struct xeon_phi *phi,
                      uint8_t slot,
                      lpaddr_t address,
                      uint8_t snooping);

/**
 * \brief Sets an entry in the system memory pagetable
 *
 * \param phi   reference to the card structure
 * \param entry the entry to set
 * \param e     the information about the new entry to set
 */
static inline void smpt_set_entry(struct xeon_phi *phi,
                                  uint8_t slot,
                                  xeon_phi_smpt_entry_t e)
{
    assert(slot < xeon_phi_smpt_system_page_num);

    xeon_phi_smpt_entry_wr(&phi->smpt->smpt_register, slot, e);
    phi->smpt->entries[slot] = e;
}

/**
 * \brief clears an entry in the system memory page table
 *
 * \param phi   reference to the card
 * \param entry entry number to clear
 *
 * \return SYS_ERR_OK on success
 */
static inline void smpt_clear_entry(struct xeon_phi *phi, uint8_t slot)
{
    assert(slot < xeon_phi_smpt_system_page_num);

    xeon_phi_smpt_entry_wr(&phi->smpt->smpt_register, slot, 0x0);
    phi->smpt->entries[slot] = 0x0;
}

/**
 * \brief Resets the system memory page tables
 */
void smpt_reset(struct xeon_phi *phi);

/**
 * \brief initializes the system memory page tables with a
 *        1:1 mapping
 *
 * \return SYS_ERR_OK on success
 */
errval_t smpt_init(struct xeon_phi *phi);

/**
 * \brief sets the entry of the SMPT for the Xeon Phi with given id
 *
 * \param phi   the local Xeon Phi
 * \param id    ID of the other Xeon Phi
 * \param addr  the physical (host)address
 *
 * \returns 1 on SUCCESS
 *          0 on attempt to set the own SMPT entry
 */
uint8_t smpt_set_coprocessor_address(struct xeon_phi *phi,
                                     uint8_t id,
                                     lpaddr_t addr);

/**
 * \brief calculates the ID of the Xeon Phi based on the physical address
 *
 * \param phi  the local Xeon Phi
 * \param addr physical address to lookup
 *
 * \returns the ID of the Xeon Phi this memory address belogngs to
 */
uint8_t smtp_get_xeon_phi_id_from_addr(struct xeon_phi *phi,
                                       lpaddr_t addr);

/**
 * \brief calculates the base address of the Xeon Phi GDDR
 *
 *        This function will return 0 if the ID is the local card.
 *
 * \param phi   the local xeon phi
 * \param id    the xeon phi id of the other card
 *
 * \returns base address of GDDR (0 if local)
 */
lpaddr_t smpt_get_coprocessor_address(struct xeon_phi *phi,
                                      uint8_t id);


/**
 * \brief sets the offset into the system memory page where the card is mapped
 *
 * \param phi    the local xeon phi
 * \param id     ID of the card
 * \param offset the offest into the page
 */
void smpt_set_coprocessor_offset(struct xeon_phi *phi,
                                 uint8_t id,
                                 lpaddr_t offset);
#endif /* XEON_PHI_SMPT_H */
