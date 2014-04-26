/**
 * \file
 * \brief Module to set the System Memory Page Tables of the Xeon Phi
 */

/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <dev/xeon_phi/xeon_phi_smpt_dev.h>

#include "xeon_phi.h"
#include "smpt.h"

/*
 * XXX: Setting a system memory pagetable entry basically enables the coprocessor
 *      to access host memory directly. This is an open issue and should be
 *      restricted by capabilities somehow.
 *
 *      Anyhow, since the access to this memory range is controlled as normal
 *      memory, we can trust the kernels on the card not to introduce potential
 *      vulnerabilities...
 *
 *      Note: the system memory pagetables are rather coarse grained (16GB)
 */

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
                      uint8_t snooping)
{
    uint32_t e_address = (uint32_t)(address >> xeon_phi_smpt_system_page_shift);
    xeon_phi_smpt_entry_t e = xeon_phi_smpt_entry_default;
    e=xeon_phi_smpt_entry_host_address_insert(e, e_address);
    if (snooping) {
        snooping = xeon_phi_smpt_snooping_enabled;
    } else {
        snooping = xeon_phi_smpt_snooping_disabled;
    }
    e=xeon_phi_smpt_entry_snoop_disabled_insert(e, snooping);

    smpt_set_entry(phi, slot, e);
}

/**
 * \brief Resets the system memory page tables
 */
void smpt_reset(struct xeon_phi *phi)
{
    for (uint32_t i = 0; i < xeon_phi_smpt_system_page_num; ++i) {
        smpt_clear_entry(phi, i);
    }
}

/**
 * \brief initializes the system memory page tables with a
 *        1:1 mapping
 *
 * \return SYS_ERR_OK on success
 */
errval_t smpt_init(struct xeon_phi *phi)
{
    if (phi->smpt->smpt_enabled) {
        debug_printf("WARNING: SMPT already setup");
        return SYS_ERR_OK;
    }
    phi->smpt = malloc(sizeof(struct smpt_info));
    if (!phi->smpt) {
        return LIB_ERR_MALLOC_FAIL;
    }

    xeon_phi_smpt_initialize(&phi->smpt->smpt_register,
                             XEON_PHI_MMIO_TO_SBOX(phi));

    lpaddr_t host_address = 0;

    for (uint32_t i = 0; i < xeon_phi_smpt_system_page_num; ++i) {
        smpt_set_address(phi, i, host_address, 1);
        host_address += xeon_phi_smpt_system_page_size;
    }

    phi->smpt->smpt_enabled = 1;

    return SYS_ERR_OK;
}
