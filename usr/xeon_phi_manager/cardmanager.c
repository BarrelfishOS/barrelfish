/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <xeon_phi/xeon_phi.h>

#include <if/xeon_phi_manager_defs.h>

#include "cardmanager.h"

struct xeon_phi_card {
    struct xeon_phi_manager_binding *binding;
    iref_t driver_iref;
};


static uint8_t num_cards = 0;

static iref_t  driver_irefs[XEON_PHI_NUM_MAX];

static struct xeon_phi_card xeon_phis[XEON_PHI_NUM_MAX];

/**
 * \brief   Inserts the information about the new Xeon Phi into the
 *          card manager
 *
 * \param   binding the flounder binding for this card
 * \param   driver  the iref to the driver
 * \param   id      returns the id of the newly inserted card
 */
errval_t cm_new_xeon_phi(struct xeon_phi_manager_binding *binding,
                         iref_t driver,
                         uint8_t *id)
{
    if (num_cards == XEON_PHI_NUM_MAX) {
        // TODO: ERROR CODE
        return -1;
    }

    struct xeon_phi_card *card = xeon_phis+num_cards;

    card->driver_iref = driver;
    card->binding = binding;
    binding->st = card;

    driver_irefs[num_cards] = driver;

    if (id) {
        *id = num_cards;
    }

    ++num_cards;

    return SYS_ERR_OK;
}


/**
 * \brief  returns an array of irefs with the assciated xeon phi drivers
 *
 * \param  iref the iref array
 * \param  num  the number of irefs in the iref array
 *
 * \return SYS_ERR_OK
 */
errval_t cm_get_irefs(iref_t **iref, uint8_t *num)
{
    if (iref) {
        *iref = driver_irefs;
    }
    if (num) {
        *num = num_cards;
    }
    return SYS_ERR_OK;
}



