/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_MANAGER_CARDS_H_
#define XEON_PHI_MANAGER_CARDS_H_


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
                         uint8_t *id);


/**
 * \brief  returns an array of irefs with the assciated xeon phi drivers
 *
 * \param  iref the iref array
 * \param  num  the number of irefs in the iref array
 *
 * \return SYS_ERR_OK
 */
errval_t cm_get_irefs(iref_t *iref, uint8_t *num);

#endif /* XEON_PHI_MANAGER_CARDS_H_ */
