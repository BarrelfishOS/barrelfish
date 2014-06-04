/*
 * Copyright (c) 2014 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetsstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef XEON_PHI_MANAGER_SERVICE_H_
#define XEON_PHI_MANAGER_SERVICE_H_


#define XEON_PHI_TEST_HOST_NAME "xp_msg_host"
#define XEON_PHI_TEST_CARD_NAME "xp_msg_card"

#define DEBUG_SVC(x...) debug_printf("SVC | " x);
//#define DEBUG_SVC(x...)

/**
 * \brief   starts Xeon Phi manager service
 *
 * \return  SYS_ERR_OK on succes
 */
errval_t service_start(char *iface);




#endif /* SERVICE_H_ */
