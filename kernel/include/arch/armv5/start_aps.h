/**
 * \file
 * \brief Definitions for the startup of application processors.
 *
 *  This file contains the prototypes for the functions which start
 *  the application processors
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef START_APS_H_
#define START_APS_H_

#pragma message "ARMv5 does not support multicore yet."

int start_aps_arm_start(uint8_t core_id, lvaddr_t entry);

#endif // START_APS_H_
