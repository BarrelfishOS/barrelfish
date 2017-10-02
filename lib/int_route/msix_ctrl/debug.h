/*
 * Copyright (c) 2007, 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MSIX_CTRL_DEBUG_H_
#define MSIX_CTRL_DEBUG_H_


/*****************************************************************
 * Debug printer and its power-switch:
 *****************************************************************/

#define MSIX_CTRL_DEBUG 1

#if defined(MSIX_CTRL_DEBUG) || defined(GLOBAL_DEBUG)
#define CTRL_DEBUG(x...) printf("msix_ctrl: " x)
#else
#define CTRL_DEBUG(x...) ((void)0)
#endif

#endif // MSIX_CTRL_DEBUG_H_
