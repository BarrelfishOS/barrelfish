/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E1000_DEBUG_H_
#define E1000_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

#if defined(E1000N_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define E1000N_DEBUG(x...) printf("e1000n: " x)
#else
#define E1000N_DEBUG(x...) ((void)0)
#endif

#endif // E1000_DEBUG_H_
