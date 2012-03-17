/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E1K_DEV_MNG_DEBUG_H_
#define E1K_DEV_MNG_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

#define E1K_DEV_MNG_SERVICE_DEBUG 1

#if defined(E1K_DEV_MNG_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define E1KDM_DEBUG(x...) printf("1KDM: " x)
#else
#define E1KDM_DEBUG(x...) ((void)0)
#endif // defined(E1K_DEV_MNG_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)

#endif // E1K_DEV_MNG_DEBUG_H_


