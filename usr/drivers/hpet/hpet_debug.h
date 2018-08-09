/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef HPET_DEBUG_H_
#define HPET_DEBUG_H_


/*****************************************************************
 * Debug printer and its power-switch:
 *****************************************************************/

#define HPET_SERVICE_DEBUG 1

#if defined(HPET_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define HPET_DEBUG(x...) printf("hpet_driver: " x)
#else
#define HPET_DEBUG(x...) ((void)0)
#endif


#endif // HPET_DEBUG_H_
