/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ETHERSRV_DEBUG_H_
#define ETHERSRV_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

void ethersrv_debug_printf(const char *fmt, ...);

//#define ETHERSRV_DEBUG(x...) printf("ETHERSRV: " x)

#if defined(ETHERSRV_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define ETHERSRV_DEBUG(x...) printf("ETHERSRV: " x)
#else
#define ETHERSRV_DEBUG(x...) ((void)0)
#endif                          // defined(ETHERSRV_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)

#endif                          // ETHERSRV_DEBUG_H_
