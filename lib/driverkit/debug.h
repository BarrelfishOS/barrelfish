/*
 * Copyright (c) 2017, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __DRIVERKIT_DEBUG__
#define __DRIVERKIT_DEBUG__

//#define ENABLE_DRIVERKIT_DEBUG 1

#if defined(ENABLE_DRIVERKIT_DEBUG) || defined(GLOBAL_DEBUG)
#define DRIVERKIT_DEBUG(x...) debug_printf("[dkit] " x)
#else
#define DRIVERKIT_DEBUG(x...) ((void)0)
#endif

#endif // __DRIVERKIT_DEBUG__
