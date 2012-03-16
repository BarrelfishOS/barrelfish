/**
 * \file
 * \brief dist2 Debug Macros
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIST2_DEBUG_H_
#define DIST2_DEBUG_H_

//#define DIST_SERVICE_DEBUG 1

#if defined(DIST_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define DIST2_DEBUG(x...) debug_printf("octopus_service: " x)
#else
#define DIST2_DEBUG(x...) ((void)0)
#endif

#endif // DIST2_DEBUG_H_
