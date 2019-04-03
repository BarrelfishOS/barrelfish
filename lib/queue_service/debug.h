/*
 * Copyright (c) 2018, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstrasse 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __QUEUE_SERVICE_DEBUG__
#define __QUEUE_SERVICE_DEBUG__

//#define ENABLE_QUEUE_SERVICE_DEBUG 1

#if defined(ENABLE_QUEUE_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define QUEUE_SERVICE_DEBUG(x...) debug_printf("[queue_service] " x)
#else
#define QUEUE_SERVICE_DEBUG(x...) ((void)0)
#endif

#endif // __QUEUE_SERVICE_DEBUG__
