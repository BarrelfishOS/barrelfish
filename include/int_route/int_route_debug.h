/**
 * \file
 * \brief Interrupt routing debug print
 */

/*
 * Copyright (c) 2016, 2019, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstrasse 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef INT_ROUTE_INT_ROUTE_DEBUG_H_
#define INT_ROUTE_INT_ROUTE_DEBUG_H_

//#define INT_ROUTE_SERVICE_DEBUG

#if defined(INT_ROUTE_SERVICE_DEBUG)
#define INT_DEBUG(x...) debug_printf("int_service: " x)
#else
#define INT_DEBUG(x...) ((void)0)
#endif


#endif /* INCLUDE_INT_ROUTE_INT_ROUTE_DEBUG_H_ */
