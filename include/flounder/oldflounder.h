/*
 * Interface Definition: Flounder base definitions
 * 
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 * 
 * This file is distributed under the terms in the attached LICENSE
 * file. If you do not find this file, copies can be found by
 * writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich.
 *  Attn: Systems Group.
 * 
 */

#ifndef __OLDFLOUNDER_H
#define __OLDFLOUNDER_H

#include <barrelfish/barrelfish.h>

typedef struct capref cap_t; 
typedef char *string_t;

typedef int (*connect_callback_t)(void *service, void *service_response);

#endif // __OLDFLOUNDER_H
