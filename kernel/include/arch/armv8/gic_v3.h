/*
 * Copyright (c) 2016, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __GIC_V3_H__
#define __GIC_V3_H__

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

/*
 * generic interrupt controller functionality
 */
void     gicv3_init(void);

#endif // __GIC_V3_H__
