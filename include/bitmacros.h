/*
 * Macros for bit manipulation: masks, etc.
 *
 * Copyright (c) 2015, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __BITMACROS_H
#define __BITMACROS_H

/* A one-bit mask at bit n */
#define BIT(n) (1ULL << (n))

/* An n-bit mask, beginning at bit 0 */
#define MASK(n) (BIT(n) - 1)

/* An n-bit field selector, beginning at bit m */
#define FIELD(m,n,x) (((x) >> m) & MASK(n))

/* Round n up to the next multiple of size */
#define ROUND_UP(n, size) ((((n) + (size) - 1)) & (~((size) - 1)))

#endif /* __BITMACROS_H */
