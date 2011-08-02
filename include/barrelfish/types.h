/**
 * \file
 * \brief Definitions of standard Barrelfish userland types.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_TYPES_H
#define BARRELFISH_TYPES_H

#include <barrelfish_kpi/types.h>

/// Cycle count type
#ifdef __i386__
typedef uint64_t cycles_t; // rdtsc() is 64-bit on i386
#define PRIuCYCLES PRIu64
#define PRIxCYCLES PRIx64
#define PRIXCYCLES PRIX64
#else
typedef size_t cycles_t;
#define PRIuCYCLES "zu"
#define PRIxCYCLES "zx"
#define PRIXCYCLES "zX"
#endif

typedef uint32_t iref_t;

#define NULL_IREF 0

#define PRIxIREF PRIx32
#define PRIuIREF PRIu32

#define BYTES_IN_IREF (sizeof(iref_t) / sizeof(uint8_t))

/// Relative delay time (in microseconds)
typedef uint64_t delayus_t;
#define PRIuDELAYUS PRIu64
#define PRIxDELAYUS PRIx64
#define PRIXDELAYUS PRIX64

/// PCI addresses
typedef uint64_t pciaddr_t;
#define PRIxPCIADDR PRIx64
#define PRIuPCIADDR PRIu64
/// PCI size
typedef uint64_t pcisize_t;
#define PRIxPCISIZE PRIx64
#define PRIuPCISIZE PRIu64

#endif // TYPES_H
