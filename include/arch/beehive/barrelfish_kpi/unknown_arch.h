/**
 * \file
 * \brief Not sure where to put these definitions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BEEHIVE_BARRELFISH_KPI_UNKNOWN_H
#define ARCH_BEEHIVE_BARRELFISH_KPI_UNKNOWN_H

// required for lib/lwip/src/barrelfish/idc_barrelfish.c

// We have a totally ordered memory system, but need to ensure that
// compiler optimised does not reorder across it.
#define mfence() __asm volatile("" : /*outs*/ : /*ins*/ : "memory")


// Provided by assembler
extern void bee_dcache_flush_rgn(void * a,size_t n);

#define cache_flush_range(_b, _l) bee_dcache_flush_rgn((_b), (_l))

#endif // ARCH_BEEHIVE_BARRELFISH_KPI_UNKNOWN_H
