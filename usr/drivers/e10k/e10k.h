/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E10K_H_
#define E10K_H_

#include <barrelfish/barrelfish.h>

#include "e10k_dev.h"

#include "e10k_queue.h"
#include "sleep.h"

#define E10K_PCI_DEVID 0x10FB

// Helper functions
void debug_dumpmem(void* buf, size_t len);
void* alloc_map_frame(vregion_flags_t attr, size_t size, struct capref *retcap);


#endif // ndef E10K_H_
